-module(frequency).
-behaviour(gen_server).

-export([start/0, init/1, allocate/0, deallocate/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% Public API

start() ->
  gen_server:start_link({local, frequency}, frequency, [], []).

allocate() ->
  gen_server:call(frequency, {allocate, self()}).

deallocate(Freq) ->
  gen_server:cast(frequency, {deallocate, Freq}).

stop() ->
  gen_server:cast(frequency, stop).

% Callbacks

init(_Args) ->
  Frequencies = {get_frequencies(), []},
  {ok, Frequencies}.

handle_call({allocate, Pid}, _From, Frequencies) ->
  {NewFrequencies, Reply} = allocate(Frequencies, Pid),
  {reply, Reply, NewFrequencies}.

handle_cast({deallocate, Freq}, Frequencies) ->
  NewFrequencies = deallocate(Frequencies, Freq),
  {noreply, NewFrequencies};
handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.

handle_info({'EXIT', _Pid, normal}, LoopData) ->
  {noreply, LoopData};
handle_info({'EXIT', Pid, Reason}, LoopData) ->
  io:format("Process: ~p exited with reason: ~p~n", [Pid, Reason]),
  {noreply, LoopData};
handle_info(_Msg, LoopData) ->
  {noreply, LoopData}.

terminate(_Reason, _LoopData) ->
  ok.

% Helpers

get_frequencies() -> [10,11,12,13,14,15].

allocate({[], _Allocated} = Frequencies, _Pid) ->
  {Frequencies, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free], NewAllocated}.
