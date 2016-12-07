-module(from).
-behaviour(gen_server).

-export([start/0, init/1, add/1, handle_call/3]).

start() ->
  gen_server:start_link({local, from}, from, 0, []).

add(Value) ->
  gen_server:call(from, {add, Value}).

init(Value) ->
  {ok, Value}.

handle_call({add, Value}, From, Current) ->
  gen_server:reply(From, ok),
  timer:sleep(1000),
  Next = Current + Value,
  io:format("Current: ~p, Next: ~p~n", [Current, Next]),
  {noreply, Next}.
