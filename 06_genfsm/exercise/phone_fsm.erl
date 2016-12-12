-module(phone_fsm).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, terminate/3, handle_sync_event/4]).
-export([action/2, busy/1, reject/1, accept/1, hangup/1, inbound/1]).
-export([idle/2, calling/2, connected/2, receiving/2]).

start_link(Ms) ->
  gen_fsm:start_link(?MODULE, Ms, []).

init(Ms) ->
  process_flag(trap_exit, true),
  hlr:attach(Ms),
  {ok, idle, Ms}.

terminate(_Reason, idle, _Ms) ->
  hlr:detach();
terminate(_Reason, calling, {_Ms, CallingMsId}) ->
  phone_fsm:hangup(CallingMsId),
  hlr:detach();
terminate(_Reason, connected, {_Ms, OtherMsId, _Freq}) ->
  phone_fsm:hangup(OtherMsId),
  hlr:detach();
terminate(_Reason, receiving, {_Ms, FromMsId}) ->
  phone_fsm:reject(FromMsId),
  hlr:detach().

action({outbound, ToMs}, MsId) ->
  gen_fsm:sync_send_all_state_event(MsId, {outbound, ToMs});
action(Action, MsId) ->
  gen_fsm:send_event(MsId, {action, Action}).

busy(ToMsId)    -> gen_fsm:send_event(ToMsId, {busy,    self()}).
reject(ToMsId)  -> gen_fsm:send_event(ToMsId, {reject,  self()}).
accept(ToMsId)  -> gen_fsm:send_event(ToMsId, {accept,  self()}).
hangup(ToMsId)  -> gen_fsm:send_event(ToMsId, {hangup,  self()}).
inbound(ToMsId) -> gen_fsm:send_event(ToMsId, {inbound, self()}).

idle({inbound, FromMsId}, Ms) ->
  phone:reply(inbound, FromMsId, Ms),
  {next_state, receiving, {Ms, FromMsId}};
idle(Event, LoopData) ->
  io:format("~p in idle state, event ignored. LoopData:~w, Event:~w~n",
            [self(), LoopData, Event]),
  {next_state, idle, LoopData}.

calling({action, hangup}, {Ms, CallingMsId}) ->
  phone_fsm:hangup(CallingMsId),
  {next_state, idle, Ms};
calling({busy, Pid}, {Ms, Pid}) ->
  phone:reply(busy, Pid, Ms),
  {next_state, idle, Ms};
calling({reject, Pid}, {Ms, Pid}) ->
  phone:reply(rejected, Pid, Ms),
  {next_state, idle, Ms};
calling({accept, Pid}, {Ms, Pid}) ->
  case frequency:allocate() of
    {error, no_frequency} ->
      phone_fsm:reject(Pid),
      phone:reply(no_frequency, Pid, Ms),
      {next_state, idle, Ms};
    {ok, Freq} ->
      phone:reply(accepted, Pid, Ms),
      {next_state, receiving, {Ms, Pid, Freq}}
  end;
calling({inbound, Pid}, LoopData) ->
  phone_fsm:busy(Pid),
  {next_state, calling, LoopData};
calling(Event, LoopData) ->
  io:format("~p in calling state, event ignored. LoopData:~w, Event:~w~n",
            [self(), LoopData, Event]),
  {next_state, calling, LoopData}.

connected({inbound, FromMsId}, LoopData) ->
  phone_fsm:busy(FromMsId),
  {next_state, connected, LoopData};
connected({action, hangup}, {Ms, OtherMsId}) -> % we hangup the call
  phone_fsm:hangup(OtherMsId),
  {next_state, idle, Ms};
connected({action, hangup}, {Ms, OtherMsId, Freq}) -> % we hangup the call
  phone_fsm:hangup(OtherMsId),
  frequency:deallocate(Freq),
  {next_state, idle, Ms};
connected({hangup, OtherMsId}, {Ms, OtherMsId}) -> % they hangup the call
  phone:reply(hangup, OtherMsId, Ms),
  {next_state, idle, Ms};
connected({hangup, OtherMsId}, {Ms, OtherMsId, Freq}) -> % they hangup the call
  phone:reply(hangup, OtherMsId, Ms),
  frequency:deallocate(Freq),
  {next_state, idle, Ms};
connected(Event, LoopData) ->
  io:format("~p in connected state, event ignored. LoopData:~w, Event:~w~n",
            [self(), LoopData, Event]),
  {next_state, connected, LoopData}.

receiving({action, accept}, {Ms, FromMsId}) ->
  phone_fsm:accept(FromMsId),
  {next_state, connected, {Ms, FromMsId}};
receiving({action, reject}, {Ms, FromMsId}) ->
  phone_fsm:reject(FromMsId),
  {next_state, idle, Ms};
receiving({hangup, FromMsId}, {Ms, FromMsId}) ->
  phone:reply(hangup, FromMsId, Ms),
  {next_state, idle, Ms};
receiving({inbound, FromMsId}, LoopData) ->
  phone_fsm:busy(FromMsId),
  {next_state, receiving, LoopData};
receiving(Event, LoopData) ->
  io:format("~p in receiving state, event ignored. LoopData:~w, Event:~w~n",
            [self(), LoopData, Event]),
  {next_state, receiving, LoopData}.

handle_sync_event({outbound, ToMs}, _From, idle, Ms) ->
  case hlr:lookup_id(ToMs) of
    {error, invalid} ->
      io:format("ERROR, INVALID MSISDN~n"),
      phone:reply(invalid, ToMs, Ms),
      {reply, {error, invalid}, idle, Ms};
    {ok, ToMsId} when is_pid(ToMsId) ->
      phone:reply(outbound, ToMs, Ms),
      phone_fsm:inbound(ToMsId),
      {reply, ok, calling, {Ms, ToMsId}}
  end;
handle_sync_event({outbound, _ToMs}, _From, State, LoopData) ->
  {reply, {error, busy}, State, LoopData}.
