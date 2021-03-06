-module(frequency_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([stop/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  exit(whereis(?MODULE), shutdown).

init(_) ->
  ChildSpecList = [child_spec(freq_overload), child_spec(frequency)],
  {ok, {{rest_for_one, 2, 3600}, ChildSpecList}}.

child_spec(Module) ->
  {Module, {Module, start_link, []},
   permanent, 2000, worker, [Module]}.
