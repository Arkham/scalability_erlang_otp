-module(ex2).
-export([print_all/1]).

print_all([]) ->
  io:format("~n");
print_all([X|Xs]) ->
  io:format("~p\t", [X]),
  print_all(Xs).
