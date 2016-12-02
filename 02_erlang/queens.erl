-module(queens).
-export([queens/1]).

queens(0) -> [[]];
queens(N) ->
  [[Val | Columns] ||
    Columns <- queens(N-1),
    Val <- [1,2,3,4,5,6,7,8] -- Columns,
    safe(Val, Columns, 1)].

safe(_Val, [], _N) -> true;
safe(Val, [Column|Columns], N) ->
  % io:format("~p ~p ~p~n", [Val, [Column|Columns], N]),
  (Val /= Column + N) andalso (Val /= Column - N) andalso
    safe(Val, Columns, (N+1)).
