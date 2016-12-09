-module(coffee).
-export([tea/0, espresso/0, americano/0, cappuccino/0, pay/1, cup_remove/0, cancel/0]).
-export([start_link/0, init/0]).

start_link() ->
  {ok, spawn_link(?MODULE, init, [])}.

tea()        -> ?MODULE ! {selection, tea,        100}.
espresso()   -> ?MODULE ! {selection, espresso,   150}.
americano()  -> ?MODULE ! {selection, americano,  100}.
cappuccino() -> ?MODULE ! {selection, cappuccino, 150}.

cup_remove() -> ?MODULE ! cup_removed.
pay(Coin)    -> ?MODULE ! {pay, Coin}.
cancel()     -> ?MODULE ! cancel.

init() ->
  register(?MODULE, self()),
  hw:reboot(),
  selection().

%% State: selection
selection() ->
  hw:display("Make Your Selection", []),

  receive
    {selection, Type, Price} ->
      payment(Type, Price, 0);
    {pay, Coin} ->
      hw:return_change(Coin),
      selection();
    _Other ->
      selection()
  end.

%% State: payment
payment(Type, Price, Paid) ->
  hw:display("Please pay: ~w", [Price - Paid]),

  receive
    {pay, Coin} ->
      if
        Coin + Paid >= Price ->
          hw:display("Preparing Drink", []),
          hw:return_change(Coin + Paid - Price),
          hw:drop_cup(),
          hw:prepare(Type),
          remove();
        true ->
          payment(Type, Price, Coin + Paid)
      end;
    cancel ->
      hw:return_change(Paid),
      selection();
    _Other ->
      payment(Type, Price, Paid)
  end.

%% State: remove
remove() ->
  hw:display("Remove Drink", []),

  receive
    cup_removed ->
      selection();
    {pay, Coin} ->
      hw:return_change(Coin),
      remove();
    _Other ->
      remove()
  end.
