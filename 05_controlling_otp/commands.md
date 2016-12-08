# Controlling OTP Behaviours

## Enable tracing

```
sys:trace(frequency, true).
```

## Print out calls 

```
sys:log(frequency, print).
```

## Get list of calls as tuples

```
sys:log(frequency, get).
```

## Install your own trace functions

```
F = fun
  (Count, {out, {error, no_frequency}, Pid, _LoopData}, ProcData) ->
    io:format(“Client ~p was refused! Count: ~w~n”, [Pid, Count]),
    Count + 1;
  (Count, _, _) ->
    Count
  end.

sys:install(frequency, {F, 1}).
```

# do stuff

```
sys:remove(frequency, F).
```

## Enable statistics

```
sys:statistics(frequency, true).
sys:statistics(frequency, get).
sys:statistics(frequency, false).
```

## Inspect process status

```
sys:get_status(frequency).
```

## Get and replace process state

```
{Free, Alloc} = sys:get_state(frequency).
sys:replace_state(frequency, fun(_) -> {[16,16], Alloc} end).
```

## Garbage collection info

Garbage collection runs.

```
> dbg:tracer().
> {ok, Pid} = frequency:start().
> dbg:p(Pid, [garbage_collection, timestamp]).
> frequency:allocate(), frequency:allocate(), frequency:allocate(),
  frequency:allocate(), frequency:allocate().
```

Garbage collection does not run.

```
> dbg:tracer().
> {ok, Pid} = gen_server:start_link({local, frequency}, frequency, [],
  [{spawn_opt, [{min_heap_size, 1024}]}]).
> dbg:p(Pid, [garbage_collection, timestamp]).
> frequency:allocate(), frequency:allocate(), frequency:allocate(),
  frequency:allocate(), frequency:allocate().
```

## Inspect garbage collection stats

```
process_info(Pid, garbage_collection).
```
