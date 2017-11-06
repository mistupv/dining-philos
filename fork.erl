-module(fork).
-export([start/1]).

-include("dining.hrl").

start(WaiterPid) ->
  %WaiterPid ! fork_set,
  loop(free).

loop(State) ->
  receive
    {get_state, WaiterPid} ->
      WaiterPid ! State,
      loop(State);
    {set_state, NewState} ->
      loop(NewState)
  end. 