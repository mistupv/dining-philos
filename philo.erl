-module(philo).
-export([start/1]).

-include("dining.hrl").

start(WaiterPid) ->
  WaiterPid ! {intro, self()},
  receive
    {seated, PhiloId} ->
      io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " has been seated~n")
  end,
  loop(WaiterPid, PhiloId).

loop(WaiterPid, PhiloId) ->
  think(PhiloId),
  hungry(WaiterPid, PhiloId),
  loop(WaiterPid, PhiloId).

think(PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is thinking~n"),
  ThinkTime = rand:uniform(?THINK_FACTOR),
  timer:sleep(ThinkTime).

hungry(WaiterPid, PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is hungry~n"),
  WaiterPid ! {hungry, self()},
  receive
    think ->
      ok;
    eat   ->
      eat(PhiloId),
      WaiterPid ! {eaten, self()}
  end.

eat(PhiloId) ->
  timer:sleep(?EATING_TIME),
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " has eaten~n").
