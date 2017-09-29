-module(philo).
-export([start/1]).

-include("dining.hrl").

start(WaiterPid) ->
  WaiterPid ! {intro, self()},
  receive
    {seated, PhiloId} ->
      io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " has been seated~n")
  end,
  loop(WaiterPid, PhiloId, 0).

loop(WaiterPid, PhiloId, NumEaten) ->
  think(PhiloId),
  HasEaten = hungry(WaiterPid, PhiloId, NumEaten),
  loop(WaiterPid, PhiloId, NumEaten + HasEaten).

think(PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is thinking~n"),
  ThinkTime = rand:uniform(?THINK_FACTOR),
  timer:sleep(ThinkTime).

hungry(WaiterPid, PhiloId, NumEaten) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is hungry~n"),
  WaiterPid ! {hungry, self()},
  receive
    think ->
      0;
    eat   ->
      eat(PhiloId),
      WaiterPid ! {eaten, self()},
      1;
    close ->
      io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " leaves, has eaten ~p times~n", [NumEaten]),
      exit(normal)
  end.

eat(PhiloId) ->
  timer:sleep(?EATING_TIME),
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " has eaten~n").
