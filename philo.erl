-module(philo).
-export([start/1]).

-include("dining.hrl").

start(WaiterPid) ->
  WaiterPid ! {intro, self()},
  receive
    seated ->
      io:fwrite("SOMEONE has been seated~n")
  end,
  loop(WaiterPid).

loop(WaiterPid) ->
  think(),
  hungry(WaiterPid),
  loop(WaiterPid).

think() ->
  io:fwrite("starts thinking~n"),
  ThinkTime = rand:uniform(?THINK_FACTOR),
  timer:sleep(ThinkTime),
  io:fwrite("stops thinking~n").

hungry(WaiterPid) ->
  io:fwrite("SOMEONE is hungry~n"),
  WaiterPid ! {hungry, self()},
  receive
    think ->
      ok;
    eat   ->
      eat()
  end.

eat() ->
  timer:sleep(?EATING_TIME).