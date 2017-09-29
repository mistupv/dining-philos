-module(dining).
-export([main/0]).

-include("dining.hrl").

main() ->
  Self = self(),
  WaiterPid = spawn(waiter, start, [?NUM_FORKS, Self]),
  receive
    table_prepared -> ok
  end,
  [spawn(philo, start, [WaiterPid]) || _ <- lists:seq(1, ?NUM_FORKS)],
  timer:sleep(?RUNNING_TIME),
  WaiterPid ! close.
