-module(dining).
-export([main/0]).

-include("dining.hrl").

main() ->
  Self = self(),
  {ok, Log} = file:open(?FILE_NAME, [write]),
  erlang:group_leader(Log, Self),
  WaiterPid = spawn(waiter, start, [?NUM_FORKS, Self]),
  receive
    table_prepared -> ok
  end,
  [spawn(philo, start, [WaiterPid]) || _ <- lists:seq(1, ?NUM_FORKS)],
  timer:sleep(?RUNNING_TIME),
  WaiterPid ! close.
