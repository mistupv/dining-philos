-module(dining).
-export([main/0, main/1]).

-include("dining.hrl").


main() ->
  main(?FILE_NAME).

main(FileName) ->
  Self = self(),
  {ok, Log} = file:open(FileName, [write]),
  erlang:group_leader(Log, Self),
  WaiterPid = spawn(waiter, start, [?NUM_FORKS, Self]),
  receive
    table_prepared -> ok
  end,
  PhiloPids = [spawn(philo, start, [WaiterPid]) || _ <- lists:seq(1, ?NUM_FORKS)],
  timer:sleep(?RUNNING_TIME),
  WaiterPid ! close,
  wait(PhiloPids).

wait(Pids) ->
  timer:sleep(1000),
  AllClosed = lists:all(fun (X) ->
              case X of
                undefined -> true;
                _ -> false
              end
            end, [process_info(Pid) || Pid <- Pids]),
  case AllClosed of
    true -> ok;
    false -> wait(Pids)
  end.
