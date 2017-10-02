-module(dining).
-export([main/0, main/1, main/2]).

-include("dining.hrl").

main() ->
  main(?DEFAULT_FILE_NAME).

main(FileName) ->
  main(FileName, ?DEFAULT_RUNNING_TIME).

main(FileName, RunningTime) ->
  Self = self(),
  {ok, Log} = file:open(FileName, [write]),
  erlang:group_leader(Log, Self),
  WaiterPid = spawn(waiter, start, [Self]),
  receive
    table_prepared -> ok
  end,
  PhiloPids = [spawn(philo, start, [WaiterPid]) || _ <- lists:seq(1, ?NUM_FORKS)],
  timer:sleep(RunningTime),
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
