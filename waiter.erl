-module(waiter).
-export([start/1]).

-include("dining.hrl").

start(DiningPid) ->
  ref_start(),
  ref_add(?CUR_SEAT, 1),
  ForkDict = [{Id, free} || Id <- lists:seq(1, ?NUM_FORKS)],
  PhiloDict = [],
  DiningPid ! table_prepared,
  loop(ForkDict, PhiloDict).

loop(ForkDict, PhiloDict) ->
  check_close(PhiloDict),
  {UpdForkDict, UpdPhiloDict} =
    receive
      {intro, PhiloPid} ->
        {NewPhiloDict, PhiloId} = seat_philo(PhiloPid, PhiloDict),
        PhiloPid ! {seated, PhiloId},
        {ForkDict, NewPhiloDict};
      {hungry, PhiloPid} ->
        PhiloId = proplists:get_value(PhiloPid, PhiloDict),
        LeftForkId = PhiloId,
        RightForkId =  1 + (LeftForkId rem ?NUM_FORKS),
        LeftForkState = proplists:get_value(LeftForkId, ForkDict),
        RightForkState = proplists:get_value(RightForkId, ForkDict),
        case {LeftForkState, RightForkState} of
          {free, free} ->
            NewForkDict =
              pick_up_forks(PhiloPid, LeftForkId, RightForkId, ForkDict),
            {NewForkDict, PhiloDict};
          _ ->
            keep_thinking(PhiloPid),
            {ForkDict, PhiloDict}
        end;
      {eaten, PhiloPid} ->
        NewForkDict = put_down_forks(PhiloPid, ForkDict, PhiloDict),
        {NewForkDict, PhiloDict}
    end,
    loop(UpdForkDict, UpdPhiloDict).

seat_philo(PhiloPid, PhiloDict) ->
  CurSeat = ref_lookup(?CUR_SEAT),
  NewPhiloDict = lists:append(PhiloDict, [{PhiloPid, CurSeat}]),
  NextSeat = CurSeat + 1,
  ref_add(?CUR_SEAT, NextSeat),
  {NewPhiloDict, CurSeat}.

keep_thinking(PhiloPid) ->
  PhiloPid ! think.

pick_up_forks(PhiloPid, LeftForkId, RightForkId, ForkDict) ->
  PhiloPid ! eat,
  TmpForkDict = lists:keyreplace(LeftForkId, 1, ForkDict, {LeftForkId, used}),
  NewForkDict = lists:keyreplace(RightForkId, 1, TmpForkDict, {RightForkId, used}),
  NewForkDict.

put_down_forks(PhiloPid, ForkDict, PhiloDict) ->
  PhiloId = proplists:get_value(PhiloPid, PhiloDict),
  LeftForkId  = PhiloId,
  RightForkId = 1 + (LeftForkId rem ?NUM_FORKS), % Correct version
  % RightForkId = 1 + (?NUM_FORKS rem LeftForkId), % Bugged version
  TmpForkDict = lists:keyreplace(LeftForkId, 1, ForkDict, {LeftForkId, free}),
  NewForkDict = lists:keyreplace(RightForkId, 1, TmpForkDict, {RightForkId, free}),
  NewForkDict.

check_close(PhiloDict) ->
  receive
    close ->
      [Pid ! close || {Pid, _} <- PhiloDict],
      ref_stop(),
      exit(normal)
  after
    0 -> ok
  end.

ref_add(Id, Ref) ->
    ets:insert(?DIN_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?DIN_REF, Id, 2).

ref_start() ->
    ?DIN_REF = ets:new(?DIN_REF, [set, public, named_table]),
    ok.

ref_stop() ->
    ets:delete(?DIN_REF).