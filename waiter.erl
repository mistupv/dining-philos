-module(waiter).
-export([start/2]).

-include("dining.hrl").

start(NumForks, DiningPid) ->
  ref_start(),
  ref_add(?CUR_SEAT, 0),
  ForkDict = [{Id, free} || Id <- lists:seq(0, NumForks - 1)],
  PhiloDict = [],
  DiningPid ! table_prepared,
  loop(ForkDict, PhiloDict).

loop(ForkDict, PhiloDict) ->
  {UpdForkDict, UpdPhiloDict} =
    receive
      {intro, PhiloPid} ->
        NewPhiloDict = seat_philo(PhiloPid, PhiloDict),
        PhiloPid ! seated,
        io:fwrite("PhiloDict: ~p~n", [NewPhiloDict]),
        {ForkDict, NewPhiloDict};
      {hungry, PhiloPid} ->
        PhiloId = proplists:get_value(PhiloPid, PhiloDict),
        LeftForkId = PhiloId,
        RightForkId =  (LeftForkId + 1) rem ?NUM_FORKS,
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
        NewForkDict = put_down_forks(PhiloPid, ForkDict),
        {NewForkDict, PhiloDict}
    end,
    loop(UpdForkDict, UpdPhiloDict).

seat_philo(PhiloPid, PhiloDict) ->
  CurSeat = ref_lookup(?CUR_SEAT),
  NewPhiloDict = lists:append(PhiloDict, [{PhiloPid, CurSeat}]),
  NextSeat = CurSeat + 1,
  ref_add(?CUR_SEAT, NextSeat),
  NewPhiloDict.

keep_thinking(PhiloPid) ->
  PhiloPid ! think.

pick_up_forks(PhiloPid, LeftForkId, RightForkId, ForkDict) ->
  PhiloPid ! eat,
  TmpForkDict = lists:keyreplace(LeftForkId, 1, ForkDict, {LeftForkId, used}),
  NewForkDict = lists:keyreplace(RightForkId, 1, TmpForkDict, {RightForkId, used}),
  NewForkDict.

put_down_forks(PhiloPid, ForkDict) ->
  PhiloId = dict:fecth(PhiloPid),
  LeftForkId  = PhiloId,
  RightForkId = (LeftForkId + 1) rem ?NUM_FORKS,
  TmpForkDict = lists:keyreplace(LeftForkId, 1, ForkDict, {LeftForkId, free}),
  NewForkDict = lists:keyreplace(RightForkId, 1, TmpForkDict, {RightForkId, free}),
  NewForkDict.

ref_add(Id, Ref) ->
    ets:insert(?DIN_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?DIN_REF, Id, 2).

ref_start() ->
    ?DIN_REF = ets:new(?DIN_REF, [set, public, named_table]),
    ok.

% ref_stop() ->
%     ets:delete(?DIN_REF).