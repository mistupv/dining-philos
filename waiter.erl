-module(waiter).
-export([start/1]).

-include("dining.hrl").

start(DiningPid) ->
  ref_start(),
  ref_add(?CUR_SEAT, 1),
  Self = self(),
  ForkDict = [{Id, spawn(fork, start, [Self])} || Id <- lists:seq(1, ?NUM_FORKS)],
  %[receive fork_set -> ok end || lists:seq(1, ?NUM_FORKS)],
  PhiloDict = [],
  DiningPid ! table_prepared,
  loop(ForkDict, PhiloDict).

loop(ForkDict, PhiloDict) ->
  check_close(PhiloDict),
  UpdPhiloDict =
    receive
      {intro, PhiloPid} ->
        {NewPhiloDict, PhiloId} = seat_philo(PhiloPid, PhiloDict),
        PhiloPid ! {seated, PhiloId},
        NewPhiloDict;
      {hungry, PhiloPid} ->
        PhiloId = proplists:get_value(PhiloPid, PhiloDict),
        LeftForkId = PhiloId,
        RightForkId =  1 + (LeftForkId rem ?NUM_FORKS),
        LeftPid = proplists:get_value(LeftForkId, ForkDict),
        RightPid = proplists:get_value(RightForkId, ForkDict),
        LeftForkState = ask_state(LeftPid),
        RightForkState = ask_state(RightPid),
        case {LeftForkState, RightForkState} of
          {free, free} ->
            PhiloPid ! {eat, LeftPid, RightPid};
          _ ->
            PhiloPid ! think
        end,
        PhiloDict
    end,
    loop(ForkDict, UpdPhiloDict).

seat_philo(PhiloPid, PhiloDict) ->
  CurSeat = ref_lookup(?CUR_SEAT),
  NewPhiloDict = lists:append(PhiloDict, [{PhiloPid, CurSeat}]),
  NextSeat = CurSeat + 1,
  ref_add(?CUR_SEAT, NextSeat),
  {NewPhiloDict, CurSeat}.

% put_down_forks(PhiloPid, ForkDict, PhiloDict) ->
%   PhiloId = proplists:get_value(PhiloPid, PhiloDict),
%   LeftForkId  = PhiloId,
%   RightForkId = 1 + (LeftForkId rem ?NUM_FORKS), % Correct version
%   % RightForkId = 1 + (?NUM_FORKS rem LeftForkId), % Bugged version
%   TmpForkDict = lists:keyreplace(LeftForkId, 1, ForkDict, {LeftForkId, free}),
%   NewForkDict = lists:keyreplace(RightForkId, 1, TmpForkDict, {RightForkId, free}),
%   NewForkDict.

ask_state(Pid) ->
  Pid ! {get_state, self()},
  receive
    State -> State
  end.

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