%%
%% Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
%% Copyright 2013 by José Albert Cruz Almaguer.
%%
%% This program is licensed to you under the terms of version 3 of the
%% GNU Affero General Public License. This program is distributed WITHOUT
%% ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
%% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
%% AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
%%

-module(reproducer).
-author("jalbertcruz@gmail.com").

-compile(export_all).

-include("../include/mtypes.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


evolve(Subpop, ParentsCount, DoWhenLittle) ->
  L = length(Subpop),
  if
    L < 3 ->
      DoWhenLittle(),
      {false, none};

    true ->
      Pop2r = selectPop2Reproduce(Subpop, ParentsCount),
      Parents = parentsSelector(Pop2r, ParentsCount),
      NInds = lists:map(fun crossover/1, Parents),
      NIndsFlatt = [I || {I, _} <- NInds] ++ [I || {_, I} <- NInds],
      NoParents = lists:subtract(Subpop, flatt(Parents)), % OJO, performance
      BestParents = [bestParent(Pop2r)],
      {true, {NoParents, NIndsFlatt, BestParents}}
  end.


flatt(Parents) ->
  [A || {A, _} <- Parents] ++ [A || {_, A} <- Parents].

%% 1. De los que tengan el fitness calculado tomará una subpoblación para reproducir
%      ( por decidir como lo hará )
%      - simple: first/next/next...

% Obtención de un conjunto de los mejores individuos.
%
% Table = atom, atomo identificador de la ets
% N = integer(),  cantidad de inds a seleccionar
% returns: [{Ind, IndFitness}]
extractSubpopulation(Sels, N) ->
  L = length(Sels),
  StartIndex =
    if
      L < N -> 1;
      true -> L - N + 1
    end,
% el ordenamiento es de menor a mayor, tomo entonces los ultimos
  Population = lists:sublist(lists:keysort(2, Sels), StartIndex, N),
%%   lists:foreach(fun(I) -> ets:delete(Table, I) end, Population),
  Population.

bestParent(Pop2r) ->
  T = lists:keysort(2, Pop2r),
  lists:last(T).

mergeFunction(Table, Subpop, NoParents, NInds, BestParents, PoolSize) ->
%%   ets:insert(Table, [{I, F, 2} || {I, F} <-
%%     lists:append(NoParents, BestParents)]),
%%   ets:insert(Table, [{I, none, 1} || I <- NInds]).

  L1 = [{Ind, {Fit, 2}} || {Ind, Fit} <-
    lists:append([Subpop, NoParents, BestParents])],
  L2 = [{Ind, {none, 1}} || Ind <- NInds],
  Sub1 = dict:from_list(lists:append(L1, L2)),
  Table1 = dict:filter(
    fun(Key, Value) ->
      not dict:is_key(Key, Sub1)
    end, Table),
  LCant2drop = dict:size(Table1) - (PoolSize - dict:size(Sub1)),
  Cant2drop = if
    LCant2drop >= 0 ->
      LCant2drop;
    true ->
      0
  end,

  KeysToEraseFromTable1 = lists:sublist(
    dict:fetch_keys(
      dict:filter(
        fun(Key, {Fit, State}) ->
          State == 2
        end, Table)
    ), Cant2drop),

  RestOlds = dict:filter(
    fun(Key, Value) ->
      not lists:member(Key, KeysToEraseFromTable1)
    end, Table1),

  LMore2drop = (dict:size(Sub1) + dict:size(RestOlds)) - PoolSize,

  More2drop = if
    LMore2drop >= 0 ->
      LMore2drop;
    true ->
      0
  end,

  Res = if
    More2drop > 0 ->
      KeysToEraseFromRestOlds = lists:sublist(
        dict:fetch_keys(
          dict:filter(
            fun(Key, {Fit, State}) ->
              State == 1
            end, Table)
        ), More2drop),
      dict:filter(
        fun(Key, Value) ->
          not lists:member(Key, KeysToEraseFromRestOlds)
        end, RestOlds);

    true ->
      RestOlds
  end,
  Result = dict:merge(fun(K, V1, V2) -> V2 end, Res, Sub1),
  [{Ind, Fit, State} || {Ind, {Fit, State}} <- dict:to_list(Result)].


% 2. Se seleccionarán 2n padres
%     - tomar aleatoriamente 3 inds, seleccionar el mejor
% Pop: [{Ind, IndFitness}]
% N = integer(),  cantidad de inds a seleccionar
selectPop2Reproduce(Pop, N) ->
  L = length(Pop),
  Select1from3 = fun() ->
    R3 = [lists:nth(random:uniform(L), Pop) || _ <- lists:seq(1, 3)],
    [First | Rest] = R3,
    lists:foldl(fun({I1, F1}, {I2, F2}) ->
      if
        F1 > F2 -> {I1, F1};
        true -> {I2, F2}
      end
    end, First, Rest)
  end,
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  [Select1from3() || _ <- lists:seq(1, 2 * N)].

% 3. De esos padres se seleccionarán n parejas
%     Aleatoriamente: first + random del resto, eliminadolo para la proxima iteracion
parentsSelector(Pop, 1) ->
  [I1 | Rest] = Pop,
  N1 = random:uniform(length(Rest)),
  [{I1, lists:nth(N1, Rest)}];

parentsSelector(Pop, N) ->
  [I1 | Rest] = Pop,
  N1 = random:uniform(length(Rest)),
  Head = {I1, lists:nth(N1, Rest)},
  {A, [_ | B]} = lists:split(N1 - 1, Rest),
  [Head | parentsSelector(lists:append(A, B), N - 1)].

% 4. Cruce y mutacion
%   - Posicion del cruce aleatoria
%   - Cada nuevo individuo selecciono el gen a mutar y lo muto
% Ind1 = Ind2 = [integer()]
crossover({{Ind1, _}, {Ind2, _}}) ->
  L = length(Ind1),
  CrossPoint = random:uniform(L),
  {A1, A2} = lists:split(CrossPoint, Ind1),
  {B1, B2} = lists:split(CrossPoint, Ind2),
  Child1 = lists:append(A1, B2),
  MuttationPoint = random:uniform(L),
  {M1, [Bit1 | M2]} = lists:split(MuttationPoint - 1, Child1),
  B3 = lists:append(M1, [problem:changeGen(Bit1) | M2]), % mutacion
  {B3, lists:append(B1, A2)}.

start() ->
  spawn(reproducer, init, []).

init() ->
  loop(none).

loop(D) ->
  receive

    {init, PManager, PProfiler} ->
      loop(#reproducer{manager = PManager, profiler = PProfiler});

    {evolve, Table, N} ->
      Sels = ets:select(Table,
        ets:fun2ms(fun({Ind, F, State}) when State == 2 -> {Ind, F} end)),
      Subpop = extractSubpopulation(Sels, N),

      {Res, ResultData} = evolve(
        Subpop,
        N div 2,
        fun() ->
          D#reproducer.manager ! {repEmpthyPool, self()}
        end
      ),
      if
        Res ->
          {NoParents, NInds, BestParents} = ResultData,
%%           {_, LDict} = process_info(D#reproducer.manager, dictionary),
%%           {_, PoolSize} = lists:keyfind(poolSize, 1, LDict),

          EtsAll = dict:from_list(ets:select(Table,
            ets:fun2ms(fun({X1, Y1, Z1}) -> {X1, {Y1, Z1}} end))),

          D#reproducer.manager ! {updatePool,
            mergeFunction(EtsAll, Subpop, NoParents, NInds, BestParents, problem:popSize())},

          D#reproducer.manager ! {evolveDone, self()},
%%PROFILER:
          D#reproducer.profiler ! {iteration, NInds};

        true ->
          ok
      end,
      loop(D);

    {emigrateBest, Table, Destination} ->
      Sels = ets:select(Table,
        ets:fun2ms(fun({Ind, F, State}) when State == 2 -> {Ind, F} end)),
      L = length(Sels),
      if
        L > 0 ->
          Population = lists:keysort(2, Sels),
          P = lists:last(Population), % el ordenamiento es de menor a mayor, tomo entonces el ultimo
          Destination ! {migration, P},
%%PROFILER:
          D#reproducer.profiler ! {migration, P, now()};

        true ->
          ok
      end,
      loop(D);

    finalize ->
      D#reproducer.manager ! {reproducerFinalized, self()},
%%       io:format("Reproducer ended: ~p, ", [self()]),
      ok

  end.

