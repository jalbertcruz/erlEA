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

start(Table, PManager, Profiler) ->
  spawn(reproducer, init, [Table, PManager, Profiler]).

init(Table, PManager, Profiler) ->
  loop(Table, PManager, Profiler).

loop(Table, PManager, Profiler) ->
  receive

    {evolve, N} ->
      Pop = extractSubpopulation(Table, N),
      L = length(Pop),
      if
        L < 3 -> PManager ! {repEmpthyPool, self()};
        true ->
          ParentsCount = N div 2, % TODO: confirmar este numero
          Pop2r = selectPop2Reproduce(Pop, ParentsCount),
          Parents = parentsSelector(Pop2r, ParentsCount),
          NInds = lists:map(fun crossover/1, Parents),
          NIndsFlatt = [I || {I, _} <- NInds] ++ [I || {_, I} <- NInds],
          NoParents = lists:subtract(Pop, flatt(Parents)), % OJO, performance
          BestParents = [bestParent(Pop2r)],
          updatePool(Table, NoParents, NIndsFlatt, BestParents),
          PManager ! {evolveDone, self()},
%%PROFILER:
          Profiler ! {iteration, NIndsFlatt}
      end,
      loop(Table, PManager, Profiler);

    {emigrateBest, Destination} ->
      MS = ets:fun2ms(fun({Ind, F, State}) when State == 2 -> {Ind, F} end),
      Sels = ets:select(Table, MS),
      L = length(Sels),
      if
        L > 0 ->
          Population = lists:keysort(2, Sels),
          P = lists:last(Population), % el ordenamiento es de menor a mayor, tomo entonces el ultimo
%%PROFILER:
          Profiler ! {migration, P, now()},

          Destination ! {migration, P};
        true -> ok
      end,
      loop(Table, PManager, Profiler);

    finalize ->
      PManager ! {reproducerFinalized, self()},
%%       io:format("Reproducer ended: ~p, ", [self()]),
      ok

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
extractSubpopulation(Table, N) ->
  MS = ets:fun2ms(fun({Ind, F, State}) when State == 2 -> {Ind, F} end),
  Sels = ets:select(Table, MS),
  L = length(Sels),
  StartIndex =
    if
      L < N -> 1;
      true -> L - N + 1
    end,
% el ordenamiento es de menor a mayor, tomo entonces los ultimos
  Population = lists:sublist(lists:keysort(2, Sels), StartIndex, N),
  lists:foreach(fun(I) -> ets:delete(Table, I) end, Population),
  Population.

bestParent(Pop2r) ->
  T = lists:keysort(2, Pop2r),
  lists:last(T).

updatePool(Table, NoParents, NInds, BestParents) ->
  ets:insert(Table, [{I, F, 2} || {I, F} <- NoParents]),
  ets:insert(Table, [{I, F, 2} || {I, F} <- BestParents]),
  ets:insert(Table, [{I, none, 1} || I <- NInds]).

% 2. Se seleccionarán 2n padres
%     - tomar aleatoriamente 3 inds, seleccinar el mejor
% Pop: [{Ind, IndFitness}]
% N = integer(),  cantidad de inds a seleccionar
selectPop2Reproduce(Pop, N) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  [select1from3(Pop) || _ <- lists:seq(1, 2 * N)].

% Pop: [{Ind, IndFitness}]
select1from3(Pop) ->
  L = length(Pop),
  N1 = random:uniform(L),
  N2 = random:uniform(L),
  N3 = random:uniform(L),
  {I1, F1} = lists:nth(N1, Pop),
  {I2, F2} = lists:nth(N2, Pop),
  {I3, F3} = lists:nth(N3, Pop),
  if F1 > F2 ->
    if F1 > F3 -> R = {I1, F1}; % F1
      true -> R = {I3, F3}      % F3
    end;
    true -> if
      F2 > F3 -> R = {I2, F2}; % F2
      true -> R = {I3, F3}     % F3
    end
  end,
  R.

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
  MuttationP = random:uniform(L),
  {M1, [Bit1 | M2]} = lists:split(MuttationP - 1, Child1),
  B3 = lists:append(M1, [changeB(Bit1) | M2]), % mutacion
  {B3, lists:append(B1, A2)}.

changeB(B) ->
  if B == 1 -> 0;
    true -> 1
  end.

% 5. Envio al pool
%


