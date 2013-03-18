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
-module(model_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/mtypes.hrl").

-import(model, [genInitPop/2, genInd/1, parentsSelector/2, recombine/1, mutateInd/1, mutate/1, selectNewPopulation/2, selReplacement/3, dividePopulation/2, replaceIndividuals/3]).

-compile(export_all).

genInitPop_test_() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  N = random:uniform(10) * 10,
  Size = random:uniform(500) * 10,
  IP = genInitPop(Size, N),
  Inds = [genInd(random:uniform(10) + 5) || _ <- lists:seq(1, random:uniform(100) + 5)],
  [?_assert(lists:all(fun(X) -> N =:= length(X) end, IP)),
    ?_assert(lists:all(fun(I) -> lists:all(fun(C) -> (C =:= 1) or (C =:= 0) end, I) end, Inds))].


recombine_test() ->
  T = random:uniform(10) + 5,
  I1 = genInd(T),
  I2 = genInd(T),
  ?assertEqual(T, length(recombine({I1, I2}))).

mutateInd_test() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  N = random:uniform(10) * 10,
  SInd = genInd(N),
  SIndM = mutateInd(SInd),
  SInd1 = numberOf1(SInd),
  SIndM1 = numberOf1(SIndM),
  ?assert(abs(SInd1 - SIndM1) =< 1).

numberOf1(Ch) ->
  length(lists:filter(fun(E) -> (E =:= 1) end, Ch)).

allNumberOf1(L) ->
  [numberOf1(El) || El <- L].

mutate_test() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  N = random:uniform(10) * 10,
  Size = random:uniform(500) * 10,
  IP = genInitPop(Size, N),
  IPMutated = mutate(IP),
  IP1 = allNumberOf1(IP),
  IPMutated1 = allNumberOf1(IPMutated),
  IP1Mix = lists:zip(IP1, IPMutated1),
  ?assert(lists:all(fun({N1, N2}) -> (abs(N1 - N2) =< 1) end, IP1Mix)).

dp_test() ->
  L1 = lists:seq(1, 256),
  R1 = dividePopulation(L1, 8),
%Fl1 = lists:flatten(R1),
  [?_assertEqual(length(R1), 8)].


rInd_test() ->
  Pop = [a, b, c, d, e, f, d, g, h],
  NInd = [z, z, z],
  Indxs = [2, 5, 8],
  ResExped = [a, c, d, f, d, h, z, z, z],
  Res = replaceIndividuals(Pop, NInd, Indxs),
  ?assertEqual(ResExped, Res).
