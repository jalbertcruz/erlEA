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

-module(profiler).
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

start() ->
  Pid = spawn(profiler, init, []),
  register(profiler, Pid),
  Pid.

init() ->
  loop({none, none}, [], [], false).

loop({InitEvol, EndEvol}, Iterations, Emigrations, SolutionReached) ->

  receive

    {migration, {Ind, Fit}, T} ->
      if SolutionReached ->
        loop({InitEvol, EndEvol}, Iterations, Emigrations, SolutionReached);
        true ->
          loop({InitEvol, EndEvol}, Iterations, [{Ind, Fit, T} | Emigrations], SolutionReached)
      end;

    {iteration, Population} ->

      if SolutionReached ->
        loop({InitEvol, EndEvol}, Iterations, Emigrations, SolutionReached) ;

        true ->
          PopExtWFit = [evaluator:maxOnes(I) || I <- Population],
          Min = lists:min(PopExtWFit),
          Max = lists:max(PopExtWFit),
          Total = lists:foldl(fun(X, Sum) -> X + Sum end, 0, PopExtWFit),
          Ave = Total / length(Population),
          loop({InitEvol, EndEvol}, [{Min, Max, Ave} | Iterations], Emigrations, SolutionReached)
      end;

    {initEvol, T} ->
      loop({T, EndEvol}, Iterations, Emigrations, SolutionReached);

    {endEvol, T} ->
      if SolutionReached ->
        loop({InitEvol, EndEvol}, Iterations, Emigrations, true);
        true ->
          L = length(Iterations),
          io:format("Evolution delay: ~p seconds.~n", [getSecs(InitEvol, T)]),
          io:format("Number of migrations: ~p~n", [length(Emigrations)]),
          io:format("Number of iterations: ~p~n", [L]),
          loop({InitEvol, EndEvol}, Iterations, Emigrations, true)
      end;

    finalize ->
      ok

  end.

getSecs({_, S1, MicroS1}, {_, S2, MicroS2}) ->
  M = 1000000,
  N1 = S1 * M + MicroS1,
  N2 = S2 * M + MicroS2,
  R = N2 - N1,
  PE = R div M,
  PR = R rem M,
  PRL = integer_to_list(PR),
  L1 = length(PRL),
  FCad = if L1 == 6 -> ""; true -> string:substr("000000", L1 + 1) end,
  Res = integer_to_list(PE) ++ "." ++ FCad ++ PRL,
  list_to_float(Res).
