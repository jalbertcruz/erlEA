%% 
%% Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
%% Copyright 2011 by José Albert Cruz Almaguer.
%% 
%% This program is licensed to you under the terms of version 3 of the
%% GNU Affero General Public License. This program is distributed WITHOUT
%% ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
%% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
%% AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
%% 
-module(manager).

-include("../include/mtypes.hrl").

-compile(export_all).


start(Pmts, StartEvolution, ChangeParameters, GraphicalNode)->
    register(manager, spawn(manager, init, [Pmts, StartEvolution, ChangeParameters, GraphicalNode])).

init(Pmts, StartEvolution, ChangeParameters, GraphicalNode)->
    loop(Pmts, StartEvolution, ChangeParameters, GraphicalNode, []).

loop(Pmts, StartEvolution, ChangeParameters, GraphicalNode, Values) ->
    receive
	finalize ->
	    case length(Pmts) of
		0 -> {monitor, GraphicalNode} ! {list, [3000] ++ [ X || {X, _} <- Values ], [3000] ++ [ Y || {_, Y} <- Values ]},
		     self() ! fin,
		    ok;
		
		_ -> 
		    [{Next, Mark} | Rest] = Pmts,
		    ChangeParameters(Next, Mark),
		    StartEvolution(),
		    loop(Rest, StartEvolution, ChangeParameters, GraphicalNode, Values)
		end;

	{iterate, Id, Count} ->
	    loop(Pmts, StartEvolution, ChangeParameters, GraphicalNode, [ {Id, Count} | Values ]);

	fin ->
	    ok
    end.

