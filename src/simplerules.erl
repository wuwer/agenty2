-module(simplerules).

-export([father/3]).

father (Engine, {male, X}, {parent, X, Y}) ->
	seresye_engine:assert (Engine, {father, X, Y}).
