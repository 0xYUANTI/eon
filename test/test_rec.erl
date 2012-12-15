-module(test_rec).
-behaviour(eon_type_rec).
-include_lib("stdlib2/include/prelude.hrl").
-export([name/0, parameters/0, decl/2]).

name() -> some_obj.

parameters() -> [foo].

decl(_Obj, Params) ->
    Min = ?unlift(eon:get(Params, foo)),
    [ foo, {test_prim, [min,Min, max,Min]}
    , bar, {test_list, [min,0, max,1]}
    ].
