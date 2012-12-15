-module(test_prim).
-behaviour(eon_type_prim).
-include_lib("stdlib2/include/prelude.hrl").
-export([name/0, parameters/0, normalize/2, validate/2, convert/2]).

name() -> int_range.

parameters() -> [{min, 0}, max].

normalize(X, _Params) -> X.

validate(X, Params) ->
    is_integer(X) andalso
    X >= ?unlift(eon:get(Params, min)) andalso
    X =< ?unlift(eon:get(Params, max)).

convert(X, _Params) -> X.
