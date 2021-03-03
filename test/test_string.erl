-module(test_string).
-behaviour(eon_type_prim).
-export([name/0, parameters/0, normalize/2, validate/2, convert/2]).

name()       -> string.

parameters() -> [{allow_empty, true}].

normalize(Bin, _Params) -> Bin.

validate(<<>>, Params)                     -> eon:get_(Params, allow_empty);
validate(Bin, _Params) when is_binary(Bin) -> true.

convert(Bin, _Params) -> Bin.
