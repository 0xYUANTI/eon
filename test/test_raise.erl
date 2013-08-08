-module(test_raise).
-behaviour(eon_type_prim).
-include_lib("stdlib2/include/prelude.hrl").
-export([name/0, parameters/0, normalize/2, validate/2, convert/2]).

name() -> test_raise.

parameters() -> [].

normalize(_X, _Params) -> eon_type:raise(myexn).

validate(_X, _Params) -> true.

convert(X, _Params) -> X.
