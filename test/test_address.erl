-module(test_address).
-behaviour(eon_type_rec).
-export([name/0, parameters/0, decl/2]).

name()                  -> address.

parameters()            -> [].

decl(_Term, _Params)    ->
    [ { <<"street">>, {test_string, [{allow_empty, false}]}}
    , { <<"residents">>, test_strings}
    ].
