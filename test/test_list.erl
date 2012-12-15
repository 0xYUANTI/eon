-module(test_list).
-behaviour(eon_type_list).
-export([name/0, parameters/0, element_type/2]).

name() -> int_list.

parameters() -> [min, max].

element_type(_Lst, Params) -> {test_prim, Params}.
