-module(test_alias).
-behaviour(eon_type_alias).
-export([rewrite/0, rewrite/1]).

rewrite()       -> {test_prim, [max, 666]}.
rewrite(Params) -> {test_prim, Params}.
