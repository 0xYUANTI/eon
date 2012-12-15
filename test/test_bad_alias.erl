-module(test_bad_alias).
-behaviour(eon_type_alias).
-export([rewrite/0, rewrite/1]).

rewrite()  -> 42.
rewrite(X) -> erlang:error(X).
