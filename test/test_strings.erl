-module(test_strings).
-behaviour(eon_type_list).
-export([name/0, parameters/0, element_type/2]).

name()                  -> strings.

parameters()            -> test_string:parameters().

element_type(_, Params) -> {test_string, Params}.
