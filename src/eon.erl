%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Dictionary API.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eon).
-compile({no_auto_import, [size/1]}).

%%%_* Exports ==========================================================
%% Constructors
-export([new/0]).
-export([new/1]).
-export([new/2]).

%% Basics
-export([del/2]).
-export([equal/2]).
-export([get/2]).
-export([get/3]).
-export([get_/2]).
-export([dget/2]).
-export([dget/3]).
-export([dget_/2]).
-export([is_empty/1]).
-export([is_key/2]).
-export([keys/1]).
-export([set/3]).
-export([size/1]).
-export([vals/1]).
-export([zip/2]).

%% Higher-order
-export([filter/2]).
-export([fold/3]).
-export([map/2]).

%% Sets
-export([difference/2]).
-export([intersection/2]).
-export([union/2]).

%% Iterators
-export([done/1]).
-export([make/1]).
-export([next/1]).

%% Types
-export_type([literal/2]).
-export_type([object/0]).
-export_type([object/2]).

%%%_* Includes =========================================================
-include("eon.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Macros ===========================================================
-define(assertObjEq(Obj1, Obj2), (true = equal(Obj1, Obj2))).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-type object()      :: orddict:orddict().
-type object(A, B)  :: orddict:orddict()
                     | [{A, B}]
                     | literal(A, B).

-type literal(A, B) :: [A | B].
-type deep_key()    :: binary() | list().

-type func(A)       :: fun((_, _) -> A)
                     | fun((_)    -> A).

%%%_ * Constructors ----------------------------------------------------
-spec new() -> object(_, _).
%% @doc new() is a fresh object.
new() -> orddict:new().

-spec new(object(A, B)) -> object(A, B).
%% @doc new(Obj) is the canonical representation of Obj.
new([X|_] = Xs) when is_tuple(X) -> Xs;
new(Xs) when is_list(Xs)         -> orddict:from_list(partition(Xs)).

partition(KVs) ->
  ?hence(0 =:= length(KVs) rem 2),
  [{K, V} || [K, V] <- s2_lists:partition(2, KVs)].

-spec new(object(A, B), func(boolean())) -> object(A, B).
%% @doc new(Obj, Pred) is the canonical representation of Obj if Pred
%% returns true for all entries.
new(X, Pred) ->
  orddict:map(
    fun(K, V) ->
      if is_function(Pred, 1) -> Pred(V);
         is_function(Pred, 2) -> Pred(K, V)
      end orelse throw({error, {validator, K, V}}),
      V
    end, new(X)).

new_test() ->
  _           = new(),

  _           = new(orddict:store( foo
                                 , 42
                                 , orddict:store(bar, 666, orddict:new())) ),
  _           = new([{foo, 42}, {bar, 666}]),
  _           = new([{foo, bar},baz]),
  _           = new([foo,42, bar,666]),
  {error, _}  = (catch new([foo,bar, baz])),
  {'EXIT', _} = (catch new(42)),

  _           = new([foo,bar], fun(K, V) -> K =:= foo andalso V =:= bar end),
  {error, _}  = (catch new([foo,bar], fun(V) -> V =:= baz end)).

%%%_ * Basics ----------------------------------------------------------
-spec del(object(A, B), A) -> object(A, B).
%% @doc del(Obj, Key) is Obj with the entry for Key removed.
del(Obj, Key) -> lists:keydelete(Key, 1, new(Obj)).

del_test() ->
  Nil = new(),
  Nil = del(Nil, foo),
  Nil = del(set(Nil, foo, bar), foo).


-spec equal(object(_, _), object(_, _)) -> boolean().
%% @doc equal(Obj1, Obj2) is true iff Obj1 matches Obj2.
equal(Obj, Obj)   -> true;
equal(Obj1, Obj2) -> lists:sort(new(Obj1)) =:= lists:sort(new(Obj2)).

equal_test() ->
  true  = equal(new(), new()),
  false = equal([foo,bar], [foo,baz]).


-spec get(object(A, B), A) -> maybe(B, notfound).
%% @doc get(Obj, Key) is the value associated with Key in Obj,
%% or an error if no such value exists.
get(Obj, Key) ->
  case lists:keyfind(Key, 1, new(Obj)) of
    {Key, Val} -> {ok, Val};
    false      -> {error, notfound}
  end.

-spec get(object(A, B), A, B) -> B.
%% @doc get(Obj, Key, Default) is the value associated with Key in Obj,
%% or Default if no such value exists.
get(Obj, Key, Default) ->
  case get(Obj, Key) of
    {ok, Res}         -> Res;
    {error, notfound} -> Default
  end.

-spec get_(object(A, B), A) -> B | no_return().
%% @doc get(Obj, Key) is the value associated with Key in Obj,
%% or an exception if no such value exists.
get_(Obj, Key) ->
  {ok, Res} = get(Obj, Key),
  Res.

get_test() ->
  {error, notfound} = get(new(), foo),
  bar               = get(new(), foo, bar),
  {ok, bar}         = get(set(new(), foo, bar), foo),
  bar               = get(set(new(), foo, bar), foo, baz),
  1                 = get_([foo, 1], foo),
  {error, {lifted_exn, {badmatch, {error, notfound}}, _}}
                    = ?lift(get_(new(), foo)).

-spec dget(object(deep_key(), B), deep_key()) -> maybe(B, notfound).
%% @doc dget(Obj, Key) is the value associated with the deep Key in Obj,
%% or an error if no such value exists. A deep key is a `.`-delimetered key.
dget(Obj, Key) ->
    lists:foldl(fun(_, {error, notfound}=E) -> E;
                   (K, {ok, Acc})           -> eon:get(Acc, K)
                end, {ok, Obj}, normalize_deep_key(Key)).

-spec dget(object(deep_key(), B), deep_key(), B) -> B.
%% @doc dget(Obj, Key) is the value associated with the deep Key in Obj,
%% or Default if no such value exists. A deep key is a `.`-delimetered key.
dget(Obj, Key, Default) ->
  case dget(Obj, Key) of
    {ok, Res}         -> Res;
    {error, notfound} -> Default
  end.

-spec dget_(object(deep_key(), B), deep_key()) -> B | no_return().
%% @doc dget(Obj, Key) is the value associated with the deep Key in Obj,
%% or an exception if no such value exists. A deep key is a `.`-delimetered key.
dget_(Obj, Key) ->
  {ok, Res} = dget(Obj, Key),
  Res.

dget_test() ->
  P = [ {<<"one">>,   1}
      , {<<"two">>,   [ {<<"two_one">>,   21}
                      , {<<"two_two">>,   [{<<"two_two">>, 22}]}
                      , {<<"two_three">>, [{ <<"two_three_one">>
                                           , [{<<"two_three_one">>, 231}]
                                           }]}
                      ]}
      , {"three",     3} ],
  {error, notfound} = dget(P, <<"non.existent">>),
  blarg             = dget(P, <<"non.existent">>, blarg),
  {ok, 1}           = dget(P, <<"one">>),
  {ok, 3}           = dget(P, "three"),
  true              = equal(get(P, <<"two">>), dget(P, <<"two">>)),
  {ok, 21}          = dget(P, <<"two.two_one">>),
  {ok, 22}          = dget(P, <<"two.two_two.two_two">>),
  {ok, 231}         = dget(P, <<"two.two_three.two_three_one.two_three_one">>),
  {error, {lifted_exn, {badmatch, {error, notfound}}, _}}
                    = ?lift(dget_(new(), "foo")).


-spec is_empty(object(_, _)) -> boolean().
%% @doc is_empty(Obj) is true iff Obj is empty.
is_empty(Obj) -> 0 =:= size(new(Obj)).

is_empty_test() ->
  true  = is_empty(new()),
  false = is_empty([foo,bar]).


-spec is_key(object(A, _), A) -> boolean().
%% @doc is_key(Obj, Key) is true iff there is a value associated with
%% Key in Obj.
is_key(Obj, Key) -> lists:keymember(Key, 1, new(Obj)).

is_key_test() ->
  false = is_key(new(), foo),
  true  = is_key(set(new(), foo, bar), foo).


-spec keys(object(A, _)) -> [A].
%% @doc keys(Obj) is a list of all keys in Obj.
keys(Obj) -> [K || {K, _} <- new(Obj)].

keys_test() ->
  [] = keys(new()).


-spec set(object(A, B), A, B) -> object(A, B).
%% @doc set(Obj, Key, Val) is an object which is identical to Obj
%% execept that it maps Key to Val.
set(Obj, Key, Val) -> lists:keystore(Key, 1, new(Obj), {Key, Val}).

set_test() ->
  ?assertObjEq(set(new(), foo, bar),
               set(new(), foo, bar)).


-spec size(object(_, _)) -> non_neg_integer().
%% @doc size(Obj) is the number of mappings in Obj.
size(Obj) -> orddict:size(new(Obj)).

size_test() ->
  0 = size(new()),
  1 = size([foo,bar]).


-spec vals(object(A, _)) -> [A].
%% @doc vals(Obj) is a list of all values in Obj.
vals(Obj) -> [V || {_, V} <- new(Obj)].

vals_test() ->
  Vs   = vals([foo,1, bar,2]),
  true = lists:member(1, Vs),
  true = lists:member(2, Vs).


-spec zip(object(A, B), object(A, C)) -> object(A, {B, C}).
%% @doc zip(Obj1, Obj2) is an object which maps keys from Obj1 to values
%% from both Obj1 and Obj2.
%% Obj1 and Obj2 must have the same set of keys.
zip(Obj1, Obj2) ->
  ?hence(lists:sort(keys(Obj1)) =:= lists:sort(keys(Obj2))),
  orddict:merge(fun(_K, V1, V2) -> {V1, V2} end, new(Obj1), new(Obj2)).

zip_test() ->
  ?assertObjEq([foo,{bar, baz}],
               zip([foo,bar], [foo,baz])).

%%%_ * Higher-order ----------------------------------------------------
-spec map(func(C), object(A, _)) -> object(A, C).
%% @doc map(F, Obj) is the result of mapping F over Obj's entries.
map(F, Obj) ->
  orddict:map(
    fun(K, V) ->
      if is_function(F, 1) -> F(V);
         is_function(F, 2) -> F(K, V)
      end
    end, new(Obj)).

map_test() ->
  ?assertObjEq([foo,1],
               map(fun(V) -> V+1 end, [foo,0])),
  ?assertObjEq([foo,1],
               map(fun(_K, V) -> V+1 end, [foo,0])).


-spec filter(func(boolean()), object(_, _)) -> object(_, _).
%% @doc filter(F, Obj) is the subset of entries in Obj for which Pred
%% returns true.
filter(Pred, Obj) ->
  orddict:filter(
    fun(K, V) ->
      if is_function(Pred, 1) -> Pred(V);
         is_function(Pred, 2) -> Pred(K, V)
      end
    end, new(Obj)).

filter_test() ->
  ?assertObjEq(new(),
               filter(fun(V) -> V =/= 42 end, [foo, 42])),
  ?assertObjEq(new(),
               filter(fun(K, _V) -> K =/= foo end, [foo, 42])).


-spec fold(fun(), A, object(_, _)) -> A.
%% @doc fold(F, Acc0, Obj) is Obj reduced to Acc0 via F.
fold(F, Acc0, Obj) ->
  lists:foldl(
    fun({K, V}, Acc) ->
      if is_function(F, 2) -> F(V, Acc);
         is_function(F, 3) -> F(K, V, Acc)
      end
    end, Acc0, new(Obj)).

fold_test() ->
  6  = fold(fun(V, Sum)    -> V+Sum end,   0, [1,2, 3,4]),
  10 = fold(fun(K, V, Sum) -> K+V+Sum end, 0, [1,2, 3,4]).

%%%_ * Sets ------------------------------------------------------------
-spec union(object(_, _), object(_, _)) -> object(_, _).
%% @doc union(Obj1, Obj2) is Obj1 plus any entries from Obj2 whose keys
%% do not occur in Obj1.
union(Obj1, Obj2) ->
  lists:ukeymerge(1, lists:ukeysort(1,new(Obj1)), lists:ukeysort(1,new(Obj2))).

union_test() ->
  ?assertObjEq([foo,1, bar,2, baz,3],
               union([foo,1, bar,2], [bar,3, baz,3])).


-spec difference(object(_, _), object(_, _)) -> object(_, _).
%% @doc difference(Obj1, Obj2) is Obj1 with all entries whose keys occur
%% in Obj2 removed.
difference(Obj1, Obj2) ->
  orddict:filter(fun(K, _V) -> not is_key(new(Obj2), K) end, new(Obj1)).

difference_test() ->
  ?assertObjEq(new(),
               difference([foo,1], [foo, 2])).


-spec intersection(object(_, _), object(_, _)) -> object(_, _).
%% @doc intersection(Obj1, Obj2) is Obj1 with all entries whose keys do
%% not occur in Obj2 removed.
intersection(Obj1, Obj2) ->
  orddict:filter(fun(K, _V) -> is_key(new(Obj2), K) end, new(Obj1)).

intersection_test() ->
  ?assertObjEq([bar,2],
               intersection([foo,1, bar,2], [bar,2, baz,2])).

%%%_ * Iterators -------------------------------------------------------
-type iterator()         :: [{_, _}].

-spec make(object(_, _)) -> iterator().
%% @doc make(Obj) is an iterator for Obj.
make(Obj)                -> new(Obj).

-spec next(iterator())   -> {_, iterator()}.
%% @doc next(It0) is the next entry in iterator It0 and the updated
%% iterator It.
next([X|Xs])             -> {X, Xs}.

-spec done(iterator())   -> boolean().
%% @doc done(It) is true iff iterator it is empty.
done([])                 -> true;
done([_|_])              -> false.

iterator_test() ->
 It0         = make([foo,1, bar,2]),
 {Elt1, It1} = next(It0),
 true        = Elt1 =:= {foo,1} orelse Elt1 =:= {bar,2},
 false       = done(It1),
 {Elt2, It}  = next(It1),
 true        = Elt2 =:= {foo,1} orelse Elt2 =:= {bar,2},
 true        = Elt1 =/= Elt2,
 true        = done(It).
%%%_* Private functions ================================================
normalize_deep_key(K) when is_binary(K) -> binary:split(K, <<".">>, [global]);
normalize_deep_key(K) when is_list(K)   -> string:tokens(K, ".").

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
