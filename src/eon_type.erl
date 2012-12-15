%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Type checker.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eon_type).

%%%_* Exports ==========================================================
-export([ check_obj/2
        , check_term/2
        ]).

%%%_* Includes =========================================================
-include("eon.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Macros ===========================================================
-define(dummy, '__dummy__').

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-type decl(A)    :: lit(A, spec()).    %declaration
-type spec()     :: type()             %single alternative
                  | sum().             %multiple alternatives
-type type()     :: cb()               %eon_type_*
                  | {cb(), lit(_, _)}. %type args
-type sum()      :: [spec()].          %left-to-right

-record(spec,
        { term   :: _                  %input
        , type   :: cb()               %declared type
        , p_have :: obj(_, _)          %instantiated params
        , p_need :: obj(_, _)          %uninstantiated params
        }).

%%%_ * API -------------------------------------------------------------
-spec check_obj(obj(A, _), decl(A)) -> maybe(obj(A, _), _).
%% @doc check_obj(Obj, Decl) is the converted version of Obj if it
%% conforms to Decl or an error if not.
check_obj(Obj, Decl) ->
  s2_maybe:do(
    [ ?thunk(parse(Obj, Decl))
    , fun check/1
    ]).

-spec check_term(_, spec()) -> maybe(_, _).
%% @doc check_term(Term, Spec) is the converted version of Term if it
%% conforms to Spec or an error if not.
check_term(Term, Spec) ->
  s2_maybe:do([ ?thunk(check_obj([term,Term], [term,Spec]))
              , fun(Obj) -> eon:get(Obj, term) end
              ]).

%%%_ * Parsing ---------------------------------------------------------
-spec parse(obj(A, _), decl(A)) -> [obj(A, #spec{})].
parse(Obj, Decl) ->
  s2_maybe:do(
    [ ?thunk({Obj, Decl})
    , fun to_obj/1
    , fun resolve/1
    , fun normalize/1
    , fun expand/1
    , fun merge/1
    ]).

%% Literal->object; basic checks.
to_obj({Obj, Decl})               -> {eon:new(Obj), eon:new(Decl, fun is_spec/1)}.

is_spec(T) when is_atom(T)        -> true;
is_spec({T, _As}) when is_atom(T) -> true;
is_spec(Sum) when is_list(Sum)    -> [] =:= [X || X <- Sum, not is_spec(X)].

%% Resolve aliasing.
resolve({Obj, Decl}) -> {Obj, eon:map(fun do_resolve/1, Decl)}.

do_resolve(T) when is_atom(T) ->
  case is_alias(T) of
    true  -> do_resolve(T:rewrite());
    false -> T
  end;
do_resolve({T, As}) when is_atom(T) ->
  case is_alias(T) of
    true  -> do_resolve(T:rewrite(As));
    false -> {T, As}
  end;
do_resolve(Sum) when is_list(Sum) ->
  [do_resolve(X) || X <- Sum].

%% Add args; unnest sums.
normalize({Obj, Decl})          -> {Obj, eon:map(fun do_normalize/1, Decl)}.

do_normalize(T) when is_atom(T) -> {T, eon:new()};
do_normalize({T, As})           -> {T, As};
do_normalize(Sum)               -> lists:flatten([do_normalize(X) || X<-Sum]).

%% Build explicit proposition tree.
expand({Obj, Decl}) -> {Obj, do_expand(Decl)}.

do_expand(Decl) ->
  case do_expand1(eon:make(Decl), Decl) of
    []   -> [Decl];
    Alts -> lists:flatmap(fun do_expand/1, Alts)
  end.

do_expand1(It0, Decl) ->
  case eon:done(It0) of
    true  -> [];
    false ->
      case eon:next(It0) of
        {{_K, {_T, _As}}, It} ->
          do_expand1(It, Decl);
        {{K, Sum}, _} ->
          lists:flatmap(fun(T) -> do_expand(eon:set(Decl, K, T)) end, Sum)
      end
  end.

%% Generate #spec{} records.
merge({Obj, Decls}) -> [do_merge(Obj, Decl) || Decl <- Decls].

do_merge(Obj, Decl) ->
  eon:map(fun({Term, {Type, Args0}}) ->
            Args   = eon:new(Args0),
            Params = eon:new([case P of
                                {_, _} -> P; %default value
                                _      -> {P, ?dummy}
                              end || P <- Type:parameters()]),
            ?hence(eon:is_empty(eon:difference(Args, Params))),
            #spec{ term   = Term
                 , type   = Type
                 , p_have = Args
                 , p_need = eon:difference(Params, Args)
                 }
          end, eon:zip(Obj, Decl)).

%%%_ * Type Checking ---------------------------------------------------
check(Alts) ->
  check(Alts, []).

%% Try alternatives until one passes the typechecker.
check([Alt|Alts], Rsns) ->
  case ?lift(fixpoint(Alt)) of
    {ok, Obj} ->
      case is_instantiated(Obj) of
        true  -> Obj; %success
        false -> check(Alts, [{missing_params, missing_params(Obj)}|Rsns])
      end;
    {error, Rsn} ->
      %% We don't differentiate between crash in a callback function and
      %% internal crash yet.
      check(Alts, [Rsn|Rsns])
  end;
check([], Rsns) ->
  {error, {untypable, Rsns}}.

is_instantiated(Obj) ->
  eon:is_empty(eon:filter(fun(V) -> not is_value(V) end, Obj)).

is_value(#spec{}) -> false;
is_value(_)       -> true.

missing_params(Obj) ->
  eon:fold(
    fun(K, #spec{type=Type, p_need=P_need}, Acc) ->
        [{K, Type, eon:keys(P_need)}|Acc];
       (_, _, Acc) ->
        Acc
    end, [], Obj).

%% On each iteration, we try to reduce one or more specs to values.
%% To reduce a spec, we need #spec.p_have to contain all arguments
%% required by #spec.type (i.e. #spec.p_need is empty).
%% We then call #spec.type:validate(#spec.term, #spec.p_have) to
%% get a value (or check(#spec.type:spec(#spec.term, #spec.p_have) for
%% recursive types).
%%
%% Type parameters are accumulated as follows:
%% 1) Explicit arguments, i.e. the As in a {T, As} type spec.
%% These take precedence over everything else.
%% 2) Implicit arguments. An entry in the current object which has
%% already been reduced to a value may contribute to another entry's
%% parameters as follows. If the value-entry's key matches a parameter
%% name, the value-entry's value will be used as the value of that
%% parameter.
%% 3) Default arguments. If no explicit arguments were provided
%% and no implicit arguments could be found, the type checker will use
%% default values provided by the type's parameters/0 callback, if any.
%%
%% The practical implications of this are that the type checker will
%% automagically figure out the correct order in which to check input
%% fields and that more complex types can depend on contextual
%% information provided by simpler types.
fixpoint(Obj) ->
  fixpoint(Obj, step1(Obj)).

fixpoint(Obj0, Obj1) ->
  case eon:equal(Obj0, Obj1) of
    true ->
      Obj2 = step2(Obj1),
      case eon:equal(Obj1, Obj2) of
        true  -> Obj2;
        false -> fixpoint(Obj2, step1(Obj2))
      end;
    false ->
      fixpoint(Obj1, step1(Obj1))
  end.

step1(Obj) ->
  (redfun(fun reduce_spec/3))((redfun(fun extend_spec_implicit/3))(Obj)).

step2(Obj) ->
  (redfun(fun extend_spec_default/3))(Obj).

redfun(F) ->
  fun(Obj) ->
    eon:map(fun(Key, #spec{} = S) -> F(Key, S, Obj);
               (_Key, Val)        -> Val
            end, Obj)
  end.

%% Parameter substitution.
extend_spec_implicit(_Key, #spec{p_have=P_have0, p_need=P_need0} = Spec, Obj) ->
  Potential = eon:filter(fun is_value/1, Obj),
  Implicit  = eon:intersection(Potential, P_need0),
  P_have    = eon:union(P_have0, Implicit),
  P_need    = eon:difference(P_need0, Implicit),
  Spec#spec{p_have=P_have, p_need=P_need}.

extend_spec_default(_Key, #spec{p_have=P_have0, p_need=P_need0} = Spec, _Obj) ->
  Default = eon:filter(fun(V) -> V =/= ?dummy end, P_need0),
  P_have  = eon:union(P_have0, Default),
  P_need  = eon:difference(P_need0, Default),
  Spec#spec{p_have=P_have, p_need=P_need}.

%% Typing.
reduce_spec(Key, Spec, _Obj) ->
  case is_parameterized(Spec) of
    true  -> type(Key, Spec);
    false -> Spec
  end.

is_parameterized(#spec{p_need=P_need}) -> eon:is_empty(P_need).

type(Key, #spec{term=_Term, type=_Type, p_have=_P_have} = Spec) ->
  ?debug("~p :: ~p(~p)", [_Term, _Type, _P_have]),
  case ?lift(typecheck(Spec)) of
    {ok, Val}    -> Val;
    {error, Rsn} -> throw({error, {Key, Rsn}})
  end.

typecheck(#spec{term=Term0, type=Type, p_have=P_have}) ->
  case constructor(Type) of
    eon_type_prim ->
      Term = Type:normalize(Term0, P_have),
      case Type:validate(Term, P_have) of
        true  -> Type:convert(Term, P_have);
        false -> throw({error, {validate, Type:name()}})
      end;
    eon_type_rec ->
      ?unlift(check_obj(Term0, Type:decl(Term0, P_have)));
    eon_type_list ->
      [?unlift(check_term(T, Type:element_type(Term0, P_have)))|| T <- Term0]
  end.

%%%_ * Behaviours ------------------------------------------------------
is_alias(Type) -> constructor(Type) =:= eon_type_alias.

constructor(Type) ->
  [Constructor] = [B || B <- behaviours(Type),
                        is_constructor_behaviour(B)],
  Constructor.

behaviours(Type) ->
  lists:flatten(
    [Vs || {K, Vs} <- Type:module_info(attributes),
           is_behaviour_attribute(K)]).

is_behaviour_attribute(behaviour)        -> true;
is_behaviour_attribute(behavior)         -> true;
is_behaviour_attribute(_)                -> false.

is_constructor_behaviour(eon_type_alias) -> true;
is_constructor_behaviour(eon_type_list)  -> true;
is_constructor_behaviour(eon_type_prim)  -> true;
is_constructor_behaviour(eon_type_rec)   -> true;
is_constructor_behaviour(_)              -> false.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
  %% to_obj
  {error, _}    = parse(42, []),
  {error, _}    = parse([], [foo,42]),

  %% resolve
  {error, _}    = parse([foo,42], [foo,[test_bad_alias]]),

  %% normalize
  %% FIXME: not sure how this could fail
  Nil           = eon:new(),
  [{type, Nil}] = do_normalize([[type]]),

  %% expand
  [_, _]        = do_expand([ foo, {test_prim, []}
                            , bar, [{test_prim, []}, {test_rec, []}]
                            , baz, {test_rec, []}
                            ]),

  %% merge
  {error, _}    = parse([], [foo,test_prim]),
  {error, _}    = parse([foo,42], [foo,{test_prim, 42}]),
  {error, _}    = parse([foo,42], [foo,{test_prim, [snarf,blarg]}]),

  ok.

-define(T(Type), s2_atoms:catenate(['test_', Type])).

check_test() ->
  Decl =
    [ foo,  {test_prim, [max,42]}
    , bar,  test_alias
    , baz,  [{test_alias, [max,9]}, {test_alias, [max,10]}]
    , quux, test_rec
    ],

  %% Ok
  Obj =
    [ foo, 42
    , bar, 666
    , baz, 10
    , quux, [ foo, 42
            , bar, [0,1,1,0]
            ]
    ],
  {ok, _} = check_obj(Obj, Decl),

  %% Validation failure
  Decl1 = Decl,
  Obj1 =
    [ foo, 43 %too large
    , bar, 666
    , baz, 10
    , quux, [ foo, 42
            , bar, [0,1,1,0]
            ]
    ],
  {error, {untypable, [ {foo, {validate, int_range}}
                      , {baz, {validate, int_range}}
                      ]}} = check_obj(Obj1, Decl1),

  %% Missing parameters
  Decl2 =
    [ bar,  test_alias
    , baz,  {test_alias, [max,10]}
    , quux, test_rec
    ],

  Obj2 =
    [ bar, 666
    , baz, 10
    , quux, [ foo, 42
            , bar, [0,1,1,0]
            ]
    ],
  {error, {untypable, [{missing_params, [{quux, test_rec, [foo]}]}]}} =
    check_obj(Obj2, Decl2),

  ok.

cover_test() ->
  true  = is_behaviour_attribute(behavior),
  false = is_constructor_behaviour(foo).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
