%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Primitive type.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eon_type_prim).

%%%_* Exports ==========================================================
-export([ behaviour_info/1
        ]).

%%%_* Code =============================================================
behaviour_info(callbacks) ->
    [ {name,       0}
    , {parameters, 0}
    , {normalize,  2}
    , {validate,   2}
    , {convert,    2}
    ];
behaviour_info(_) -> undefined.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
