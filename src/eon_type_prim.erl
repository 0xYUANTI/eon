%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Primitive type.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eon_type_prim).

%%%_* Callbacks ========================================================
-callback name()                          -> eon_type:name().
-callback parameters()                    -> eon_type:params().
-callback normalize(_, eon_type:params()) -> _.
-callback validate(_, eon_type:params())  -> boolean().
-callback convert(_, eon_type:params())   -> _.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
