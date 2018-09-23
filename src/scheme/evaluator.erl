-module(evaluator).

-export([evaluate/1,
	 evaluate/2]).

evaluate(Expr) ->
    evaluate(Expr, environment:global_env()).

evaluate(Expr, Env) ->
    evaluate_inner(parser:parse(Expr), Env).

%%%
%%
%%%

evaluate_inner([{symbol, Rator}|Rands], Env) ->
    evaluate_symbol(Rator, Rands, maps:find(Rator, Env));

evaluate_inner(Expr, Env) ->
    {e, Expr, Env}.

%%

evaluate_symbol(_, Rands, {ok, F}) ->
    F(Rands);

evaluate_symbol(Rator, Rands, error) ->
    {error, unknown, Rator, Rands}.
