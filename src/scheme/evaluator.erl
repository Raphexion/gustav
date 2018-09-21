-module(evaluator).

-export([evaluate/1]).

evaluate(Expr) ->
    evaluate(parser:parse(Expr),
	     environment:global_env()).

evaluate([{symbol, Rator}|Rands], Env) ->
    evaluate_symbol(Rator, Rands, maps:find(Rator, Env));

evaluate(Expr, Env) ->
    {e, Expr, Env}.


%%%
%%
%%%

evaluate_symbol(_, Rands, {ok, F}) ->
    F(Rands);

evaluate_symbol(Rator, Rands, error) ->
    {error, unknown, Rator, Rands}.
