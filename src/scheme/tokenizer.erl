-module(tokenizer).

-export([tokenize/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%%====================================================================
%% API functions
%%====================================================================

tokenize(Text) ->
    clean(split(grow_open(grow_close(Text)))).

%%====================================================================
%% Internal functions
%%====================================================================

grow_open(Text) ->
    replace(Text, "(", " ( ").

grow_close(Text) ->
    replace(Text, ")", " ) ").

replace(Text, From, To) ->
    lists:append(string:replace(Text, From, To, all)).

split(Text) ->
    string:split(Text, " ", all).

clean(Tokens) ->
    [T || T <- Tokens, length(T) > 0].

%%====================================================================
%% Tests
%%====================================================================

-ifdef(EUNIT).

tokenize_minimal_test() ->
    Text = "(define (f x y) (+ x y))",
    Tokens = tokenize(Text),
    ?assert(Tokens =:= ["(", "define",
			"(", "f", "x", "y", ")",
			"(", "+", "x", "y", ")", ")"]).

-endif.
