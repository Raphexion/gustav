-module(reader).

-export([read/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%%====================================================================
%% API functions
%%====================================================================

read([]) ->
    {error, "unexpected EOF read[]"};

read(["("|Tokens]) ->
    read_list(Tokens, []);

read([")"|_Tokens]) ->
    {error, "unexpected EOF read(')')"};

read([Token]) ->
    try
	list_to_integer(Token)
    catch
	error:badarg ->
	    try
		list_to_float(Token)
	    catch
		error:badarg ->
		    {symbol, Token}
	    end
    end;

read(Error) ->
    {error, "unable to read", Error}.

%%====================================================================
%% Internal functions
%%====================================================================
read_list([], Acc) ->
    {error, "unexpected EOF read_list([],", Acc, ")"};

read_list([")"], Acc) ->
    lists:reverse(Acc);

read_list(["("|Tokens], Acc) ->
    case read_list(Tokens, []) of
	{L, _Rem} ->
	    [L|Acc];
	L ->
	    [L|Acc]
    end;

read_list([")"|Tokens], Acc) ->
    {lists:reverse(Acc), Tokens};

read_list([Head|Tail], Acc) ->
    SubExpr = read([Head]),
    read_list(Tail, [SubExpr|Acc]).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(EUNIT).

read_integer_1_test() ->
    Text = "42",
    Tokens = read(tokenizer:tokenize(Text)),
    ?assert(Tokens =:= 42).

read_integer_2_test() ->
    Text = "x",
    Tokens = read(tokenizer:tokenize(Text)),
    ?assert(Tokens =:= {symbol, "x"}).

read_integer_3_test() ->
    Text = "(xy)",
    Tokens = tokenizer:tokenize(Text),
    Read = read(Tokens),
    ?assert(Read =:= [{symbol, "xy"}]).

read_integer_4_test() ->
    Text = "(xy zy)",
    Tokens = tokenizer:tokenize(Text),
    Read = read(Tokens),
    ?assert(Read =:= [{symbol, "xy"}, {symbol, "zy"}]).

read_integer_5_test() ->
    Text = "((xy))",
    Tokens = tokenizer:tokenize(Text),
    Read = read(Tokens),
    ?assert(Read =:= [[{symbol, "xy"}]]).

read_integer_6_test() ->
    Text = "(((xy)))",
    Tokens = tokenizer:tokenize(Text),
    Read = read(Tokens),
    ?assert(Read =:= [[[{symbol, "xy"}]]]).

read_integer_7_test() ->
    Text = "((xy qr))",
    Tokens = tokenizer:tokenize(Text),
    Read = read(Tokens),
    ?assert(Read =:= [[{symbol, "xy"}, {symbol, "qr"}]]).

-endif.
