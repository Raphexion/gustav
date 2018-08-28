-module(gus_bin).

-export([split/2,
	 combine/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%%====================================================================
%% API functions
%%====================================================================

split(Bin, BlockSize) ->
    Len = byte_size(Bin),
    Div = Len div BlockSize,
    Rem = Len rem BlockSize,
    split(Bin, BlockSize, Div, Rem, 0, []).

combine(Blocks) ->
    lists:foldl(fun(X, Acc) -> <<Acc/binary, X/binary>> end,
			<<>>,
			Blocks).


%%====================================================================
%% Internal functions
%%====================================================================

split(_Bin, _BlockSize, Div, 0, Div, Acc) ->
    lists:reverse(Acc);

split(Bin, BlockSize, Div, Rem, Div, Acc) ->
    Data =  binary:part(Bin, Div * BlockSize, Rem),
    lists:reverse([Data | Acc]);

split(Bin, BlockSize, Div, Rem, Index, Acc) ->
    Block = binary:part(Bin, Index * BlockSize, BlockSize),
    split(Bin, BlockSize, Div, Rem, Index + 1, [Block | Acc]).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(EUNIT).

minimal_1_test() ->
    Bin = <<1,2,3,4,5,6,7,8,9,10>>,
    ?assert(Bin =:= combine(split(Bin, 1))).

minimal_2_test() ->
    Bin = <<1,2,3,4,5,6,7,8,9,10>>,
    ?assert(Bin =:= combine(split(Bin, 2))).

minimal_3_test() ->
    Bin = <<1,2,3,4,5,6,7,8,9,10>>,
    ?assert(Bin =:= combine(split(Bin, 3))).

minimal_6_test() ->
    Bin = <<1,2,3,4,5,6,7,8,9,10>>,
    ?assert(Bin =:= combine(split(Bin, 6))).

-endif.
