-module(gustav).

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([packer/1,
	 packer/2,
	 unpacker/2]).

%%====================================================================
%% API functions
%%====================================================================

packer(Dictionary) ->
    packer(Dictionary, <<>>).

packer(Dictionary, BinaryData) ->
    fun(X) ->
	    case X of
		[] ->
		    BinaryData;
		[{Key, Size} | Tail] ->
		    {ok, Value} = maps:find(Key, Dictionary),
		    Next = <<Value:Size>>,
		    Blob = <<BinaryData/binary, Next/binary>>,
		    (packer(Dictionary, Blob))(Tail)
	    end
    end.


unpacker(Dictionary, BinaryData) ->
    fun(X) ->
	    case X of
		[] ->
		    Dictionary;
		[{Key, Size} | Tail] ->
		    <<Value:Size,
		      Rest/binary>> = BinaryData,
		    D = Dictionary#{ Key => Value },
		    (unpacker(D, Rest))(Tail)
	    end
    end.

%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Tests
%%====================================================================

packer_minimal_test() ->
    F = packer(#{a => 1, b => 2, c => 3, d => 4}),
    B = F([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
    ?assert(B =:= <<1:8, 2:16, 3:32, 4:8>>).

%%

unpacker_minimal_test() ->
    F = unpacker(#{}, <<1:8, 2:16, 3:32, 4:8>>),
    D = F([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
    ?assert(maps:find(a, D) =:= {ok, 1}),
    ?assert(maps:find(b, D) =:= {ok, 2}),
    ?assert(maps:find(c, D) =:= {ok, 3}),
    ?assert(maps:find(d, D) =:= {ok, 4}),
    ?assert(maps:find(e, D) =:= error).

unpacker_remain_prev_test() ->
    F = unpacker(#{e => 5}, <<1:8, 2:16, 3:32, 4:8>>),
    D = F([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
    ?assert(maps:find(a, D) =:= {ok, 1}),
    ?assert(maps:find(b, D) =:= {ok, 2}),
    ?assert(maps:find(c, D) =:= {ok, 3}),
    ?assert(maps:find(d, D) =:= {ok, 4}),
    ?assert(maps:find(e, D) =:= {ok, 5}).

unpacker_overwrite_prev_test() ->
    F = unpacker(#{a => 5}, <<1:8, 2:16, 3:32, 4:8>>),
    D = F([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
    ?assert(maps:find(a, D) =:= {ok, 1}),
    ?assert(maps:find(b, D) =:= {ok, 2}),
    ?assert(maps:find(c, D) =:= {ok, 3}),
    ?assert(maps:find(d, D) =:= {ok, 4}).
