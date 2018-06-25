-module(gus_ser).

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
		    case maps:find(Key, Dictionary) of
			{ok, Value} ->
			    Next = <<Value:Size>>,
			    Blob = <<BinaryData/bitstring, Next/bitstring>>,
			    (packer(Dictionary, Blob))(Tail);

			error ->
			    {error, {key_not_found, Key}}
		    end
	    end
    end.


unpacker(Dictionary, <<>>) ->
    fun(X) ->
	    case X of
		[] ->
		    Dictionary;
		[{Key, Size} | _Tail] ->
		    {error, {not_enough_data, {Key, Size}}}
	    end
    end;

unpacker(Dictionary, BinaryData) ->
    fun(X) ->
	    case X of
		[] ->
		    Dictionary;
		[{Key, Size} | Tail] ->
		    <<Value:Size, Rest/binary>> = BinaryData,
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

packer_minimal_bitstring_test() ->
    F = packer(#{a => 1, b => 2, c => 3, d => 4}),
    B = F([{a, 7}, {b, 9}]),
    ?assert(B =:= <<1:7, 2:9>>).

packer_missing_key_test() ->
    F = packer(#{a => 1}),
    B = F([{a, 8}, {b, 16}]),
    ?assert(B =:= {error, {key_not_found, b}}).

%%

unpacker_minimal_test() ->
    F = unpacker(#{}, <<1:8, 2:16, 3:32, 4:8>>),
    D = F([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
    ?assert(maps:find(a, D) =:= {ok, 1}),
    ?assert(maps:find(b, D) =:= {ok, 2}),
    ?assert(maps:find(c, D) =:= {ok, 3}),
    ?assert(maps:find(d, D) =:= {ok, 4}),
    ?assert(maps:find(e, D) =:= error).

unpacker_missing_data_test() ->
    F = unpacker(#{}, <<1:8>>),
    D = F([{a, 8}, {b, 16}]),
    ?assert(D =:= {error, {not_enough_data, {b, 16}}}).

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
