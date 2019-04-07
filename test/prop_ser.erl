-module(prop_ser).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% WARNING, this test has two limitations
%% 1) We only test non-empty use-case (only sane use of lib)
%% 2) We assume that the size is big enough for the value
%%    The library will truncate as needed if number of bits
%%    are fewer than needed. For example, if you try to pack
%%    the number 2 in 1 bit.

prop_ser_minimal() ->
    ?FORALL(Type, non_empty(map(atom(), val_and_size())),
        begin
	    Dictionary = build_dictionary(Type),
	    PackDetails = build_pack_details(Type),

	    %% pack-up
 	    Packer = gus_ser:packer(Dictionary),
	    Payload = Packer(PackDetails),

	    %% un-pack
	    Unpacker = gus_ser:unpacker(#{}, Payload),
	    Dictionary =:= Unpacker(PackDetails)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

build_dictionary(Map) ->
    maps:map(fun(_, {V, _}) -> V end, Map).

build_pack_details(Map) ->
    maps:to_list(maps:map(fun(_, {_, S}) -> S end, Map)).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

val_and_size() ->
    ?SUCHTHAT(ValSize, {val(), size()}, size_big_enough(ValSize)).

val() ->
    pos_integer().

size() ->
    choose(1, 64).

size_big_enough({Val, Size}) ->
    Val < math:pow(2, Size) - 1.
