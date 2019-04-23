-module(prop_mini_udp).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(KeyValues, key_values(),
	    begin
		Port = 12345,
		Dict = #{a => 0, b => 0, c => 0},
		{ok, S} = mini_udp_server:start_link(Port, Dict),
		{ok, C} = mini_udp_client:start_link("localhost", Port, Dict),

		{ok, Dict} =:= mini_udp_server:status(S)
		    andalso updates(Dict, S, C, KeyValues)
		    andalso close(S, C)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

updates(_Dict, _S, _C, []) ->
    true;
updates(Dict0, S, C, [{Key, Value}|Rest]) ->
    Dict = maps:put(Key, Value, Dict0),
    mini_udp_client:set(C, Key, Value),
    timer:sleep(50),
    {ok, Dict} =:= mini_udp_server:status(S)
	andalso updates(Dict, S, C, Rest).

close(S, C) ->
    gen_server:stop(S),
    gen_server:stop(C),
    true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

key() ->
    oneof([a, b, c]).

value() ->
    range(1, 255).

key_value() ->
    {key(), value()}.

key_values() ->
    list(key_value()).
