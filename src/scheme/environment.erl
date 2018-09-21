-module(environment).

-export([global_env/0,
	 new_env/1]).

global_env() ->
    #{ "+" => fun scheme_math:add/1
     , "-" => fun scheme_math:sub/1
     }.

new_env(Map) ->
    maps:merge(global_env(), Map).
