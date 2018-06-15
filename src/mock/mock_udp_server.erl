-module(mock_udp_server).
-behaviour(gus_udp_server).

-export([start_link/1]).
-export([decode/4]).

start_link(Port) ->
    gus_udp_server:start_link(?MODULE, Port, #{cnt => 0}).

decode(_Ip, _Port, Data, LocalState) ->
    {ok, Cnt} = maps:find(cnt, LocalState),
    {reply, LocalState#{cnt => Cnt + 1}, Data}.
