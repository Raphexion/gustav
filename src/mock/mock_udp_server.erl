-module(mock_udp_server).
-behaviour(gus_udp_server).

-export([start_link/1]).
-export([decode/4]).

start_link(Port) ->
    gus_udp_server:start_link(?MODULE, Port).

decode(_Client, _Ip, _Port, Data) ->
    {reply, Data}.
