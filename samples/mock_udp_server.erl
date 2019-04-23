-module(mock_udp_server).
-behaviour(gus_udp_server).

-export([start_link/1,
	 start_link/2,
	 decode/4,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

start_link(Port) ->
    gus_udp_server:start_link(?MODULE, Port, #{cnt => 0}).

start_link(Port, ProcessName) ->
    gus_udp_server:start_link(?MODULE, Port, ProcessName, #{cnt => 0}).

decode(_Ip, _Port, Data, LocalState) ->
    {ok, Cnt} = maps:find(cnt, LocalState),
    {reply, LocalState#{cnt => Cnt + 1}, Data}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.
