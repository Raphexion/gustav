-module(mini_udp_server).
-behaviour(gus_udp_server).
-include("gus_mini.hrl").

-define(PACKING_SPEC, [{a, 8}, {b, 16}, {c, 32}]).

%% API
-export([start_link/2,
	 dict/1,
	 send_to/4]).

%% Behaviour callbacks
-export([decode/4,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Port, Dict) ->
    start_link(Port, Dict, fun default_decoder/2).

start_link(Port, Dict, Decoder) ->
    gus_udp_server:start_link(?MODULE, Port, #mini_state{decoder=Decoder, dict=Dict}).

dict(Server) ->
    {ok, #mini_state{dict=Dict}} = gen_server:call(Server, localstate),
    {ok, Dict}.

send_to(Pid, Address, Port, Message) ->
    gus_udp_server:send_to(Pid, Address, Port, Message).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

decode(_Ip, _Port, Data, LocalState=#mini_state{decoder=Decoder}) ->
    Decoder(LocalState, Data).

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

default_decoder(LocalState=#mini_state{dict=Dict0}, Payload) ->
    UnPacker = gus_ser:unpacker(Dict0, Payload),
    Dict1 = UnPacker(?PACKING_SPEC),
    {noreply, LocalState#mini_state{dict=Dict1}}.
