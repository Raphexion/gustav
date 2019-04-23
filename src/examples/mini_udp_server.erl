-module(mini_udp_server).
-behaviour(gus_udp_server).
-include("gus_mini.hrl").

-define(PACKING_SPEC, [{a, 8}, {b, 16}, {c, 32}]).

%% API
-export([start_link/2,
	 status/1,
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
    gus_udp_server:start_link(?MODULE, Port, Dict).

status(Server) ->
    gen_server:call(Server, localstate).

send_to(Pid, Address, Port, Message) ->
    gus_udp_server:send_to(Pid, Address, Port, Message).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

decode(_Ip, _Port, Data, LocalState0) ->
    UnPacker = gus_ser:unpacker(LocalState0, Data),
    LocalState1 = UnPacker(?PACKING_SPEC),
    {noreply, LocalState1}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.
