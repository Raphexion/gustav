-module(mini_udp_client).
-behaviour(gen_server).
-define(PACKING_SPEC, [{a, 8}, {b, 16}, {c, 32}]).
-compile([debug_info, export_all]).

%% API

-export([start_link/3,
	 set/3,
	 client_port/1]).

%% Behaviour callbacks

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Host, Port, Dict) ->
    gen_server:start_link(?MODULE, {host_format(Host), Port, Dict}, []).

set(Client, Key, Value) ->
    gen_server:call(Client, {set, Key, Value}).

client_port(Client) ->
    gen_server:call(Client, port).

last_data(Client) ->
    gen_server:call(Client, last_data).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {socket,
		dict,
		host,
		port,
		data = <<>>}).

init({Host, Port, Dict}) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active,true}, {reuseaddr, true}]),
    {ok, #state{socket=Socket, dict=Dict, host=Host, port=Port}}.

handle_call({set, Key, Value}, _From, State=#state{dict=Dict0}) ->
    Dict1 = maps:put(Key, Value, Dict0),
    Packer = gus_ser:packer(Dict1),
    Payload = Packer(?PACKING_SPEC),
    send(Payload, State),
    {reply, ok, State#state{dict=Dict1}};

handle_call(port, _From, State=#state{socket=Socket}) ->
    {ok, {_Ip, Port}} = inet:sockname(Socket),
    {reply, {ok, Port}, State};

handle_call(last_data, _From, State=#state{data=Data}) ->
    {reply, {ok, Data}, State}.

handle_cast(What, State) ->
    {noreply, State}.

handle_info({udp, _, _, _, Data}, State) ->
    {noreply, State#state{data=Data}};

handle_info(What, State) ->
    io:fwrite(">>~p<<~n", [What]),
    {noreply, State}.

terminate(_Reason, #state{socket=Socket}) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

host_format(Host={_, _, _, _}) ->
    Host;
host_format("localhost") ->
    {127, 0, 0, 1};
host_format(Host) when is_list(Host) ->
    [A, B, C, D] = integers(string:tokens(Host, ".")),
    {A, B, C, D}.

integers(Values) ->
    integers(Values, []).

integers([], Acc) ->
    lists:reverse(Acc);
integers([H|T], Acc) ->
    integers(T, [list_to_integer(H)|Acc]).

send(Payload, #state{socket=Socket, host=Host, port=Port}) ->
    gen_udp:send(Socket, Host, Port, Payload).
