-module(gus_udp_server).
-behaviour(gen_server).

-export([send_to/4]).
-export([start_link/3,
	 start_link/4]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-callback decode(any(), any(), any(), any()) -> any().
-callback handle_call(any(), any(), any()) -> any().
-callback handle_cast(any(), any()) -> any().
-callback handle_info(any(), any()) -> any().

-record(state, {sock, mod, localstate}).

send_to(Pid, Address, Port, Message) ->
    gen_server:call(Pid, {send_to, Address, Port, Message}).

start_link(Mod, Port, LocalState) ->
    gen_server:start_link(?MODULE, [Mod, Port, LocalState], []).

start_link(Mod, Port, ProcessName, LocalState) ->
    gen_server:start_link(?MODULE, [Mod, Port, ProcessName, LocalState], []).

init([Mod, Port, LocalState]) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, #state{sock=Sock, mod=Mod, localstate=LocalState}};

init([Mod, Port, ProcessName, LocalState]) ->
    register(ProcessName, self()),
    init([Mod, Port, LocalState]).

handle_call({send_to, Address, Port, Message}, _From, State=#state{sock=Sock}) ->
    Res = gen_udp:send(Sock, Address, Port, Message),
    {reply, {ok, Res}, State};

handle_call(localstate, _From, State=#state{localstate=LocalState}) ->
    {reply, {ok, LocalState}, State};

handle_call({localstate, LocalState}, _From, State) ->
    {reply, ok, State#state{localstate=LocalState}};

handle_call(What, From, State=#state{mod=Mod, localstate=LocalState0}) ->
    {reply, Response, LocalState} = Mod:handle_call(What, From, LocalState0),
    {reply, Response, State#state{localstate=LocalState}}.

handle_cast(What, State=#state{mod=Mod, localstate=LocalState0}) ->
    {noreply, LocalState} = Mod:handle_cast(What, LocalState0),
    {noreply, State#state{localstate=LocalState}}.

handle_info({udp, _Client, Ip, Port, Data}, State=#state{sock=Sock, mod=Mod, localstate=LocalState0}) ->
    LocalState = case Mod:decode(Ip, Port, Data, LocalState0) of
		     {reply, LocalState1, Payload} ->
			 gen_udp:send(Sock, Ip, Port, Payload),
			 LocalState1;

		     {reply, LocalState1, Payload, DPort} ->
			 gen_udp:send(Sock, Ip, DPort, Payload),
			 LocalState1;

		     {reply, LocalState1, Payload, DIp, DPort} ->
			 gen_udp:send(Sock, DIp, DPort, Payload),
			 LocalState1;

		     {noreply, LocalState1} ->
			 LocalState1;

		     _ ->
			 LocalState0
		 end,
    {noreply, State#state{localstate=LocalState}};

handle_info(What, State=#state{mod=Mod, localstate=LocalState0}) ->
    {noreply, LocalState} = Mod:handle_info(What, LocalState0),
    {noreply, State#state{localstate=LocalState}}.

terminate(_Reason, #state{sock=Sock}) ->
    gen_udp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
