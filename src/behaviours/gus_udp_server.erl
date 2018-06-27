-module(gus_udp_server).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-callback start_link(any()) -> {ok, pid()}.
-callback decode(any(), any(), any(), any()) -> any().

-record(state, {sock, mod, localstate}).

start_link(Mod, Port, LocalState) ->
    gen_server:start_link(?MODULE, [Mod, Port, LocalState], []).

init([Mod, Port, LocalState]) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, #state{sock=Sock, mod=Mod, localstate=LocalState}}.

handle_call(localstate, _From, State=#state{localstate=LocalState}) ->
    {reply, {ok, LocalState}, State};

handle_call({localstate, LocalState}, _From, State) ->
    {reply, ok, State#state{localstate=LocalState}};

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

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
    {noreply, State#state{localstate=LocalState}}.

terminate(_Reason, #state{sock=Sock}) ->
    gen_udp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
