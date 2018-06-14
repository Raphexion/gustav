-module(gus_udp_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-callback start_link(any()) -> {ok, pid()}.
-callback decode(any(), any(), any()) -> any().

-record(state, {sock, mod}).

start_link(Mod, Port) ->
    gen_server:start_link(?MODULE, [Mod, Port], []).

init([Mod, Port]) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, #state{sock=Sock, mod=Mod}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({udp, _Client, Ip, Port, Data}, State=#state{sock=Sock, mod=Mod}) ->
    case Mod:decode(Ip, Port, Data) of
	{reply, Payload} ->
	    gen_udp:send(Sock, Ip, Port, Payload);
	_ ->
	    ok
    end,
    {noreply, State}.

terminate(_Reason, #state{sock=Sock}) ->
    gen_udp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
