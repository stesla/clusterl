-module(clusterl_network).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("protocol.hrl").

-record(state, {id,
                port,
                socket}).

-define(SERVER, {local, ?MODULE}).

%%====================================================================
%% API
%%====================================================================
start_link(Id) ->
  gen_server:start_link(?SERVER, ?MODULE, [Id], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Id]) ->
  case open_socket() of
    {ok, Socket} ->
      {ok, Port} = inet:port(Socket),
      error_logger:info_msg("Network Port = ~p~n", [Port]),
      clusterl_radio:add_listener(self()),
      clusterl_radio:broadcast(?ANNOUNCE(Id, Port)),
      {ok, #state{id=Id, port=Port, socket=Socket}};
    Error ->
      {stop, Error}
  end.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({radio_signal, Ip, UdpPort, Signal}, State) ->
  handle_signal(Ip, UdpPort, Signal, State),
  {noreply, State};

handle_info(Info, State) ->
  error_logger:info_msg("Received Message: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_signal(_Ip, _UdpPort, ?ANNOUNCE(Id, TcpPort), State) ->
  #state{id=MyId, port=_MyTcpPort} = State,
  case Id of
    MyId -> ignore;
    _ ->
      error_logger:info_msg("ANNOUNCE ~p ~p~n", [Id, TcpPort])
  end;

handle_signal(Ip, UdpPort, Signal, _State) ->
  {Q1, Q2, Q3, Q4} = Ip,
  error_logger:info_msg("Signal from ~B.~B.~B.~B:~B -- ~p~n",
                        [Q1, Q2, Q3, Q4, UdpPort, Signal]).

open_socket() ->
  gen_tcp:listen(0, [binary,
                     {active, once},
                     {packet, 0},
                     {reuseaddr, true}]).
