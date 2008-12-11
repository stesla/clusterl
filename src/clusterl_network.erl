-module(clusterl_network).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state_name/2, state_name/3
%% disconnected
%% connected
%% full

-include("protocol.hrl").

-record(state, {id,
                port,
                socket}).

%%====================================================================
%% API
%%====================================================================
start_link(Id) ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Id], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Id]) ->
  case open_socket() of
    {ok, Socket} ->
      {ok, Port} = inet:port(Socket),
      error_logger:info_msg("Network Port = ~p~n", [Port]),
      clusterl_radio:add_listener(self()),
      clusterl_radio:broadcast(?ANNOUNCE(Id, Port)),
      {ok, disconnected, #state{id=Id, port=Port, socket=Socket}};
    Error ->
      {stop, Error}
  end.

%%% state_name(_Event, State) ->
%%%   {next_state, state_name, State}.

%%% state_name(_Event, _From, State) ->
%%%   Reply = ok,
%%%   {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({radio_signal, Ip, UdpPort, Signal}, StateName, State) ->
  handle_signal(Ip, UdpPort, Signal, StateName, State),
  {next_state, StateName, State};

handle_info(Info, StateName, State) ->
  error_logger:info_msg("Received Message: ~p~n", [Info]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_signal(_Ip, _UdpPort, ?ANNOUNCE(Id, TcpPort), _StateName, State) ->
  #state{id=MyId, port=_MyTcpPort} = State,
  case Id of
    MyId -> ignore;
    _ ->
      error_logger:info_msg("ANNOUNCE ~p ~p~n", [Id, TcpPort])
  end;

handle_signal(Ip, UdpPort, Signal, _StateName, _State) ->
  {Q1, Q2, Q3, Q4} = Ip,
  error_logger:info_msg("Signal from ~B.~B.~B.~B:~B -- ~p~n",
                        [Q1, Q2, Q3, Q4, UdpPort, Signal]).

open_socket() ->
  gen_tcp:listen(0, [binary,
                     {active, once},
                     {packet, 0},
                     {reuseaddr, true}]).
