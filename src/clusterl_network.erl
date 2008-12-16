-module(clusterl_network).

-behaviour(gen_fsm).

%% API
-export([start_link/0, transmit/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state_name/2, state_name/3
%% disconnected
%% connected

%% Internal
-export([accept/2, connect/3]).

-include("config.hrl").
-include("protocol.hrl").

-record(state, {accept,
                link_ids = sets:new(),
                links = dict:new(),
                id,
                port,
                socket}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [?ID], []).

transmit(Signal) ->
  gen_fsm:send_all_state_event(?MODULE, {transmit, Signal}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Id]) ->
  process_flag(trap_exit, true),
  case open_socket() of
    {ok, Socket} ->
      {ok, Port} = inet:port(Socket),
      error_logger:info_msg("Network Id = ~p~nNetwork Port = ~p~n",
                            [Id, Port]),
      clusterl_radio:add_listener(self()),
      clusterl_radio:broadcast(?ANNOUNCE(Id, Port)),
      {ok, Pid} = spawn_accept(Socket),
      State = #state{accept=Pid,
                     id=Id,
                     port=Port,
                     socket=Socket},
      {ok, disconnected, State};
    Error ->
      {stop, Error}
  end.

%%% state_name(_Event, State) ->
%%%   {next_state, state_name, State}.

%%% state_name(_Event, _From, State) ->
%%%   Reply = ok,
%%%   {reply, Reply, state_name, State}.

handle_event({transmit, Signal}, connected, State) ->
  Links = dict:fetch_keys(State#state.links),
  rpc:pmap({clusterl_connection, transmit}, [Signal], Links),
  {next_state, connected, State};

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({'EXIT', _Pid, normal}, StateName, State) ->
  {next_state, StateName, State};

handle_info({'EXIT', Pid, Reason}, StateName,  #state{accept=Pid} = State) ->
  handle_accept_exit(Reason, StateName, State);

handle_info({connection_closed, Connection}, StateName, State) ->
  #state{link_ids=Ids, links=Links} = State,
  [Id] = dict:fetch(Connection, Links),
  error_logger:info_msg("Removing link for ~p~n", [Id]),
  NewIds = sets:del_element(Id, Ids),
  NewLinks = dict:erase(Connection, Links),
  {next_state, StateName, State#state{link_ids=NewIds, links=NewLinks}};

handle_info({connection_opened, Connection, Id}, StateName, State) ->
  #state{link_ids=Ids, links=Links} = State,
  case sets:is_element(Id, Ids) of
    true ->
      %% TODO: Consider sending something over the wire
      clusterl_connection:close(Connection),
      {next_state, StateName, State};
    false ->
      NewIds = sets:add_element(Id, Ids),
      NewLinks = dict:append(Connection, Id, Links),
      NewState = State#state{link_ids=NewIds, links=NewLinks},
      {next_state, connected, NewState}
  end;

handle_info({signal, Ip, Signal}, StateName, State) ->
  handle_signal(Ip, Signal, StateName, State),
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
accept(ListenSocket, Pid) ->
  case gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT) of
    {error, timeout} -> exit(normal);
    {error, Reason} -> exit({tcp, accept, Reason});
    {ok, Socket} -> clusterl_connection:start_link(Pid, Socket, inbound)
  end.


connect(Ip, TcpPort, Pid) ->
  Opts = [binary, {active, false}, {packet, 0}],
  case gen_tcp:connect(Ip, TcpPort, Opts, ?CONNECT_TIMEOUT) of
    {ok, Socket} -> clusterl_connection:start_link(Pid, Socket, outbound);
    _ -> ignore
  end.

handle_accept_exit(Reason, StateName, #state{socket=ListenSocket} = State) ->
  case Reason of
    {tcp, accept, emfile} ->
      {stop, {too_many_file_descriptors, this_process}, State};
    {tcp, accept, enfile} ->
      {stop, {too_many_file_descriptors, whole_system}, State};
    _ ->
      {ok, Accept} = spawn_accept(ListenSocket),
      {next_state, StateName, State#state{accept=Accept}}
  end.

handle_signal(Peer, ?ANNOUNCE(Id, Port), StateName, State)
  when is_integer(Port) ->
  handle_signal(Peer, ?ANNOUNCE(Id, {Peer, Port}), StateName, State);

handle_signal(_Peer, ?ANNOUNCE(Id, {Ip, Port}), _StateName, State) ->
  #state{id=MyId} = State,
  case Id of
    MyId ->
      ignore;
    _ ->
      proc_lib:spawn_link(?MODULE, connect, [Ip, Port, self()])
  end;

handle_signal(Peer, Signal, _StateName, _State) ->
  {Q1,Q2,Q3,Q4} = Peer,
  error_logger:info_msg("Signal from ~B.~B.~B.~B: ~p~n",
                        [Q1, Q2, Q3, Q4, Signal]).

open_socket() ->
  gen_tcp:listen(0, [binary,
                     {active, false},
                     {packet, 0},
                     {reuseaddr, true}]).

spawn_accept(ListenSocket) ->
  Pid = proc_lib:spawn_link(?MODULE, accept, [ListenSocket, self()]),
  {ok, Pid}.
