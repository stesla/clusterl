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

%% Internal
-export([accept/2, connect/4]).

-include("protocol.hrl").

-record(state, {accept,
                connections = dict:new(),
                id,
                port,
                socket}).

-define(ACCEPT_TIMEOUT, 10000).
-define(CONNECT_TIMEOUT, 60000).

%%====================================================================
%% API
%%====================================================================
start_link(Id) ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Id], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Id]) ->
  process_flag(trap_exit, true),
  case open_socket() of
    {ok, Socket} ->
      {ok, Port} = inet:port(Socket),
      error_logger:info_msg("Network Port = ~p~n", [Port]),
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

handle_event({accepted, Socket}, StateName, State) ->
  {ok, {Address, Port}} = inet:peername(Socket),
  error_logger:info_msg("Connection from ~p ~p~n", [Address, Port]),
  ok = inet:setopts(Socket, [{active, once}]),
  {next_state, StateName, State};

handle_event({connected, Id, Socket}, StateName, State) ->
  {ok, {Address, Port}} = inet:peername(Socket),
  error_logger:info_msg("Connected to ~p on ~p ~p~n", [Id, Address, Port]),
  ok = inet:setopts(Socket, [{active, once}]),
  {next_state, StateName, State};

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({'EXIT', _Pid, normal}, StateName, State) ->
  {next_state, StateName, State};

handle_info({'EXIT', Pid, Reason}, StateName,  #state{accept=Pid} = State) ->
  handle_accept_exit(Reason, StateName, State);

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
accept(ListenSocket, Pid) ->
  case gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT) of
    {ok, Socket} ->
      gen_tcp:controlling_process(Socket, Pid),
      gen_fsm:send_all_state_event(Pid, {accepted, Socket});
    {error, timeout} ->
      exit(normal);
    {error, Reason} ->
      exit({tcp, accept, Reason})
  end.

connect(Id, Ip, TcpPort, Pid) ->
  Opts = [binary, {active, false}, {packet, 0}],
  case gen_tcp:connect(Ip, TcpPort, Opts, ?CONNECT_TIMEOUT) of
    {ok, Socket} ->
      gen_tcp:controlling_process(Socket, Pid),
      gen_fsm:send_all_state_event(Pid, {connected, Id, Socket});
    _ ->
      ignore
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

handle_signal(_Ip, _UdpPort, ?ANNOUNCE(_Id, _TcpPort) = _Signal, full, _State) ->
  error_logger:info_msg("ANNOUNCE WHEN FULL~n");

handle_signal(Ip, _UdpPort, ?ANNOUNCE(Id, TcpPort), _StateName, State) ->
  #state{id=MyId, port=_MyTcpPort} = State,
  case Id of
    MyId -> ignore;
    _ ->
      error_logger:info_msg("ANNOUNCE ~p ~p~n", [Id, TcpPort]),
      proc_lib:spawn_link(?MODULE, connect, [Id, Ip, TcpPort, self()])
  end;

handle_signal(Ip, UdpPort, Signal, _StateName, _State) ->
  {Q1, Q2, Q3, Q4} = Ip,
  error_logger:info_msg("Signal from ~B.~B.~B.~B:~B -- ~p~n",
                        [Q1, Q2, Q3, Q4, UdpPort, Signal]).

open_socket() ->
  gen_tcp:listen(0, [binary,
                     {active, false},
                     {packet, 0},
                     {reuseaddr, true}]).

spawn_accept(ListenSocket) ->
  Pid = proc_lib:spawn_link(?MODULE, accept, [ListenSocket, self()]),
  {ok, Pid}.
