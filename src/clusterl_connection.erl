-module(clusterl_connection).

-behaviour(gen_fsm).

%% API
-export([start_link/1, transmit/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM states
%% state_name/2, state_name/3
-export([wait_for_socket/2,
         ready/2]).

-record(state, {owner,
                peer,
                socket}).

%%====================================================================
%% API
%%====================================================================
start_link(Socket) when is_port(Socket) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [self()], []),
  gen_tcp:controlling_process(Socket, Pid),
  gen_fsm:send_event(Pid, {set_socket, Socket}),
  {ok, Pid}.

transmit(Pid, Signal) when is_pid(Pid) ->
  Data = term_to_binary(Signal),
  gen_fsm:send_event(Pid, {transmit, Data}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Owner]) ->
  {ok, wait_for_socket, #state{owner=Owner}}.

%%% state_name(_Event, State) ->
%%%   {next_state, state_name, State}.

ready({transmit, Data}, #state{socket=S} = State) ->
  ok = gen_tcp:send(S, Data),
  {next_state, ready, State};

ready(Event, State) ->
  error_logger:info_msg("Received Event: ~p~n", [Event]),
  {next_state, ready, State}.

wait_for_socket({set_socket, Socket}, State) ->
  {ok, Peer} = inet:peername(Socket),
  ok = inet:setopts(Socket, [{active, once}]),
  {next_state, ready, State#state{peer=Peer, socket=Socket}};
wait_for_socket(Event, State) ->
  error_logger:info_msg("Received Event: ~p~n", [Event]),
  {next_state, wait_for_socket, State}.

%%% state_name(_Event, _From, State) ->
%%%   Reply = ok,
%%%   {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({tcp, S, Data}, ready, #state{socket=S} = State) ->
  #state{owner=Pid, peer={Ip, _Port}} = State,
  Pid ! {signal, Ip, binary_to_term(Data)},
  ok = inet:setopts(S, [{active, once}]),
  {next_state, ready, State};

handle_info({tcp_closed, S}, ready, #state{owner=Pid, socket=S} = State) ->
  Pid ! {connection_closed, self()},
  {stop, normal, State};

handle_info({tcp_error, S, Reason}, ready, #state{socket=S} = State) ->
  {stop, {tcp_error, Reason}, State};

handle_info(Info, StateName, State) ->
  error_logger:info_msg("Received Message: ~p~n", [Info]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
