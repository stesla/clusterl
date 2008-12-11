-module(clusterl_connection).

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM states
%% state_name/2, state_name/3
-export([wait_for_socket/2,
         ready/2]).

-record(state, {id,
                owner,
                socket}).

%%====================================================================
%% API
%%====================================================================
start_link(Owner, Id, Socket) when is_pid(Owner), is_port(Socket) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [Owner, Id], []),
  gen_tcp:controlling_process(Socket, Pid),
  gen_fsm:send_event(Pid, {set_socket, Socket}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Owner, Id]) ->
  {ok, wait_for_socket, #state{id=Id, owner=Owner}}.

%%% state_name(_Event, State) ->
%%%   {next_state, state_name, State}.

ready(Event, State) ->
  error_logger:info_msg("Received Event: ~p~n", [Event]),
  {next_state, ready, State}.

wait_for_socket({set_socket, Socket}, State) ->
  {next_state, ready, State#state{socket=Socket}};
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
  #state{id=Id, owner=Owner} = State,
  Owner ! {connection_signal, Id, binary_to_term(Data)},
  {next_state, ready, State};

handle_info({tcp_closed, S}, ready, #state{socket=S} = State) ->
  #state{id=Id, owner=Owner} = State,
  Owner ! {connection_closed, Id},
  {next_state, ready, State};

handle_info({tcp_error, S, Reason}, ready, #state{socket=S} = State) ->
  {stop, {tcp_error, Reason}, State};

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
