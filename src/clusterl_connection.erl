-module(clusterl_connection).

-behaviour(gen_fsm).

%% API
-export([start_link/3, transmit/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM states
%% state_name/2, state_name/3
-export([wait_for_connect/2,
         wait_for_socket/2,
         ready/2]).

-record(state, {direction,
                owner,
                peer,
                socket}).

-include("config.hrl").
-include("protocol.hrl").

%%====================================================================
%% API
%%====================================================================
start_link(Owner, Socket, Direction) when is_pid(Owner),
                                          is_port(Socket),
                                          Direction =:= inbound orelse
                                          Direction =:= outbound ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, [Owner, Direction], []),
  gen_tcp:controlling_process(Socket, Pid),
  gen_fsm:send_event(Pid, {set_socket, Socket}),
  {ok, Pid}.

transmit(Pid, Signal) when is_pid(Pid) ->
  gen_fsm:send_event(Pid, {transmit, Signal}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Owner, Direction]) ->
  {ok, wait_for_socket, #state{owner=Owner, direction=Direction}}.

%%% state_name(_Event, State) ->
%%%   {next_state, state_name, State}.

ready({transmit, Signal}, #state{socket=S} = State) ->
  transmit_signal(S, Signal),
  {next_state, ready, State};

ready(Event, State) ->
  error_logger:info_msg("Received Event: ~p~n", [Event]),
  {next_state, ready, State}.

wait_for_connect({connect, Id}, State) ->
  #state{direction=Direction, owner=Pid, socket=Socket} = State,
  case Direction of
    inbound -> transmit_signal(Socket, ?CONNECT(?ID));
    outbound -> ignore
  end,
  Pid ! {connection_opened, self(), Id},
  {next_state, ready, State};

wait_for_connect(timeout, State) ->
  #state{peer={{Q1,Q2,Q3,Q4},Port}} = State,
  error_logger:info_msg("Connection from ~B.~B.~B.~B:~B timed out.~n",
                        [Q1,Q2,Q3,Q4,Port]),
  %% TODO: Consider a non-normal exit status.
  {stop, normal, State}.

wait_for_socket({set_socket, Socket}, State) ->
  {ok, Peer} = inet:peername(Socket),
  case State#state.direction of
    inbound -> ignore;
    outbound -> transmit_signal(Socket, ?CONNECT(?ID))
  end,
  ok = inet:setopts(Socket, [{active, once}]),
  NewState = State#state{peer=Peer, socket=Socket},
  {next_state, wait_for_connect, NewState, ?CONNECT_TIMEOUT};

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

handle_info({tcp, S, Data}, StateName, #state{socket=S} = State) ->
  Signal = binary_to_term(Data),
  case StateName of
    ready ->
      #state{owner=Pid, peer={Ip, _Port}} = State,
      Pid ! {signal, Ip, Signal};
    wait_for_connect ->
      case Signal of
        ?CONNECT(_) -> gen_fsm:send_event(self(), Signal);
        _ -> ignore
      end
  end,
  ok = inet:setopts(S, [{active, once}]),
  {next_state, StateName, State};

handle_info({tcp_closed, S}, StateName, #state{owner=Pid, socket=S} = State) ->
  case StateName of
    ready -> Pid ! {connection_closed, self()};
    _ -> ignore
  end,
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
transmit_signal(Socket, Signal) ->
  ok = gen_tcp:send(Socket, term_to_binary(Signal)).
