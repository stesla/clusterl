-module(clusterl_network).

-behaviour(gen_fsm).

%% API
-export([receive_signal/3, start_link/0, transmit/3]).

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
receive_signal(Pid, Ip, Signal) ->
  Pid ! {signal, self(), Ip, Signal}.

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [?ID], []).

transmit(Signal, To, Hops) ->
  gen_fsm:send_all_state_event(?MODULE, {transmit, Signal, To, Hops}).

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
      Header = ?HEADER(origin, any, infinity),
      clusterl_radio:broadcast(?FRAME(Header, ?ANNOUNCE(Id, Port))),
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

handle_event({transmit, Signal, To, Hops}, connected, State) ->
  Header = ?HEADER({id, State#state.id}, To, Hops),
  transmit(?FRAME(Header, Signal), links(State)),
  {next_state, connected, State};

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info({'EXIT', Pid, Reason}, StateName,  #state{accept=Pid} = State) ->
  handle_accept_exit(Reason, StateName, State);

handle_info({'EXIT', _Pid, normal}, StateName, State) ->
  {next_state, StateName, State};

handle_info({link_closed, Connection}, StateName, State) ->
  #state{link_ids=Ids, links=Links} = State,
  [Id] = dict:fetch(Connection, Links),
  NewIds = sets:del_element(Id, Ids),
  NewLinks = dict:erase(Connection, Links),
  {next_state, StateName, State#state{link_ids=NewIds, links=NewLinks}};

handle_info({link_opened, Connection, Id}, StateName, State) ->
  #state{link_ids=Ids, links=Links} = State,
  case sets:is_element(Id, Ids) of
    true ->
      %% TODO: Consider sending something over the wire
      clusterl_link:close(Connection),
      {next_state, StateName, State};
    false ->
      NewIds = sets:add_element(Id, Ids),
      NewLinks = dict:append(Connection, Id, Links),
      NewState = State#state{link_ids=NewIds, links=NewLinks},
      {next_state, connected, NewState}
  end;

handle_info({signal, Link, Ip, ?FRAME(Header, Data)}, StateName, State) ->
  NewHeader = case Header of
                ?HEADER(origin, To, Hops) ->
                  ?HEADER({ip, Ip}, To, Hops);
                _ ->
                  Header
              end,
  handle_frame(Link, NewHeader, Data, State),
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
    {ok, Socket} -> clusterl_link:start_link(Pid, Socket, inbound)
  end.


connect(Ip, TcpPort, Pid) ->
  Opts = [binary, {active, false}, {packet, 0}],
  case gen_tcp:connect(Ip, TcpPort, Opts, ?CONNECT_TIMEOUT) of
    {ok, Socket} -> clusterl_link:start_link(Pid, Socket, outbound);
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

%%%  relay(Signal, Link, links(State)),
%%%  proc_lib:spawn_link(?MODULE, connect, [Ip, Port, self()])

handle_data(_From, _To, ?ANNOUNCE(Id, _Port), #state{id=Id}) ->
  ignore;

handle_data({ip, Ip}, any, ?ANNOUNCE(_Id, Port), _State) ->
  proc_lib:spawn_link(?MODULE, connect, [Ip, Port, self()]);

handle_data(_From, _To, Data, _State) ->
  error_logger:info_msg("Received Data: ~p~n", [Data]).

handle_frame(_Link, ?HEADER({id, Id}, _To, _Hops), _Data, #state{id=Id}) ->
  ignore;

handle_frame(Link, ?HEADER(From, To, Hops), Data, State) ->
  handle_data(From, To, Data, State),
  Frame = case Hops of
    0 -> ignore;
    infinity -> ?FRAME(?HEADER(From, To, infinity), Data);
    X -> ?FRAME(?HEADER(From, To, X - 1), Data)
  end,
  relay(Frame, Link, links(State)).

links(#state{links=Links}) ->
  dict:fetch_keys(Links).

open_socket() ->
  gen_tcp:listen(0, [binary,
                     {active, false},
                     {packet, 0},
                     {reuseaddr, true}]).

relay(ignore, _FromLink, _Links) ->
  ignore;
relay(Frame, FromLink, Links) ->
  transmit(Frame, lists:delete(FromLink, Links)).

spawn_accept(ListenSocket) ->
  Pid = proc_lib:spawn_link(?MODULE, accept, [ListenSocket, self()]),
  {ok, Pid}.

transmit(Frame, Links) ->
  rpc:pmap({clusterl_link, transmit}, [Frame], Links).
