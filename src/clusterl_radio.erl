-module(clusterl_radio).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_listener/1, broadcast/1, transmit/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id,
                listeners = sets:new(),
                radio_port,
                socket}).

-include("config.hrl").

-define(SERVER, {local, ?MODULE}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_server:start_link(?SERVER, ?MODULE, [?RADIO_PORT], []).

add_listener(Pid) when is_pid(Pid) ->
  gen_server:cast(?MODULE, {add_listener, Pid}).

broadcast(Signal) ->
  gen_server:cast(?MODULE, {broadcast, Signal}).

transmit(Ip, Port, Signal) ->
  gen_server:cast(?MODULE, {transmit, Ip, Port, Signal}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([RadioPort]) ->
  case open_socket(RadioPort) of
    {ok, Socket} ->
      {ok, Port} = inet:port(Socket),
      error_logger:info_msg("Radio Port = ~p~n", [Port]),
      {ok, #state{radio_port=RadioPort,
                  socket=Socket}};
    Error ->
      {stop, Error}
  end.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({add_listener, Pid}, State) ->
  #state{listeners=Listeners} = State,
  {noreply, State#state{listeners=sets:add_element(Pid, Listeners)}};

handle_cast({broadcast, Signal}, State) ->
  #state{radio_port=Port, socket=Socket} = State,
  transmit(Socket, ?BROADCAST_IP, Port, Signal),
  {noreply, State};

handle_cast({transmit, Ip, Port, Signal}, State) ->
  #state{socket=Socket} = State,
  transmit(Socket, Ip, Port, Signal),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({udp, S, Ip, _Port, Packet}, #state{socket=S} = State) ->
  #state{listeners=Listeners} = State,
  ok = inet:setopts(S, [{active, once}]),
  Signal = binary_to_term(Packet),
  sets:fold(fun(Pid, _) ->
                relay_signal(Pid, Ip, Signal)
            end, ok, Listeners),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
relay_signal(Pid, Ip, Signal) ->
  Pid ! {signal, Ip, Signal}.

open_socket(Port) ->
  case gen_udp:open(Port, [binary, {active, once}, {broadcast, true}]) of
    {ok, _} = Result -> Result;
    {error, eaddrinuse} -> open_socket(0);
    Error -> Error
  end.

transmit(Socket, Ip, Port, Signal) ->
  ok = gen_udp:send(Socket, Ip, Port, term_to_binary(Signal)).
