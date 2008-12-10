-module(clusterl_radio).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id,
                radio_port,
                socket}).

-define(SERVER, {local, ?MODULE}).

%%====================================================================
%% API
%%====================================================================
start_link(Id, RadioPort) ->
  gen_server:start_link(?SERVER, ?MODULE, [Id, RadioPort], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Id, RadioPort]) ->
  case open_socket(RadioPort) of
    {ok, Socket} ->
      {ok, Port} = inet:port(Socket),
      error_logger:info_msg("Radio Port = ~p~n", [Port]),
      {ok, #state{id=Id,
                  radio_port=RadioPort,
                  socket=Port}};
    Error ->
      {stop, Error}
  end.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
open_socket(Port) ->
  case gen_udp:open(Port, [binary, {active, once}, {broadcast, true}]) of
    {ok, _} = Result -> Result;
    {error, eaddrinuse} -> open_socket(0);
    Error -> Error
  end.
