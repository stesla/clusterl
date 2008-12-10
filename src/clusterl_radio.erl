-module(clusterl_radio).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket}).

-define(SERVER, {local, ?MODULE}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_server:start_link(?SERVER, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  case open_socket() of
    {ok, Socket} ->
      {ok, Port} = inet:port(Socket),
      error_logger:info_msg("Radio Port = ~p~n", [Port]),
      {ok, #state{socket=Port}};
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
get_radio_port() ->
  case application:get_env(radio_port) of
    {ok, P} when is_integer(P) -> P;
    _ -> 0
  end.

open_socket() ->
  case open_socket(get_radio_port()) of
    {ok, _} = Result -> Result;
    {error, eaddrinuse} -> open_socket(0);
    Error -> Error
  end.

open_socket(Port) ->
  gen_udp:open(Port, [binary,
                      {active, once},
                      {broadcast, true}]).
