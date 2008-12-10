-module(clusterl_network).

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
      error_logger:info_msg("Network Port = ~p~n", [Port]),
      {ok, #state{socket=Socket}};
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
open_socket() ->
  gen_tcp:listen(0, [binary,
                     {active, once},
                     {packet, 0},
                     {reuseaddr, true}]).
