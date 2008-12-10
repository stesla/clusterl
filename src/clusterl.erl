-module(clusterl).

-behaviour(application).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API functions
%%====================================================================
start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, _StartArgs) ->
  {ok, Id} = application:get_env(id),
  {ok, RadioPort} = application:get_env(radio_port),
  error_logger:info_msg("Node Id = ~p~n", [Id]),
  clusterl_sup:start_link(Id, RadioPort).

stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
