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
  clusterl_sup:start_link().

stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
