-module(clusterl_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link(Id, RadioPort) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Id, RadioPort]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([Id, RadioPort]) ->
  Radio = {clusterl_radio,
           {clusterl_radio, start_link, [Id, RadioPort]},
           permanent,
           2000,
           worker,
           [clusterl_radio]},
  Network = {clusterl_network,
             {clusterl_network, start_link, [Id]},
             permanent,
             2000,
             worker,
             [clusterl_network]},
  {ok, {{one_for_all, 0, 1}, [Radio, Network]}}.

%%====================================================================
%% Internal functions
%%====================================================================
