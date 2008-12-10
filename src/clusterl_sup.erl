-module(clusterl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
  Network = {clusterl_network,
             {clusterl_network, start_link, []},
             permanent,
             2000,
             worker,
             [clusterl_network]},
  Radio = {clusterl_radio,
           {clusterl_radio, start_link, []},
           permanent,
           2000,
           worker,
           [clusterl_radio]},
  {ok, {{one_for_all, 0, 1}, [Network, Radio]}}.

%%====================================================================
%% Internal functions
%%====================================================================
