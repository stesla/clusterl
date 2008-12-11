-module(clusterl_sup).

-behaviour(supervisor).

%% API
-export([start_connection/2, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%%====================================================================
%% API functions
%%====================================================================
start_link(Id, RadioPort) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Id, RadioPort]).

start_connection(Id, Socket) ->
  supervisor:start_child(clusterl_connection_sup, [self(), Id, Socket]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([Id, RadioPort]) ->
  ConnectionSup = {clusterl_connection_sup,
                   {supervisor,
                    start_link,
                    [{local, clusterl_connection_sup}, ?MODULE, [connection]]},
                   permanent,
                   infinity,
                   supervisor,
                   []},
  Radio = {clusterl_radio,
           {clusterl_radio, start_link, [RadioPort]},
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
  {ok, {{one_for_all, 0, 1},
        [ConnectionSup,
         Radio,
         Network]}};

init([connection]) ->
  {ok,
   {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
    [{undefined,
      {clusterl_connection, start_link, []},
      temporary,
      2000,
      worker,
      []}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
