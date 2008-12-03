%%%-------------------------------------------------------------------
%%% File    : clusterl_sup.erl
%%% Author  : Samuel Tesla <samuel@alieniloquent.com>
%%% Description : 
%%%
%%% Created :  3 Dec 2008 by Samuel Tesla <samuel@alieniloquent.com>
%%%-------------------------------------------------------------------
-module(clusterl_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  {ok,{{one_for_all,0,1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
