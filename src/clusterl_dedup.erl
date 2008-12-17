-module(clusterl_dedup).

-export([init/0, first/1]).

-define(TABLE, ?MODULE).

-include("config.hrl").

init() ->
  ets:new(?TABLE, [public, named_table]).

first(Frame) ->
  Result = ets:insert_new(?TABLE, {Frame}),
  schedule_deletion(Result, Frame),
  Result.

schedule_deletion(false, _Frame) ->
  ignore;
schedule_deletion(true, Frame) ->
  F = fun() ->
          receive after ?DUPLICATE_TIMEOUT ->
                      ets:delete(?TABLE, Frame)
                  end
      end,
  proc_lib:spawn_link(F).

