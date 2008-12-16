-module(clusterl_config).

-export([value/1]).

value(Key) ->
  {ok, Value} = application:get_env(clusterl, Key),
  Value.
