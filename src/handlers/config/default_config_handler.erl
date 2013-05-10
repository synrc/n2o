-module(default_config_handler).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-behaviour (config_handler).
-export([init/2, finish/2, get_value/4, get_values/4]).

init(_Config, _State) -> {ok, []}.
finish(_Config, _State) -> {ok, []}.

get_value(Key, DefaultValue, Config, State) ->
    case get_values(Key, [DefaultValue], Config, State) of
        [Value] -> 
            Value;
        Values ->
            error_logger:error_msg("Too many matching config values for key: ~p~n", [Key]),
            throw({nitrogen_error, too_many_matching_values, Key, Values})
    end.

get_values(Key, DefaultValue, _Config, _State) -> 
    case application:get_env(nitrogen, Key) of
        {ok, Value} -> 
            [Value];
        undefined ->
            DefaultValue
    end.
