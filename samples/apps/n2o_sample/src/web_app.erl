-module(web_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

% this is for Erlang on Xen only

start() -> start(normal, []).
start(_StartType, _StartArgs) -> 
    [ begin error_logger:info_msg("Application: ~p Start: ~p",[X,application:start(X)]) end || X <-
        [crypto,sasl,ranch,cowlib,cowboy,gproc,syntax_tools,compiler,erlydtl,rest,xmerl,n2o]],

    web_sup:start_link().

stop(_State) -> ok.
