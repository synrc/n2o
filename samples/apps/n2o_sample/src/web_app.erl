-module(web_app).
-behaviour(application).
-export([start/2, stop/1]).

% this is for Erlang on Xen only

start(_StartType, _StartArgs) -> 
    [ application:start(X) || X <-
        [crypto,sasl,ranch,cowlib,cowboy,gproc,syntax_tools,compiler,erlydtl,rest,n2o]],

    web_sup:start_link().

stop(_State) -> ok.
