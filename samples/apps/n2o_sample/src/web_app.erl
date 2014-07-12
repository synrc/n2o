-module(web_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() -> start(normal, []).
start(_StartType, _StartArgs) -> 

    % this is for Erlang on Xen only

    [application:start(X) || X <-
        [crypto,sasl,ranch,cowlib,cowboy,gproc,syntax_tools,compiler,erlydtl,rest,xmerl,n2o]],
        
    % this is for n2o_route only

    [code:ensure_loaded(M) || M <-
        [index, login]],

    web_sup:start_link().

stop(_State) -> ok.
