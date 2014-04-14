-module(web_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> 
    application:start(crypto),
    application:start(sasl),
    application:start(ranch),
    application:start(cowboy),
    application:start(gproc),
    application:start(mimetypes),
    application:start(syntax_tools),
    application:start(compiler),
    application:start(erlydtl),
    application:start(rest),
    application:start(n2o),

    web_sup:start_link().

stop(_State) -> ok.
