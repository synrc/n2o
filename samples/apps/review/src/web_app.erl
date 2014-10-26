-module(web_app).
-behaviour(application).
-export([start/0, start/2, stop/1, main/1]).

main(A) ->

   mad_repl:main(A).

start() -> start(normal, []).
start(_StartType, _StartArgs) -> 

    Res = web_sup:start_link(),
    Res.

stop(_State) -> ok.
