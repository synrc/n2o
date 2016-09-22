-module(review).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-include_lib("kvs/include/user.hrl").

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,review},review,[]).
stop(_)    -> ok.

-define(USERS, [#user{id="maxim",email="maxim@synrc.com"},
                #user{id="doxtop",email="doxtop@synrc.com"},
                #user{id="roman",email="roman@github.com"}]).

init([]) -> users:init(),
            users:populate(?USERS),
            kvs:join(),
            {ok, {{one_for_one, 5, 10}, [spec()]}}.

spec()   -> ranch:child_spec(http, 100, ranch_tcp, port(), cowboy_protocol, env()).
env()    -> [ { env, [ { dispatch, points() } ] } ].
static() ->   { dir, "apps/review/priv/static", mime() }.
n2o()    ->   { dir, "deps/n2o/priv",           mime() }.
mime()   -> [ { mimetypes, cow_mimetypes, all   } ].
port()   -> [ { port, wf:config(n2o,port,8000)  } ].
points() -> cowboy_router:compile([{'_', [

    {"/static/[...]",       n2o_static,  static()},
    {"/n2o/[...]",          n2o_static,  n2o()},
    {"/multipart/[...]",  n2o_multipart, []},
    {"/rest/:resource",     rest_cowboy, []},
    {"/rest/:resource/:id", rest_cowboy, []},
    {"/ws/[...]",           n2o_stream,  []},
    {'_',                   n2o_cowboy,  []} ]}]).
