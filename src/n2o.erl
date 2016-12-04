-module(n2o).
-description('N2O OTP Application Server').
-behaviour(supervisor).
-include_lib("n2o/include/wf.hrl").
-behaviour(application).
-export([start/2, stop/1, init/1, proc/2]).

tables()   -> [ cookies, actions, globals, caching ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> X = supervisor:start_link({local,n2o},n2o,[]),
              n2o_async:start(#handler{module=?MODULE,class=system,group=n2o,state=[],name="timer"}),
              X.
stop(_)    -> ok.
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              case wf:config(n2o,mq) of n2o_syn -> syn:init(); _ -> ok end,
              { ok, { { one_for_one, 5, 10 }, [] } }.

proc(init,#handler{}=Async) ->
    wf:info(?MODULE,"Proc Init: ~p~n",[init]),
    Timer = timer_restart(ping()),
    {ok,Async#handler{state=Timer}};

proc({timer,ping},#handler{state=Timer}=Async) ->
    case Timer of undefined -> skip; _ -> erlang:cancel_timer(Timer) end,
    wf:info(?MODULE,"N2O Timer: ~p~n",[ping]),
    (wf:config(n2o,session,n2o_session)):invalidate_sessions(),
    wf:invalidate_cache(),
    {reply,ok,Async#handler{state=timer_restart(ping())}}.

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(1000*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(n2o,timer,{0,10,0}).
