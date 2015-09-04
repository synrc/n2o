-module(n2o_async).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

async(Fun) -> async("looper",Fun).
async(Name, F) ->
    Key = key(),
    Res = case n2o_async:start(#handler{module=?MODULE,class=async,group=n2o_sup,
                                        name={Name,Key},config={F,?REQ}}) of
        {ok,X} -> {n2o_async:pid({async,{Name,Key}}),{async,{Name,Key}}};
        {error,{already_started,Pid}} -> {Pid,{async,{Name,Key}}};
        Else -> Else end,
    n2o_async:send(Name,{parent,self()}),
    Res.

pid({Class,Name}) -> wf:cache({Class,Name}).
key() -> n2o_session:session_id().
restart(Name) -> exit(n2o_async:pid({async,{Name,key()}}),kill).
send(Name,Message) -> gen_server:call(n2o_async:pid({async,{Name,key()}}),Message).
flush() -> A=wf_context:actions(), wf:actions([]), get(parent) ! {flush,A}.
flush(Pool) -> A=wf_context:actions(), wf:actions([]), wf:send(Pool,{flush,A}).
stop(Name) -> [ supervisor:F(n2o_sup,{async,{Name,key()}})||F<-[terminate_child,delete_child]].

init_context(undefined) -> [];
init_context(Req) ->
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_context:fold(init, Ctx#cx.handlers, Ctx),
    wf:actions(NewCtx#cx.actions),
    wf:context(NewCtx).

start(#handler{class=Class,name=Name,module=Module,group=Group} = Async) ->
    ChildSpec = {{Class,Name},{?MODULE,start_link,[Async]},transient,5000,worker,[Module]},
    case supervisor:start_child(Group,ChildSpec) of
         {ok,_}   -> {ok,Async#handler.name};
         {ok,_,_} -> {ok,Async#handler.name};
         Else     -> Else end.

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).
code_change(_,State,_) -> {ok, State}.
terminate(_Reason, #handler{name=Name,group=Group,class=Class}) ->
    spawn(fun() -> supervisor:delete_child(Group,Name) end),
    wf:cache({Class,Name},undefined),
    ok.

handle_call(stop,_,   #handler{module=Mod}=Async) -> {stop,normal,Async};
handle_call(Message,_,#handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_cast(Message,  #handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_info(Message,  #handler{module=Mod}=Async) -> {_,_,State} = Mod:proc(Message,Async),
                                                     {noreply,State}.

init(#handler{class=Class,name=Name}=Handler) ->
    wf:info(?MODULE,"Async Handler ~p started ~p",[Handler,self()]),
    wf:cache({Class,Name},self()),
    (Handler#handler.module):proc(init,Handler).

proc(init,#handler{class=Class,name=Name,config={F,Req}}=Async) -> init_context(Req), {ok,Async};
proc({parent,Parent},Async) -> {reply,put(parent,Parent),Async};
proc(Message,#handler{config={F,Req}}=Async) -> {reply,F(Message),Async}.
