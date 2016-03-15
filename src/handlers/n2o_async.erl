-module(n2o_async).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

% n2o_async API

async(Fun) -> async(async,wf:temp_id(),Fun).
async(Name, F) -> async(async,Name,F).
async(Class,Name,F) ->
    Key = key(),
    Handler = #handler{module=?MODULE,class=async,group=n2o,
                       name={Name,Key},config={F,?REQ},state=self()},
    case n2o_async:start(Handler) of
        {error,{already_started,P}} -> init(P,Class,{Name,Key}), {P,{Class,{Name,Key}}};
        {P,X} when is_pid(P)        -> init(P,Class,X),          {P,{Class,X}};
        Else -> Else end.

init(Pid,Class,Name) when is_pid(Pid) -> wf:cache({Class,Name},Pid,infinity), send(Pid,{parent,self()}).
send(Pid,Message) when is_pid(Pid) -> gen_server:call(Pid,Message);
send(Name,Message) -> send(async,{Name,key()},Message).
send(Class,Name,Message) -> gen_server:call(n2o_async:pid({Class,Name}),Message).
pid({Class,Name}) -> wf:cache({Class,Name}).
key() -> n2o_session:session_id().
restart(Name) -> restart(async,{Name,key()}).
restart(Class,Name) ->
    case stop(Class,Name) of #handler{}=Async -> start(Async); Error -> Error end.
flush() -> A=wf:actions(), wf:actions([]), get(parent) ! {flush,A}.
flush(Pool) -> A=wf:actions(), wf:actions([]), wf:send(Pool,{flush,A}).
stop(Name) -> stop(async,{Name,key()}).
stop(Class,Name) ->
    case n2o_async:pid({Class,Name}) of
        Pid when is_pid(Pid) ->
            #handler{group=Group} = Async = send(Pid,{get}),
            [ supervisor:F(Group,{Class,Name})||F<-[terminate_child,delete_child]],
            wf:cache({Class,Name},undefined), Async;
        Data -> {error,{not_pid,Data}} end.
start(#handler{class=Class,name=Name,module=Module,group=Group} = Async) ->
    ChildSpec = {{Class,Name},{?MODULE,start_link,[Async]},transient,5000,worker,[Module]},
    wf:info(?MODULE,"Async Start Attempt ~p~n",[Async#handler{config=[]}]),
    case supervisor:start_child(Group,ChildSpec) of
         {ok,Pid}   -> {Pid,Async#handler.name};
         {ok,Pid,_} -> {Pid,Async#handler.name};
         Else     -> Else end.

init_context(undefined) -> [];
init_context(Req) ->
    Ctx = wf:init_context(Req),
    NewCtx = wf:fold(init, Ctx#cx.handlers, Ctx),
    wf:actions(NewCtx#cx.actions),
    wf:context(NewCtx),
    NewCtx.

% Generic Async Server

init(#handler{module=Mod,class=Class,name=Name}=Handler) -> wf:cache({Class,Name},self(),infinity), Mod:proc(init,Handler).
handle_call({get},_,#handler{module=Mod}=Async)   -> {reply,Async,Async};
handle_call(Message,_,#handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_cast(Message,  #handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_info(timeout,  #handler{module=Mod}=Async) -> Mod:proc(timeout,Async);
handle_info(Message,  #handler{module=Mod}=Async) -> {noreply,element(3,Mod:proc(Message,Async))}.
start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).
code_change(_,State,_) -> {ok, State}.
terminate(_Reason, #handler{name=Name,group=Group,class=Class}) ->
    spawn(fun() -> supervisor:delete_child(Group,{Class,Name}) end),
    wf:cache({Class,Name},undefined), ok.

% wf:async page workers

proc(init,#handler{class=Class,name=Name,config={F,Req},state=Parent}=Async) -> put(parent,Parent), try F(init) catch _:_ -> skip end, init_context(Req), {ok,Async};
proc({parent,Parent},Async) -> {reply,put(parent,Parent),Async#handler{state=Parent}};
proc(Message,#handler{config={F,Req}}=Async) -> {reply,F(Message),Async}.
