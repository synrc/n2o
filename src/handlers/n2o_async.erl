-module(n2o_async).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

async(Fun) -> async("comet",Fun).
async(Name, F) ->
    Pid = case global:whereis_name(Name) of
        undefined ->
            Closure = fun(Fun) ->
                R = global:register_name(Name,self()),
                case R of
                    yes -> Fun();
                    _ -> skip end end,
            Req = case wf:context() of
                undefined -> undefined;
                _ -> ?REQ
            end,
            spawn(fun() -> init_context(Req), Closure(F) end);
        Registered -> Registered end,
    {ok,Pid}.

flush(Pool) ->
    Actions = wf_context:actions(),
    wf:actions([]),
    wf:send(Pool,{flush,Actions}).

init_context(undefined) -> [];
init_context(Req) ->
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_context:fold(init, Ctx#cx.handlers, Ctx),
    wf:actions(NewCtx#cx.actions),
    wf:context(NewCtx).

start(Async) ->
    Restart = transient,
    Shutdown = 5000,
    ChildSpec = { Async#handler.name, { ?MODULE, start_link, [Async]},
                  Restart, Shutdown, worker, [Async#handler.module] },
    case supervisor:start_child(Async#handler.group,ChildSpec) of
         {ok,_}   -> {ok,Async#handler.name};
         {ok,_,_} -> {ok,Async#handler.name};
         Else     -> Else end.

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).
code_change(_,State,_) -> {ok, State}.
terminate(_Reason, #handler{name=Name,group=Group,class=Class}) ->
    spawn(fun() -> supervisor:delete_child(Group,Name) end),
    wf:cache({Class,Name},undefined),
    ok.

handle_call(Message,_,#handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_cast(Message,  #handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_info(Message,  #handler{module=Mod}=Async) -> Mod:proc(Message,Async).

pid(Async) -> wf:cache({Async#handler.class,Async#handler.name}).

init(Handler) ->
    wf:info(?MODULE,"Async Handler ~p started ~p",[Handler,self()]),
    wf:cache({Handler#handler.class,Handler#handler.name},self()),
    {ok, (Handler#handler.module):proc(init,Handler)}.
