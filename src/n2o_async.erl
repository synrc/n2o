-module(n2o_async).
-description('N2O Async Processes').
-include("n2o.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start(#handler{class=Class,name=Name,module=Module,group=Group} = Async) ->
    ChildSpec = {{Class,Name},{?MODULE,start_link,[Async]},
                 transient,5000,worker,[Module]},
    case supervisor:start_child(Group,ChildSpec) of
               {ok,Pid} -> {Pid,Async#handler.name};
             {ok,Pid,_} -> {Pid,Async#handler.name};
         {error,Reason} -> {error,Reason} end.

stop(Class,Name) ->
    case n2o_async:pid(Class,Name) of
        Pid when is_pid(Pid) ->
                #handler{group=Group} = Async = send(Pid,{get}),
                [ supervisor:F(Group,{Class,Name})
                  || F <- [ terminate_child , delete_child ] ],
                n2o:cache(Class,{Class,Name},undefined),
                Async;
        Data -> {error,{not_pid,Data}} end.

send(Pid,Message) when is_pid(Pid) -> gen_server:call(Pid,Message).
send(Class,Name,Message) -> gen_server:call(n2o_async:pid(Class,Name),Message).

pid(Class,Name) -> n2o:cache(Class,{Class,Name}).

restart(Class,Name) ->
    case stop(Class,Name)  of
          #handler{}=Async -> start(Async);
                     Error -> Error end.

start_link (Parameters)    -> gen_server:start_link(?MODULE, Parameters, []).
code_change(_,State,_)     -> {ok, State}.
handle_call({get},_,Async) -> {reply,Async,Async};
handle_call(Message,_,#handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_cast(Message,  #handler{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_info(timeout,  #handler{module=Mod}=Async) -> Mod:proc(timeout,Async);
handle_info(Message,  #handler{module=Mod}=Async) ->
    {noreply,case Mod:proc(Message,Async) of
                  {_,_,S} -> S;
                    {_,S} -> S;
                        S -> S end}.

init(#handler{module=Mod,class=Class,name=Name}=Handler) ->
    n2o:cache(Class,{Class,Name},self(),infinity),
    Mod:proc(init,Handler).

terminate(_Reason, #handler{name=Name,group=Group,class=Class}) ->
    spawn(fun() -> supervisor:delete_child(Group,{Class,Name}) end),
    n2o:cache(Class,{Class,Name},undefined), ok.
