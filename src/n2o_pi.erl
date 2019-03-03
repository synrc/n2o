-module(n2o_pi).
-description('N2O Process').
-include("n2o.hrl").
-include("n2o_pi.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/1,stop/2,send/2,send/3,pid/2,restart/2]).

start(#pi{table=Tab,name=Name,module=Module,sup=Sup} = Async) ->
    ChildSpec = {{Tab,Name},{?MODULE,start_link,[Async]},
                 transient,5000,worker,[Module]},
    case supervisor:start_child(Sup,ChildSpec) of
               {ok,Pid} -> {Pid,Async#pi.name};
             {ok,Pid,_} -> {Pid,Async#pi.name};
         {error,Reason} -> {error,Reason} end.

stop(Tab,Name) ->
    case n2o_pi:pid(Tab,Name) of
        Pid when is_pid(Pid) ->
                #pi{sup=Sup} = Async = send(Pid,{get}),
                [ supervisor:F(Sup,{Tab,Name})
                  || F <- [ terminate_child , delete_child ] ],
                n2o:cache(Tab,{Tab,Name},undefined),
                Async;
        Data -> {error,{not_pid,Data}} end.

send(Pid,Message) when is_pid(Pid) -> gen_server:call(Pid,Message).
send(Tab,Name,Message) -> gen_server:call(n2o_pi:pid(Tab,Name),Message).

pid(Tab,Name) -> n2o:cache(Tab,{Tab,Name}).

restart(Tab,Name) ->
    case stop(Tab,Name)  of
          #pi{}=Async -> start(Async);
                     Error -> Error end.

start_link (Parameters)    -> gen_server:start_link(?MODULE, Parameters, []).
code_change(_,State,_)     -> {ok, State}.
handle_call({get},_,Async) -> {reply,Async,Async};
handle_call(Message,_,#pi{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_cast(Message,  #pi{module=Mod}=Async) -> Mod:proc(Message,Async).
handle_info(timeout,  #pi{module=Mod}=Async) -> Mod:proc(timeout,Async);
handle_info(Message,  #pi{module=Mod}=Async) ->
    {noreply,case Mod:proc(Message,Async) of
                  {_,_,S} -> S;
                    {_,S} -> S;
                        S -> S end}.

init(#pi{module=Mod,table=Tab,name=Name}=Handler) ->
    n2o:cache(Tab,{Tab,Name},self(),infinity),
    Mod:proc(init,Handler).

terminate(_Reason, #pi{name=Name,sup=Sup,table=Tab}) ->
    spawn(fun() -> supervisor:delete_child(Sup,{Tab,Name}) end),
    io:format("n2o_pi:terminate~n"),
    n2o:cache(Tab,{Tab,Name},undefined), ok.

