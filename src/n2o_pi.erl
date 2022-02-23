-module(n2o_pi).

-description('N2O Process Instance'). % gen_server replacement

-include_lib("n2o/include/n2o.hrl").

-include_lib("n2o/include/n2o_pi.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/1,
         stop/2,
         send/2,
         send/3,
         cast/2,
         cast/3,
         pid/2,
         restart/2]).

start(#pi{table = Tab, name = Name, module = Module,
          sup = Sup, timeout = Timeout, restart = Restart} =
          Async) ->
    ChildSpec = {{Tab, Name},
                 {?MODULE, start_link, [Async]},
                 Restart,
                 Timeout,
                 worker,
                 [Module]},
    case supervisor:start_child(Sup, ChildSpec) of
        {ok, Pid} -> {Pid, Async#pi.name};
        {ok, Pid, _} -> {Pid, Async#pi.name};
        {error, Reason} -> {error, Reason}
    end.

stop(Tab, Name) ->
    case n2o_pi:pid(Tab, Name) of
        Pid when is_pid(Pid) ->
            #pi{sup = Sup} = Async = send(Pid, {get}),
            [supervisor:F(Sup, {Tab, Name})
             || F <- [terminate_child, delete_child]],
            n2o:cache(Tab, {Tab, Name}, undefined),
            Async;
        Data -> {error, {not_pid, Data}}
    end.

send(Pid, Message) when is_pid(Pid) ->
    try gen_server:call(Pid, Message) catch
      exit:{normal, _}:_Z -> {exit, normal};
      _X:_Y:Z -> {error, Z}
    end.

send(Tab, Name, Message) ->
    try gen_server:call(n2o_pi:pid(Tab, Name), Message) catch
      exit:{normal, _}:_Z -> {exit, normal};
      _X:_Y:Z -> {error, Z}
    end.

cast(Pid, Message) when is_pid(Pid) ->
    gen_server:cast(Pid, Message).

cast(Tab, Name, Message) ->
    gen_server:cast(n2o_pi:pid(Tab, Name), Message).

pid(Tab, Name) -> n2o:cache(Tab, {Tab, Name}).

restart(Tab, Name) ->
    case stop(Tab, Name) of
        #pi{} = Async -> start(Async);
        Error -> Error
    end.

handle(Mod, Message, Async) ->
    case Mod:proc(Message, Async) of
        {ok, S} -> {ok, S};
        {ok, S, T} -> {ok, S, T};
        {stop, X, Y, S} -> {stop, X, Y, S};
        {stop, X, S} -> {stop, X, S};
        {stop, S} -> {stop, S};
        {reply, X, S, T} -> {reply, X, S, T};
        {reply, X, S} -> {reply, X, S};
        {noreply, X, S} -> {noreply, X, S};
        {noreply, S} -> {noreply, S};
        {_, S} -> {noreply, S};
        S -> {noreply, S}
    end.

start_link(Parameters) ->
    gen_server:start_link(?MODULE, Parameters, []).

code_change(_, State, _) -> {ok, State}.

handle_call({get}, _, Async) -> {reply, Async, Async};
handle_call(_, _, #pi{module = undefined}) ->
    {noreply, []};
handle_call(Message, _, #pi{module = Mod} = Async) ->
    handle(Mod, Message, Async).

handle_cast(_, #pi{module = undefined}) ->
    {noreply, []};
handle_cast(Message, #pi{module = Mod} = Async) ->
    handle(Mod, Message, Async).

handle_info(timeout, #pi{module = undefined}) ->
    {noreply, []};
handle_info(timeout, #pi{module = Mod} = Async) ->
    handle(Mod, timeout, Async);
handle_info(_, #pi{module = undefined}) ->
    {noreply, []};
handle_info(Message, #pi{module = Mod} = Async) ->
    handle(Mod, Message, Async);
handle_info(_, _) -> {noreply, []}.

init(#pi{module = Mod, table = Tab, name = Name} =
         Handler) ->
    n2o:cache(Tab, {Tab, Name}, self(), infinity),
    Mod:proc(init, Handler).

terminate(Reason,
          #pi{name = Name, sup = Sup, table = Tab, module = Mod} = Pi) ->
    case erlang:function_exported(Mod, terminate, 2) of true -> Mod:terminate(Reason, Pi); false -> false end,
    spawn(fun () ->
                  supervisor:delete_child(Sup, {Tab, Name})
          end),
    catch n2o:cache(Tab, {Tab, Name}, undefined),
    ok.
