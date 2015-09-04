-module(n2o_file).
-author('Andrii Zadoorozhnii').
-include_lib("n2o/include/wf.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

handle_cast(Msg, State) -> {stop, {error, {unknown_cast, Msg}}, State}.
handle_info(_Info, State=#handler{}) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #handler{name=Name,group=Group}) ->
    spawn(fun() -> supervisor:delete_child(Group,Name) end),
    wf:cache({file,Name},undefined),
    ok.

handle_call(#ftp{},_,Proc)       -> { reply,Proc,Proc };
handle_call(Command,_,Proc)      -> { reply,{unknown,Command},Proc }.

init(Handler) ->
    wf:info(?MODULE,"File Transfer ~p started ~p",[Handler,self()]),
    wf:cache({file,Handler#handler.name},self()),
    {ok, Handler}.

% N2O Protocol Handlers

info(#ftp{}=FTP, Req, State) ->
    wf:info(?MODULE,"File Transfer: ~p~n",[FTP]),
    n2o_async:start(#handler{module=?MODULE,group=n2o_sup,name={FTP#ftp.sid,FTP#ftp.filename,FTP#ftp.hash}}),
    {reply,wf:format(FTP#ftp{source=wf:version()}),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
