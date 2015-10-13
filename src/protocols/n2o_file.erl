-module(n2o_file).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

-define(ROOT, wf:config(n2o,upload,code:priv_dir(n2o))).
-define(NEXT, 256*1024). % 256K chunks for best 25MB/s speed
-define(STOP, 0).

% Callbacks

filename(_Root,Sid,FileName) -> filename:join(wf:to_list(Sid),FileName).

% N2O Protocols

info(#ftp{status={event,_}}=FTP, Req, State) ->
    wf:info(?MODULE,"Event Message: ~p",[FTP#ftp{data= <<>>}]),
    Module=State#cx.module,
    Reply=try Module:event(FTP)
          catch E:R -> Error=wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
    {reply,wf:format({io,n2o_nitrogen:render_actions(wf:actions()),Reply}),Req,State};

info(#ftp{sid=Sid,filename=FileName,status= <<"init">>,block=Block,offset=Offset,size=TotalSize}=FTP,Req,State) ->
    application:set_env(n2o,formatter,bert),
    Root=?ROOT,
    RelPath=(wf:config(n2o,filename,n2o_file)):filename(Root,Sid,FileName),
    FilePath=filename:join(Root,RelPath),
    filelib:ensure_dir(FilePath),
    FileSize=case file:read_file_info(FilePath) of {ok,Fi} -> Fi#file_info.size; {error,_} -> 0 end,
    
    wf:info(?MODULE,"Info Init: ~p Offset: ~p Block: ~p~n",[FilePath,FileSize,Block]),
    
    Hash=wf:hex_encode(crypto:rand_bytes(16)),
    Block2=case Block of 0 -> ?STOP; _ -> ?NEXT end,
    Offset2=case FileSize >= Offset of true -> FileSize; false -> 0 end,
    FTP2=FTP#ftp{hash=Hash,block=Block2,offset=Offset2,filename=RelPath,data= <<>>},

    n2o_async:stop(file,{Sid,Hash}),
    n2o_async:start(#handler{module=?MODULE,class=file,group=n2o,state=FTP2,name={Sid,Hash}}),

    {reply,wf:format(FTP2),Req,State};

info(#ftp{sid=Sid,hash=Hash,status= <<"send">>}=FTP,Req,State) ->
    wf:info(?MODULE,"Info Send:~p ~p",[FTP#ftp{data= <<>>}]),
    Reply=try gen_server:call(n2o_async:pid(file,{Sid,Hash}),FTP)
        catch E:R -> wf:error(?MODULE,"Info Error call the sync: ~p~n",[FTP#ftp{data= <<>>}]),
            FTP#ftp{data= <<>>,block=?STOP} end,
    wf:info(?MODULE,"reply ~p",[Reply#ftp{data= <<>>}]),
    {reply,wf:format(Reply),Req,State};

info(#ftp{status= <<"recv">>}=FTP,Req,State) -> {reply,wf:format(FTP),Req,State};

info(#ftp{status= <<"relay">>}=FTP,Req,State) -> {reply,wf:format(FTP),Req, State};

info(Message,Req,State) -> wf:info(?MODULE, "Info Unknown message: ~p",[Message]),
    {unknown,Message,Req,State}.

% N2O Handlers

proc(init,#handler{state=#ftp{sid=Sid}=FTP}=Async) ->
    wf:info(?MODULE,"Proc Init: ~p",[FTP#ftp{data= <<>>}]),
    wf:send(Sid,FTP#ftp{data= <<>>,status={event,init}}),
    {ok,Async};

proc(#ftp{sid=Sid,hash=Hash,data=Data,status= <<"send">>,block=Block}=FTP,
     #handler{state=#ftp{size=TotalSize,offset=Offset,filename=RelPath}}=Async) when Offset+Block >= TotalSize ->
	wf:info(?MODULE,"Proc Stop ~p, last piece size: ~p", [FTP#ftp{data= <<>>},byte_size(Data)]),
	case file:write_file(filename:join(?ROOT,RelPath),<<Data/binary>>,[append,raw,binary]) of
		{error,Reason} -> {reply,{error,Reason},Async};
		ok ->
            FTP2=FTP#ftp{data= <<>>,block=?STOP},
            wf:send(Sid,FTP2#ftp{status={event,stop},filename=RelPath}),
			spawn(fun() -> supervisor:delete_child(n2o,{file,{Sid,Hash}}) end),
			{stop,normal,FTP2,Async#handler{state=FTP2#ftp{filename=RelPath}}} end;

proc(#ftp{sid=Sid,data=Data,block=Block}=FTP,#handler{state=#ftp{offset=Offset,filename=RelPath}}=Async) ->
    FTP2=FTP#ftp{status= <<"send">>,offset=Offset+Block },
    wf:info(?MODULE,"Proc Process ~p",[FTP2#ftp{data= <<>>}]),
    case file:write_file(filename:join(?ROOT,RelPath),<<Data/binary>>,[append,raw,binary]) of
        {error,Reason} -> {reply,{error,Reason},Async};
        ok -> {reply,FTP2#ftp{data= <<>>},Async#handler{state=FTP2#ftp{filename=RelPath}}} end.