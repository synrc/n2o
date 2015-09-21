-module(n2o_file).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

-define(ROOT, wf:config(n2o,upload,code:priv_dir(n2o))).
-define(next, 1024*1024).
-define(stop, 0).

% N2O Protocols

info(#ftp{sid=Sid,status= <<"init">>, offset=Size, block=B, data=Msg}=FTP, Req, State) ->
    application:set_env(n2o,formatter,bert),

    Dir   = filename:join([?ROOT,wf:to_list(Sid)]),
    File  = filename:join([Dir,FTP#ftp.filename]),
    FSize = case file:read_file_info(File) of {ok, Fi} -> Fi#file_info.size; {error, _} -> 0 end,
    filelib:ensure_dir(Dir),

    wf:info(?MODULE, "File Transfer Init: ~p : size:~p~n",[File, FSize]),
    wf:info(?MODULE, "Size: ~p block:~p==0", [Size, B]),

    case FSize >= Size of
       true  -> {reply, wf:format(FTP#ftp{block=?next, data = <<>>, offset=FSize, status= <<"send">>}), Req,State};
       false -> n2o_async:start(#handler{module=?MODULE,class=file,group=n2o, state=FTP#ftp{block=?next, offset=0},
                             name={FTP#ftp.sid,FTP#ftp.filename,FTP#ftp.hash}}),
                {reply,wf:format(FTP#ftp{offset=0, block=(case B of 0 -> ?stop;_-> ?next end)}),Req,State}
    end;

info(#ftp{sid=Sid,filename=File,hash=Hash,status= <<"send">>}=FTP, Req, State) ->
    wf:info(?MODULE,"FTP:~p",[FTP#ftp {data = <<>> }] ),
    Reply = try gen_server:call(n2o_async:pid({file,{Sid,File,Hash}}),FTP)
          catch E:R -> wf:error(?MODULE, "error call the sync: ~p ~p", [E, R]),
                       FTP#ftp{data= wf:to_binary({E,R}), block=?stop} end,
    wf:info(?MODULE,"reply ~p", [Reply#ftp{data = <<>>}]),
    {reply,wf:format(Reply),Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.

% N2O Handlers

proc(init,Async) ->
    wf:info(?MODULE, "proc init ~p", [Async]),
    {ok, Async};

proc(#ftp{sid=Sid, data=Msg, status= <<"send">>, block=B,filename=Filename}=FTP,
    #handler{state=#ftp{data=State,offset=Offset}}=Async) when erlang:byte_size(Msg) < B ->
    File = filename:join([?ROOT,wf:to_list(Sid),Filename]),
    case file:write_file(File, <<Msg/binary>>, [write,append,raw]) of
            ok -> {stop, normal, FTP#ftp{data= <<"">>,block=?stop}, Async#handler{state=FTP#ftp{block=?stop}}};
   {error, Rw} -> {reply, {error, Rw}, Async} end;

proc(#ftp{sid=Sid,data=Msg, block=B, filename=Filename}=FTP, #handler{state=#ftp{data=State, offset=Offset}}=Async) ->
    File = filename:join([?ROOT,wf:to_list(Sid),Filename]),
    F2 = FTP#ftp{status= <<"send">>, offset=Offset+B, data= <<State/binary, Msg/binary>>},
    wf:info(?MODULE,"send ~p", [F2#ftp{data= <<"">>}]),
    case file:write_file(File, <<Msg/binary>>, [write,append,raw]) of
            ok -> {reply, F2#ftp{data= <<"">>}, Async#handler{state=F2}};
   {error, Rw} -> {reply, {error, Rw}, Async} end.
