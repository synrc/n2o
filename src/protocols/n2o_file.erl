-module(n2o_file).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).
%                     1       2      3      4       5       6      7     8      9      10       11
% -record(ftp,     { sid, filename, hash, source, target, offset, meta, data, status, block, priority }).

% temporaries
-define(ROOT, code:priv_dir(n2o)).
-define(next, 1024*1024).
-define(stop, 0).

% N2O Protocols

info(#ftp{status= <<"init">>, offset=Size, block=B, data=Msg}=FTP, Req, State) ->
    application:set_env(n2o,formatter,bert),

    File = filename:join([?ROOT,wf:to_list(FTP#ftp.sid),FTP#ftp.filename]),
    FSize = case file:read_file_info(File) of {ok, Fi} -> Fi#file_info.size; {error, _} -> 0 end,

    wf:info(?MODULE,"File Transfer Init: ~p : size:~p~n",[File, FSize]),
    wf:info(?MODULE, "Size: ~p block:~p==0", [Size, B]),

    if FSize >= Size andalso B==0 ->
      wf:info(?MODULE,"file exist", []),
      {reply, wf:format(FTP#ftp{block=?stop, status=exist}), Req,State};
    true ->
      Id = {file,{FTP#ftp.sid,FTP#ftp.filename,FTP#ftp.hash}},
      Pid = n2o_async:pid({file,{FTP#ftp.sid,FTP#ftp.filename,FTP#ftp.hash}}),
      wf:info(?MODULE, "init async process. Check pid:~p", [Pid]),
      [wf:info(?MODULE, "ch:~p", [X]) || X <- supervisor:which_children(n2o)],

      case supervisor:restart_child(n2o, Id) of {ok, _} -> 
        wf:info(?MODULE,"restarted",[]),
        ok;
        {ok, _,_} -> wf:info(?MODULE,"restarted 2",[]),ok;
        {error, running} ->
          wf:info(?MODULE,"running",[]),ok;
        {error,not_found} ->
          wf:info(?MODULE, "not_found,start new process", []),
        n2o_async:start(#handler{module=?MODULE,class=file,group=n2o, state=FTP#ftp{block=?next, offset=0},
                             name={FTP#ftp.sid,FTP#ftp.filename,FTP#ftp.hash}});

      {error,R} ->
        wf:info(?MODULE,"ERROR STARTING PROCESS ~p", [R])
      end,

      {reply,wf:format(FTP#ftp{source=wf:version(), offset=0, block=(case B of 0 -> ?stop;_-> ?next end)}),Req,State} end;

info(#ftp{sid=Sid,filename=File,hash=Hash,status= <<"send">>}=FTP, Req, State) ->
  wf:info(?MODULE,"FTP:~p",[FTP#ftp {data = <<>> }] ),
  Reply = try gen_server:call(n2o_async:pid({file,{Sid,File,Hash}}),FTP)
          catch E:R ->
            wf:error(?MODULE, "error call the sync: ~p ~p", [E, R]),
            FTP#ftp{data= wf:to_binary({E,R}), block=?stop} end,

    wf:info(?MODULE,"reply ~p", [Reply#ftp{data = <<>>}]),
    {reply,wf:format(Reply),Req, State};


info(Message, Req, State) -> {unknown,Message, Req, State}.

% N2O Handlers

proc(init,Async) -> 
  wf:info(?MODULE, "proc init ~p", [Async]),
  {ok, Async};

proc(#ftp{sid=Sid, status= <<"send">>, data=Msg, offset=Offset, block=B}=FTP,
    #handler{state=#ftp{data=State}}=Async) when erlang:byte_size(Msg) < B ->
    wf:info(?MODULE,"final peace", []),

    File = filename:join([?ROOT,wf:to_list(Sid),FTP#ftp.filename]),

    case filelib:ensure_dir(File) of ok ->
        case file:write_file(File, <<State/binary, Msg/binary>>, [write, raw]) of ok ->
            F2 = FTP#ftp{data= <<"">>,block=?stop},
            wf:info(?MODULE, "~p", [self()]),
  %          n2o_async:stop({FTP#ftp.sid,FTP#ftp.filename,FTP#ftp.hash}),
  %          {reply, F2, Async#handler{state=F2}};
            {stop, normal, F2, Async#handler{state=F2}};

            {error, Rw} -> {reply, {error, Rw}, Async} end; % write failed
        {error, Rd} -> {reply, {error, Rd}, Async} % path failed
    end;

proc(#ftp{data=Msg, block=B}=FTP,#handler{state=#ftp{data=State, offset=Offset}}=Async) ->
    F2 = FTP#ftp{status= <<"send">>,
                 offset=Offset+B,
                 data= <<State/binary, Msg/binary>>},

    wf:info(?MODULE,"send ~p", [F2#ftp{data= <<"">>}]),
    {reply, F2#ftp{data= <<"">>}, Async#handler{state=F2}}.
