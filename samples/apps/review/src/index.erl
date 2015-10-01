-module(index).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
    case wf:user() of
         undefined -> wf:redirect("login.htm"), redirect_wait();
         _ -> #dtl{file = "index", app=review,bindings=[{body,body()},{list,list()}]} end.

redirect_wait() -> #dtl{}.
list() -> "<iframe src=http://synrc.com/apps/"++code()++" frameborder=0 width=700 height=1250></iframe>".
code() -> case wf:q(<<"code">>) of undefined  -> "../privacy.htm";
                                    Code -> wf:to_list(wf:depickle(Code)) end.

body() ->
    wf:update(heading,#b{id=heading,body="Review: " ++ code()}),
    wf:update(logout,#button{id=logout, body="Logout " ++ wf:user(), postback=logout}),
    [ #span{id=upload},#button { id=send, body= <<"Chat">>, postback=chat, source=[message] } ].

event(init) ->
    Room = code(),
    wf:update(upload,#upload{id=upload}),
    wf:reg(n2o_session:session_id()),
    wf:reg({topic,Room}),
    Res = wf:async("looper",fun index:loop/1),
    n2o_async:send("looper","waterline"),
    wf:info(?MODULE,"Async Process Created: ~p at Page Pid ~p~n",[Res,self()]),
    [ event({client,{E#entry.from,E#entry.media}}) || E <-
       lists:reverse(kvs:entries(kvs:get(feed,{room,Room}),entry,10)) ];

event(logout) ->
    wf:logout(),
    wf:redirect("login.htm");

event(chat) ->
    wf:info(?MODULE,"Chat pressed~n",[]),
    User = wf:user(),
    Message = wf:q(message),
    Room = code(),
    kvs:add(#entry{id=kvs:next_id("entry",1),from=wf:user(),feed_id={room,Room},media=Message}),
    wf:send({topic,Room},#client{data={User,Message}});

event(#client{data={User,Message}}) ->
    wf:wire(#jq{target=message,method=[focus,select]}),
    HTML = wf:html_encode(wf:to_list(Message)),
    wf:info(?MODULE,"HTML: ~tp~n",[HTML]),
    DTL = #dtl{file="message",app=review,bindings=[{user,User},{color,"gray"},{message,HTML}]},
    wf:insert_top(history, wf:jse(wf:render(DTL)));

event(#bin{data=Data}) ->
    wf:info(?MODULE,"Binary Delivered ~p~n",[Data]),
    #bin{data = "SERVER"};

event(#ftp{}=Data) ->
    wf:info(?MODULE,"FTP Delivered ~p~n",[Data]),
    ok;

event(Event) ->
    wf:info(?MODULE,"Event: ~p", [Event]),
    ok.

loop(M) ->
    DTL = #dtl{file="message",app=review,bindings=[{user,"system"},{message,M},{color,"silver"}]},
    wf:insert_top(history, wf:jse(wf:render(DTL))),
    wf:flush().
