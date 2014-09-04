-module(index2).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
message(Message) -> wf:js_escape(wf:html_encode(Message)).
main()    -> #dtl{file="index",app=n2o_sample,bindings=[{body,body()}]}.
body() ->
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    [ #panel{id=history}, #textbox{id=message},
      #button{id=send,body="Chat",postback={chat,Pid},source=[message]} ].

event(init) -> wf:reg(room).
event(#ev{payload={chat,Pid}},#context{req=Req}=Cx) -> Pid ! {wf:peer(Req), wf:qp(message,Cx)};
event(Event,_) -> skip.

chat_loop() ->
    receive {Peer, Message} -> 
       wf:insert_bottom(history,#panel{id=history,body=[Peer,": ",Message,#br{}]}),
       wf:flush(room) end, chat_loop().
