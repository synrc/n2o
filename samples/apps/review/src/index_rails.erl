-module(index_rails).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

% please use this file with {event,rails} in sys.config for n2o

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
main()    -> #dtl{file="index",app=n2o_sample,bindings=[{body,body()}]}.
body() ->
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    [ #panel{id=history}, #textbox{id=message},
      #button{id=send,body="Chat",postback={chat,Pid},source=[message]} ].

event(init) -> wf:reg(room).
event(#ev{msg={chat,Pid}},#cx{req=Req}=Cx) -> Pid ! {peer(), wf:qp(message,Cx)};
event(Event,_) -> skip.

chat_loop() ->
    receive {Peer, Message} -> 
       wf:insert_bottom(history,#panel{id=history,body=[Peer,": ",Message,#br{}]}),
       wf:flush(room) end, chat_loop().
