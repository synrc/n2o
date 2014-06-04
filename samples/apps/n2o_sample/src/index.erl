-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> 
    case wf:user() of
         undefined -> wf:redirect("/login"), #dtl{file="index",app=n2o_sample,bindings=[{title,""},{body,""}]};
         _ -> #dtl{file = "index", app=n2o_sample,bindings=[{title,title()},{body,body()}]}
     end.

title() -> [ <<"N2O">> ].

body() ->
    wf:info("RENDER!"),
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    [ #span{ body = wf:f("'/index?x=' is ~p",[wf:qs(<<"x">>)]) },
      #panel{ id=history },
      #textbox{ id=message },
      #button{ id=send, body= <<"Chat">>, postback={chat,Pid}, source=[message] } ].

event(init) ->
    User = wf:user(),
    wf:reg(room),
    X = wf:qs(<<"x">>),
    wf:insert_top(history,
        #panel{id=banner, body= 
            [#span{id=text, body = wf:f("User ~s logged in. X = ~p", [User,X]) },
             #button{id=logout, body="Logout", postback=logout}, 
             #br{}]}),
    wf:insert_top(history,"-1-");

event({chat,Pid}) ->
    wf:info("Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    wf:info(?MODULE,"User: ~p",[Username]),
    Message = wf:q(message),
    wf:update(banner,#panel{id=banner,body=[
        #panel{id=label,body=["Last Message: ",Message]},
        #button{id=logout, body="Logout", postback=logout}
        ]}),
    wf:wire(#jq{target=message,method=[focus,select]}),
    Pid ! {message, Username, wf:js_escape(Message)};

event(logout) -> 
    wf:logout(),
    <<"/ws/",X,_/binary>> = wf:path(?REQ),
    case X of
        $i -> wf:redirect("/login");
        $l -> wf:redirect("/login");
         _ -> wf:redirect("/static/spa/spa.htm") end;

event(login) -> login:event(login);
event(continue) -> wf:info("OK Pressed");
event(Event) -> wf:info("Event: ~p", [Event]).

chat_loop() ->
    receive 
        {message, Username, Message} ->
            Terms = #panel { body= [ Username,": ",Message,#br{} ] },
            wf:insert_bottom(history, Terms),
            wf:wire("document.querySelector('#history').scrollTop = document.querySelector('#history').scrollHeight;"),
            wf:flush(room);
        Unknown -> wf:info("Unknown Looper Message ~p",[Unknown])
    end,
    chat_loop().
