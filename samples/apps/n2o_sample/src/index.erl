-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

-define(MIN_TEMP, user1).
-define(MAX_TEMP, user2).

main() -> 
    case wf:user() of
         undefined -> wf:redirect("/login"), #dtl{file="index",app=n2o_sample,bindings=[{title,""},{body,""}]};
         _ -> #dtl{file = "index", app=n2o_sample,bindings=[{title,title()},{body,body()}]}
     end.

title() -> [ <<"N2O">> ].

log_modules() -> [index].

body() ->
    wf:info(?MODULE,"RENDER!", []),
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    [ #span{ body = wf:f("'/index?x=' is ~p",[wf:qs(<<"x">>)]) },
      #panel{ id=history },
      #textbox{ id=message },
      #button{ id=send, body= <<"Chat">>, postback={chat,Pid}, source=[message] },
      #button{ id=bin, body= <<"Binary">>, postback={binary,{raw,Pid}} },
      #button{ id=h_bin, body= <<"Binary with header">>, postback={binary,{headered,Pid}} },
      #button{ id=bin_text, body= <<"Send text from browser">>, postback={send_text, "from browser test"} } ].

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
    wf:info(?MODULE,"Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    wf:info(?MODULE,"User: ~p",[Username]),
    Message = wf:q(message),
    wf:update(banner,#panel{id=banner,body=[
        #panel{id=label,body=["Last Message: ",Message]},
        #button{id=logout, body="Logout", postback=logout}
        ]}),
    wf:wire(#jq{target=message,method=[focus,select]}),
    Pid ! {message, Username, wf:js_escape(wf:html_encode(Message))};

event(logout) -> 
    wf:logout(),
    <<"/ws/",X,_/binary>> = wf:path(?REQ),
    case X of
        $i -> wf:redirect("/login");
        $l -> wf:redirect("/login");
         _ -> wf:redirect("/static/spa/spa.htm") end;

event(login) -> login:event(login);
event(continue) -> wf:info(?MODULE,"OK Pressed");
event({send_text, Text}) -> 
    wf:info(?MODULE,"Send text: ~p",[binary_from_browser]),
    wf:wire("ws.send(enc(tuple(atom('binary'),'" ++ Text ++ "')));");
event({binary, {raw,Data}}) ->
    wf:info(?MODULE,"Binary: ~p",[Data]),
    <<"just raw binary">>;
event({binary, {headered,Data}}) ->
    wf:info(?MODULE,"Binary with header: ~p",[Data]),
    #binary{ id = 4294967295, type = 255, app = 255, version = 255, from = 4294967295, to = 4294967295,
        ?MIN_TEMP = -340277175.0000001, ?MAX_TEMP = 43857.40000004364,
        meta = <<"meta">>, data = <<"DAT">> };
event({binary, Data}) -> 
    wf:info(?MODULE,"Anybody binary: ~p",[Data]),
    Data;
event(Event) -> wf:info(?MODULE,"Event: ~p", [Event]).
chat_loop() ->
    receive 
        {message, Username, Message} ->
            Terms = #panel { body= [ Username,": ",Message,#br{} ] },
            wf:insert_bottom(history, Terms),
            wf:wire("document.querySelector('#history').scrollTop = document.querySelector('#history').scrollHeight;"),
            wf:flush(room);
        Unknown -> wf:info(?MODULE,"Unknown Looper Message ~p",[Unknown])
    end,
    chat_loop().
