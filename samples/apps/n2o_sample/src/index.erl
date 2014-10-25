-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> 
    wf:info(?MODULE,"user: ~p~n",[wf:user()]),
    case wf:user() of
         undefined -> wf:redirect("/login");
         _ -> #dtl{file = "index", app=n2o_sample,bindings=[{body,body()}]} end.

body() ->
    {ok,Pid} = wf:async(fun() -> chat_loop() end), 
    wf:update(logoutButton,#button{id=logout, body="Logout", postback=logout}),
    [ #button  { id=send, body= <<"Chat">>, postback={chat,Pid}, source=[message] } ].

event(login) ->
    login:event(login);

event(terminate) -> 
    wf:info(?MODULE,"event(terminate) called~n",[]);

event(init) -> 
    wf:info(?MODULE,"event(init) called~n",[]),
    User = wf:user(),
    wf:reg(room),
    wf:update(heading,#b{body="User: " ++ User}),
    ok;

event({chat,Pid}) ->
    wf:info(?MODULE,"Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    Message = wf:q(message),
    wf:wire(#jq{target=message,method=[focus,select]}),
    Pid ! {message, Username, wf:js_escape(wf:html_encode(Message))};

event(logout) -> 
    wf:logout(),
    wf:redirect("/login");

event(Event) -> wf:info(?MODULE,"Event: ~p", [Event]).

chat_loop() ->
    receive 
        {message, Username, Message} ->
            wf:insert_top(history, 
              #dtl{file="message",app=n2o_sample,
                   bindings=[{user,Username},{message,Message}]}),
            wf:flush(room);
        Unknown -> wf:info(?MODULE,"Unknown Looper Message ~p",[Unknown])
    end,
    chat_loop().
