-module(index).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> 
    case wf:user() of
         undefined -> wf:redirect("/login");
         _ -> #dtl{file = "index", app=n2o_sample,bindings=[{body,body()}]} end.

body() ->
    wf:update(heading,#b{body="User: " ++ wf:user()}),
    wf:update(logoutButton,#button{id=logout, body="Logout", postback=logout}),
    [ #button { id=send, body= <<"Chat">>, postback=chat, source=[message] } ].

event(chat) ->
    User = wf:user(),
    Message = wf:q(message),
    Room = wf:qs(<<"room">>),
    wf:wire(#jq{target=message,method=[focus,select]}),
    kvs:add(#entry{id=kvs:next_id("entry",1),from=wf:user(),feed_id={room,Room},media=Message}),
    wf:send({topic,Room},{client,{User,Message}});

event({client,{User,Message}}) ->
    DTL = #dtl{file="message",app=n2o_sample,
        bindings=[{user,User},{message,wf:js_escape(wf:html_encode(Message))}]},
    wf:insert_top(history, DTL);

event(init) -> 
    Room = wf:qs(<<"room">>),
    wf:reg({topic,Room}),
    [ event({client,{E#entry.from,E#entry.media}}) || E <- 
       lists:reverse(kvs:entries(kvs:get(feed,{room,Room}),entry,10)) ];

event(logout) -> wf:logout(), wf:redirect("/login");
event(Event) -> wf:info(?MODULE,"Event: ~p", [Event]).
