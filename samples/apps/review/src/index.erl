-module(index).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
    case wf:user() of
         undefined -> wf:redirect("/login"),#dtl{};
         _ -> #dtl{file = "index", app=review,bindings=[{body,body()},{list,content()}]} end.

room() -> case wf:qs(<<"room">>) of <<>> -> "lobby"; E -> wf:to_list(E) end.
content() -> case wf:qs(<<"code">>) of undefined -> list(); Code -> code() end.
code() -> case wf:qs(<<"code">>) of undefined  -> "NO CODE"; 
                       E -> {ok,Bin} = file:read_file(E), wf:to_list(Bin) end.
list() ->
    Room = room(),
    #ul{body=[ #li{body=#link{body=filename:basename(File),
                              postback={show,filename:basename(File),File}}}
     || File<-filelib:wildcard(code:priv_dir(review)++"/sippets/"++Room++"/*") ]}.

body() ->
    wf:update(heading,#b{body="Review: " ++ room()}),
    wf:update(logoutButton,#button{id=logout, body="Logout " ++ wf:user(), postback=logout}),
    [ #button { id=send, body= <<"Chat">>, postback=chat, source=[message] } ].

event({show,Short,File}) ->
    wf:redirect("/index?room="++Short++"&code="++File);

event(chat) ->
    User = wf:user(),
    Message = wf:q(message),
    Room = room(),
    wf:wire(#jq{target=message,method=[focus,select]}),
    kvs:add(#entry{id=kvs:next_id("entry",1),from=wf:user(),feed_id={room,Room},media=Message}),
    wf:send({topic,Room},{client,{User,Message}});

event({client,{User,Message}}) ->
    DTL = #dtl{file="message",app=review,
        bindings=[{user,User},{message,wf:js_escape(wf:html_encode(Message))}]},
    wf:insert_top(history, DTL);

event(init) ->
    Room = room(),
    wf:reg({topic,Room}),
    [ event({client,{E#entry.from,E#entry.media}}) || E <- 
       lists:reverse(kvs:entries(kvs:get(feed,{room,Room}),entry,10)) ];

event(logout) -> wf:logout(), wf:redirect("/login");
event(Event) -> wf:info(?MODULE,"Event: ~p", [Event]).
