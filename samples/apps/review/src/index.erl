-module(index).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
    case wf:user() of
         undefined -> wf:redirect("login.htm"), #dtl{};
         _ -> #dtl{file = "index", app=review,bindings=[{body,body()},{list,content()}]} end.

room() -> case wf:qp(<<"room">>) of <<>> -> "lobby"; E -> wf:to_list(E) end.
content() -> case wf:qp(<<"code">>) of undefined -> list(); _ -> code() end.
code() -> case wf:qp(<<"code">>) of <<>>  -> "NO CODE"; 
                       E -> {ok,Bin} = file:read_file(E), wf:to_list(Bin) end.
list() ->
    Room = room(),
    #ul{body=[ #li{body=#link{body=filename:basename(File),
                              postback={show,filename:basename(File),File}}}
     || File<-filelib:wildcard(code:priv_dir(review)++"/snippets/"++Room++"/*") ]}.

body() ->
    wf:update(heading,#b{body="Review: " ++ room()}),
    wf:update(logoutButton,#button{id=logout, body="Logout " ++ wf:user(), postback=logout}),
    [ #button { id=send, body= <<"Chat">>, postback=chat, source=[message] } ].

event({show,Short,File}) ->
    wf:redirect("index.htm?room="++Short++"&code="++File);

event(chat) ->
    wf:info(?MODULE,"Chat pressed~n",[]),
    User = wf:user(),
    Message = wf:to_list(wf:q(message)),
    Room = room(),
    kvs:add(#entry{id=kvs:next_id("entry",1),from=wf:user(),feed_id={room,Room},media=Message}),
    wf:send({topic,Room},{client,{User,Message}});

event({client,{User,Message}}) ->
    wf:wire(#jq{target=message,method=[focus,select]}),
    DTL = #dtl{file="message",app=review,
        bindings=[{user,User},{message,wf:html_encode(wf:js_escape(Message))}]},
    wf:insert_top(history, wf:jse(wf:render(DTL)));

event(init) ->
    Room = room(),
    wf:reg({topic,Room}),
    [ event({client,{E#entry.from,E#entry.media}}) || E <-
       lists:reverse(kvs:entries(kvs:get(feed,{room,Room}),entry,10)) ];

event(logout) -> wf:logout(), wf:redirect("login.htm");
event(Event) -> wf:info(?MODULE,"Event: ~p", [Event]).
