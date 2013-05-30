-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> 
    case wf:user() of
         undefined -> wf:redirect("login");
         _ -> 
%    Title = "Title",
%    Body = "Body",
             Title = wf:render(title()),
             Body = wf:render(body()),
           [ #dtl{file = "index", bindings=[{title,Title},{body,Body}]} ]
     end.

title() -> [ <<"N2O">> ].

body() -> %% area of http handler
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    wf:wire(#api{name=apiOne,tag=d1}),
  [
    #span { body= <<"Your chatroom name: ">> }, 
    #span { id=userName, body= wf:user() }, #br{}, #button{id=logout,body="Logout",postback=logout}, #br{},
    #panel { id=chatHistory },
    #button{id=but,body= <<"Click Me!">>,postback=change_me},
    #button{id=replace,body= <<"Replace Body">>,postback=replace},
    "<a onclick=\"document.apiOne('Hello')\" name='1'>API</a>",
    #textbox { id=message },
    #button { id=sendButton, body= <<"Chat">>, postback={chat,Pid}, source=[message] },
    #panel { id=n2ostatus }
 ].

api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]), event(change_me).


event(init) ->
  User = wf:user(),
%   error_logger:info_msg("User: ~p",[User]),
  [ begin
          Terms = [ #span { body= User }, ": ",
                      #span { body=integer_to_list(N) }, #br{} ],
            wf:insert_bottom(chatHistory, Terms)
            end || N <- lists:seq(1,3) ];

event(change_me) ->
    wf:replace(but,
        #link{
            url= <<"http://erlang.org">>,
            body= <<"Here's Erlang">>,
            actions=#show{effect=fade}
        }
    );

event(replace) ->
    wf:wire(#redirect{url="hello",nodrop=false});

event(logout) -> wf:user(undefined), wf:redirect("login");

event({chat,Pid}) -> %% area of websocket handler
    error_logger:info_msg("Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    Message = wf:q(message),
%    Terms = [ #span { text= <<"Message sent">> }, #br{} ],
%    wf:insert_bottom(chatHistory, Terms),
    wf:wire("$('#message').focus(); $('#message').select(); "),
    wf:reg(room),
    Pid ! {message, Username, Message};

event(Event) -> error_logger:info_msg("Event: ~p", [Event]).

chat_loop() -> %% background worker ala comet
    receive 
        {message, Username, Message} ->
            Terms = [ #span { body=Username }, ": ",
                      #span { body=Message }, #br{} ],
            wf:insert_bottom(chatHistory, Terms),
            wf:wire("$('#chatHistory').scrollTop = $('#chatHistory').scrollHeight;"),
            wf:flush(room); %% we flush to websocket process by key
        Unknown -> error_logger:info_msg("Unknown Looper Message ~p",[Unknown])
    end,
    chat_loop().
