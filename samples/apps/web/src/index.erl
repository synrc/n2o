-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> 
    case wf:user() of
         undefined -> wf:redirect("/login");
         _ -> #dtl{file = "index", bindings=[{title,title()},{body,body()}]}
     end.

title() -> [ <<"N2O">> ].

body() ->
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    [ #span{ body = io_lib:format("'/index?x=' is ~p",[wf:qs(<<"x">>)]) },
      #panel{ id=history },
      #textbox{ id=message },
      #button{ id=send, body= <<"Chat">>, postback={chat,Pid}, source=[message] } ].

event(init) ->
    User = wf:user(),
    wf:insert_bottom(history, [ #span{ body = io_lib:format("User ~p logged in.", [User]) },
                                #button{id=logout, body="Logout", postback=logout}, #br{} ]);

event(logout) -> wf:user(undefined), wf:redirect("/login");
event(login) -> login:event(login);

event({chat,Pid}) ->
    error_logger:info_msg("Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    Message = wf:q(message),
    wf:wire("$('#message').focus(); $('#message').select(); "),
    wf:reg(room),
    Pid ! {message, Username, Message};

event(Event) -> error_logger:info_msg("Event: ~p", [Event]).

chat_loop() ->
    receive 
        {message, Username, Message} ->
            Terms = [ #span { body=Username }, ": ", #span { body=Message }, #br{} ],
            wf:insert_bottom(history, Terms),
            wf:wire("$('#chatHistory').scrollTop = $('#chatHistory').scrollHeight;"),
            wf:flush(room);
        Unknown -> error_logger:info_msg("Unknown Looper Message ~p",[Unknown])
    end,
    chat_loop().
