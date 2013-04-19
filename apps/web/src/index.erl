-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> #template { file= code:priv_dir(web) ++ "/templates/index.html" }.
title() -> "Demo Page".
headline() -> "Demo Page".

body() ->
%  wf:comet_global(fun() -> chat_loop() end, chatroom),
  [
     #span { text="Your chatroom name: " }, 
     #textbox { id=userNameTextBox, text="Anonymous", style="width: 100px;", next=messageTextBox },

     #p{},
     #panel { id=chatHistory, class=chat_history },

     #p{},
     #textbox { id=messageTextBox, style="width: 70%;", next=sendButton },
     #button { id=sendButton, text="Send", postback=chat },

  "<form name='chat' onsubmit='ws.send(document.chat.msg.value); return false;'>
  <input name='msg' type='text'/>
  <input type='submit'/>
  </form>
  <div id='status'></div>" ].

api_event(history_back, tabs_example_tag, Data) -> error_logger:info_msg("API: ~p",[Data]).

event(chat) ->
    error_logger:info_msg("Button Pressed"),
    Username = wf:q(userNameTextBox),
    Message = wf:q(messageTextBox),
    wf:push({message, Username, Message}),
    wf:wire("obj('messageTextBox').focus(); obj('messageTextBox').select();");

event(Event) -> error_logger:info_msg("Event: ~p", [Event]).

push(Event) -> case Event of
        'INIT' ->
            %% The init message is sent to the first process in a comet pool.
            Terms = [
                #p{},
                #span { text="You are the only person in the chat room.", class=message }
            ],
            wf:insert_bottom(chatHistory, Terms),
            wf:flush();

        {message, Username, Message} ->
            %% We got a message, so show it!
            Terms = [
                #p{},
                #span { text=Username, class=username }, ": ",
                #span { text=Message, class=message }
            ],
            wf:insert_bottom(chatHistory, Terms),
            wf:wire("obj('chatHistory').scrollTop = obj('chatHistory').scrollHeight;"),
            wf:flush()
    end.

chat_loop() ->
    receive 
        'INIT' ->
            %% The init message is sent to the first process in a comet pool.
            Terms = [
                #p{},
                #span { text="You are the only person in the chat room.", class=message }
            ],
            wf:insert_bottom(chatHistory, Terms),
            wf:flush();

        {message, Username, Message} ->
            %% We got a message, so show it!
            Terms = [
                #p{},
                #span { text=Username, class=username }, ": ",
                #span { text=Message, class=message }
            ],
            wf:insert_bottom(chatHistory, Terms),
            wf:wire("obj('chatHistory').scrollTop = obj('chatHistory').scrollHeight;"),
            wf:flush()
    end,
    chat_loop().
