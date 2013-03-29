-module(index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file= code:priv_dir(web) ++ "/templates/index.html" }.
title() -> "Demo Page".
headline() -> "Demo Page".

body() ->
  "<form name='chat' onsubmit='ws.send(document.chat.msg.value); return false;'>
  <input name='msg' type='text'/>
  <input type='submit'/>
  </form>
  <div id='status'></div>".

event(Event) -> error_logger:info_msg("Event: ~p", [Event]).
api_event(history_back, tabs_example_tag, Data) -> error_logger:info_msg("API: ~p",[Data]).

