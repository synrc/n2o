-module(action_alert).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) -> wf:f("alert(\"~s\");", [wf:js_escape(Record#alert.text)]).
