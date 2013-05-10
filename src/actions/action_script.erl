-module(action_script).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) -> 
    Record#script.script.

