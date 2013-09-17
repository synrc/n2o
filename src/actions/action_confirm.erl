-module(action_confirm).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) -> 
    TriggerPath = Record#confirm.trigger,
    TargetPath = Record#confirm.target,
    Delegate = Record#confirm.delegate,
    Ev = #event { postback=Record#confirm.postback, trigger=TriggerPath, target=TargetPath, delegate=Delegate },
    [
        wf:f("if (confirm(\"~s\")) {", [wf:js_escape(Record#confirm.text)]),
        wf_render_actions:render_action(Ev),
        "}"
    ].

