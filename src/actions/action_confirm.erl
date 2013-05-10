-module(action_confirm).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) -> 
    TriggerPath = Record#confirm.trigger,
    TargetPath = Record#confirm.target,
    Delegate = Record#confirm.delegate,
    [
        wf:f("if (confirm(\"~s\")) {", [wf:js_escape(Record#confirm.text)]),
        #event { postback=Record#confirm.postback, trigger=TriggerPath, target=TargetPath, delegate=Delegate },
        Record#confirm.actions,
        "}"
    ].

