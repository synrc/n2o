-module(action_disable).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile (export_all).

render_action(#disable{target=Target}) -> #script{script=wf:f("$('#~s').disabled = true;", [Target])}.
