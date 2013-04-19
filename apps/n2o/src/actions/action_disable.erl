% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module (action_disable).
-include_lib ("wf.hrl").
-compile (export_all).

render_action(#disable{target=Target}) ->
	#script{script=wf:f("obj('~s').disabled = true;", [Target])}.
