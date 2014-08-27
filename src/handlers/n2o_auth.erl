-module(n2o_auth).
-author('Andrii Zadorozhnii').
-compile(export_all).

authenticate(Req, undefined) ->
	{false, Req};
authenticate(Req, {Methods, Handler, Opts}) ->
	try_auth_methods(Methods, Req, Handler, Opts).

try_auth_methods([], Req, _, _) ->
	{false, Req};
try_auth_methods([Module|Tail], Req, Handler, Opts) ->
	case Module:authenticate(Req, Handler, Opts) of
		{false, Req2} ->
			try_auth_methods(Tail, Req2, Handler, Opts);
		Authenticated ->
			Authenticated
	end.

methods({Methods, _, Opts}) ->
	[$,|Str] = lists:flatten([[$,, Module:name(Opts)] || Module <- Methods]),
	Str.

realm() -> "visitor".

is_protected(index) -> true;
is_protected(static_file) -> false;
is_protected(_) -> false.
is_authenticated(Module, User) -> false.

authenticate(index, "admin", "admin")       -> true;
authenticate(static_file, _, _)       -> true;
authenticate(Module, User, Password) -> false.
