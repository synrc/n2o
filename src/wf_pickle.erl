-module(wf_pickle).
-author('Rusty Klophaus').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

pickle(Data) ->
%    error_logger:info_msg("Pickling: ~p",[Data]),
    B = term_to_binary({Data, now()}, [compressed]),
%    B = term_to_binary({Data, now()}, [compressed]),
    <<Signature:4/binary, _/binary>> = B, %erlang:md5([B, signkey()]),
    _PickledData = base64:encode(<<Signature/binary, B/binary>>),
%    _PickledData = <<Signature/binary, B/binary>>,
%    error_logger:info_msg("Pickled: ~p",[now()]),
    _PickledData.

depickle(PickledData) ->
    try
	{Data, _PickleTime} = inner_depickle(PickledData),
	Data		
    catch _Type : _Message ->
	undefined
    end.

depickle(PickledData, TTLSeconds) ->
    try
	{Data, PickledTime} = inner_depickle(PickledData),
	AgeInSeconds = timer:now_diff(now(), PickledTime) / 1024 / 1024,
	case AgeInSeconds < TTLSeconds of
	    true -> Data;
	    false -> undefined
	end
    catch _Type : _Message ->
	undefined
    end.


%%% PRIVATE FUNCTIONS

signkey() ->
    % Read the signkey from config.
    SignKey = "1",%config_handler:get_value(signkey),

    % Return the signkey, or if it's not found, log an error and then
    % use a signkey based on the cookie.
    case SignKey /= undefined of 
        true  -> 
            SignKey;
        false -> 
            Cookie = erlang:get_cookie(),
            erlang:md5(wf:to_list(Cookie))
    end.


inner_depickle(PickledData) ->
    try
	<<S:4/binary, B/binary>> = base64:decode(wf:to_binary(PickledData)),
%	<<S:4/binary, B/binary>> = wf:to_binary(PickledData),
	<<S:4/binary, _/binary>> = B, %erlang:md5([B, signkey()]),
	{_Data, _PickleTime} = binary_to_term(B)
    catch _Type : _Message ->
	undefined
    end.