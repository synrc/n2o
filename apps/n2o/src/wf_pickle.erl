% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_pickle).
-export ([
    pickle/1,
    depickle/1, 
    depickle/2
]).
-include_lib ("wf.hrl").

% Does a plain old term_to_binary...
pickle(Data) ->
    B = term_to_binary({Data, now()}, [compressed]),
    <<Signature:4/binary, _/binary>> = erlang:md5([B, signkey()]),
    _PickledData = modified_base64_encode(<<Signature/binary, B/binary>>).

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
    SignKey = config_handler:get_value(signkey),

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
	<<S:4/binary, B/binary>> = modified_base64_decode(wf:to_binary(PickledData)),
	<<S:4/binary, _/binary>> = erlang:md5([B, signkey()]),
	{_Data, _PickleTime} = binary_to_term(B)
    catch _Type : _Message ->
	undefined
    end.

% modified_base64_encode/1 
%	- Replace '+' and '/' with '-' and '_', respectively. 
% - Strip '='.
modified_base64_encode(B) -> m_b64_e(base64:encode(B), <<>>).
m_b64_e(<<>>, Acc) -> Acc;
m_b64_e(<<$+, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $->>);
m_b64_e(<<$/, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $_>>);
m_b64_e(<<$=, Rest/binary>>, Acc) -> m_b64_e(Rest, Acc);
m_b64_e(<<H,  Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, H>>).

% modified_base64_decode/1 
% - Replace '-' and '_' with '+' and '/', respectively. 
% - Pad with '=' to a multiple of 4 chars.
modified_base64_decode(B) -> base64:decode(m_b64_d(B, <<>>)).
m_b64_d(<<>>, Acc) when size(Acc) rem 4 == 0 -> Acc;
m_b64_d(<<>>, Acc) when size(Acc) rem 4 /= 0 -> m_b64_d(<<>>, <<Acc/binary, $=>>);
m_b64_d(<<$-, Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, $+>>);
m_b64_d(<<$_, Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, $/>>);
m_b64_d(<<H,  Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, H>>).
