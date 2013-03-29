-module(eds).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, accept/1]).
-record(state, {}).
-compile(export_all).
-include("LDAP.hrl").
-include("roster.hrl").
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(_Request, _From, State) -> {reply, ignored, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

init(_Any) -> listen(1389), {ok, #state{}}.

listen(Port) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	spawn(eds,accept,[LSocket]).
	                                     
accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	spawn(fun() -> loop(Socket) end),
	accept(LSocket).

loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			Decoded = asn1rt:decode('LDAP','LDAPMessage',Data),
			io:format("Decoded: ~p~n",[Decoded]),
			case Decoded of
				{ok,{'LDAPMessage',No,Message,Asn}} -> message(No,Message,Socket);
				_Else -> noLDAP
			end,
			loop(Socket);
		E -> io:format("Error: ~p~n",[E]),
			ok
	end.

message(No,Message,Socket) ->
	case Message of
		{bindRequest, {'BindRequest',Type,Uid,Auth}} -> bind(No,Uid,Auth,Socket);
		{abandonRequest,Type} -> abandon(No,Socket);
		{unbindRequest, Null} -> abandon(0,Socket);
		{searchRequest,	{'SearchRequest',SearchDN,Scope,Deref,SizeLimit,
			TimeLimit,TypesOnly,Filter,Attributes}} -> search(No, SearchDN, Scope,Deref,SizeLimit,
			TimeLimit,TypesOnly, Filter,Attributes,Socket);
		_Else -> 
			io:format("Unknown: ~p~n",[Message])
	end.

bind(No,Uid2,Auth,Socket) ->
        io:format("UID: ~p", [Uid2]),
        Uid = case Uid2 of [] -> "maxim"; A -> A end,
	roster:init(Uid),
	Response = #'BindResponse'{resultCode = success, matchedDN = Uid, diagnosticMessage = "OK"},
	answer(Response,No,bindResponse,Socket).

answer(Response,No,ProtocolOp,Socket) ->
	Message = #'LDAPMessage'{messageID = No, protocolOp = {ProtocolOp, Response}},
	{ok, Bytes} = asn1rt:encode('LDAP', 'LDAPMessage', Message),
	io:format("~p~n", [Message]),
	gen_tcp:send(Socket, list_to_binary(Bytes)).

search(No,SearchDN,Scope,Deref,SizeLimit,TimeLimit,TypesOnly,Filter,Attributes,Socket) ->
	roster:traverse(fun({'ContactRecord',CommonName,GivenName,Mail}) -> 
		CN = {'PartialAttribute', "cn", [CommonName]},
		MAIL = {'PartialAttribute', "mail", [Mail]},
		Response = {'SearchResultEntry', CommonName, [CN,MAIL]},
		answer(Response,No,searchResEntry,Socket),
		continue
	end),
	Done = #'LDAPResult'{resultCode = success, matchedDN = SearchDN, diagnosticMessage = "OK"},
	answer(Done,No,searchResDone,Socket).

abandon(No,Socket) ->
        io:format("Abandon: ~p~n",[{No,Socket}]),
	roster:done(), ok.
%	gen_tcp:close(Socket).
