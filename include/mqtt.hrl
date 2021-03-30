-ifndef(MQTT_HRL).
-define(MQTT_HRL, true).

% MQTT listener state
-record(mqcn, { conn :: gen_statem:start_ret()
              , proto = [] :: [] | list()
              }).

% MQTT topics 
% * /:events/:erdpou/:service/:submodule/:node/:vsn/[:client_id] - n2o service listeners
%   :erdpou - service owner
%   :submodule - submodule, subprotocol or page
%   :client_id - optional client id. shrinks reply topic to '/actions/:submodule/:client_id'
%
% * /:actions/:page/:client_id - client listeners: js, etc.

-define(B(L),   n2o:to_binary(L)).
-define(E(P,T), application:get_env(n2o,P,T)).
-define(VSN, "1").

-define(ACT_TOPIC(P),      ?B([?E(action_topic,"/actions"),"/",P])).
-define(ACT_TOPIC(P,Cid),  ?B([?E(action_topic,"/actions"),"/",P,"/",Cid])).

-define(EV_TOPIC(O,S,N),     ?EV_TOPIC(O,S,"+",N)).
-define(EV_TOPIC(O,S,M,N),   ?B([?E(events_topic,"/events"),"/",?B(O),"/",S,"/",M,"/",N,"/",?VSN,"/#"])).

-endif.
