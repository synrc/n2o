-ifndef(MQTT_HRL).
-define(MQTT_HRL, true).

% MQTT listener state
-record(mqcn, { conn :: gen_statem:start_ret()
              , proto = [] :: [] | list()
              }).

% MQTT topics 
% * /:events/:vsn/:service/:node/:page - n2o service listeners
% * /:actions/:vsn/:page/:client_id - client listeners: js, etc.

-define(B(L),   iolist_to_binary(L)).
-define(E(P,T), application:get_env(n2o,P,T)).
-define(VSN, "1").

-define(ACT_TOPIC(P,Cid),   ?B([?E(action_topic,"/actions"),"/",?VSN,"/",P,"/",Cid])).
-define(EV_TOPIC(S,N),      ?EV_TOPIC(S,N,"#")).
-define(EV_TOPIC(S,N,P),    ?B([?E(events_topic,"/events"),"/",?VSN,"/",S,"/",N,"/",P])).

-endif.
