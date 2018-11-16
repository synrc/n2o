
-type(ws_header_key() :: atom() | binary() | string()).
-type(ws_header_val() :: atom() | binary() | string() | integer()).

-record(mqtt_client,
        { client_id     :: binary() | undefined,
          client_pid    :: pid(),
          username      :: binary() | undefined,
          peername      :: {inet:ip_address(), inet:port_number()},
          clean_sess    :: boolean(),
          proto_ver     :: 3 | 4,
          keepalive = 0,
          will_topic    :: undefined | binary(),
          ws_initial_headers :: list({ws_header_key(), ws_header_val()}),
          connected_at  :: erlang:timestamp()
        }).

-type(mqtt_msgid() :: binary() | undefined).
-type(mqtt_pktid() :: 1..16#ffff | undefined).

-record(mqtt_message,
        { id              :: mqtt_msgid(),
          pktid           :: mqtt_pktid(),
          from            :: {binary(), undefined | binary()},
          topic           :: binary(),
          qos     = 0     :: 0 | 1 | 2,
          flags   = []    :: [retain | dup | sys],
          retain  = false :: boolean(),
          dup     = false :: boolean(),
          sys     = false :: boolean(),
          headers = []    :: list(),
          payload         :: binary(),
          timestamp       :: erlang:timestamp()
}).

