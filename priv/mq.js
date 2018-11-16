var match, pl = /\+/g, search = /([^&=]+)=?([^&]*)/g,
    decode_uri = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
    query = window.location.search.substring(1),
    nodes = 4,
    params = {}; while (match = search.exec(query)) params[decode_uri(match[1])] = decode_uri(match[2]);
var l = location.pathname,
    x = l.substring(l.lastIndexOf("/") + 1),
    ll = x.lastIndexOf("."),
    module = x == "" ? "index" : (ll > 0 ? x.substring(0, ll) : x);
var ws = { send: function (payload, qos) {
        var message = new Paho.MQTT.Message(payload);
        message.destinationName = topic("events");
        message.qos = qos || 2;
        mqtt.send(message); } };

var subscribeOptions = {
    qos: 2,  // QoS
    invocationContext: { foo: true },  // Passed to success / failure callback
    onSuccess: function (x) { console.log("MQTT Subscribe"); },
    onFailure: function (m) { console.log("MQTT Subscription failed: " + m.errorMessage); },
    timeout: 2 };

var options = {
    timeout: 2,
    userName: module,
    password: token(),
    cleanSession: false,
    onFailure: function (m) { console.log("MQTT Connection failed: " + m.errorMessage); },
    onSuccess: function ()  { console.log("MQTT Connect");
                              ws.send(enc(tuple(atom('init'),bin(token()))));
                            } };

function gen_client()  { return Math.random().toString(36).substring(2) + (new Date()).getTime().toString(36); }
function pageModule()  { return module || 'api'; }
function client()      { var c = localStorage.getItem("client"), a;
                         if (null == c) { c = 'emqttd_' + gen_client(); }
                         localStorage.setItem("client", c); return c; }
function token()       { return localStorage.getItem("token")  || ''; };
function topic(prefix) { return prefix + "/1/" + rnd() + "/" + pageModule() + "/anon/" + client() + "/" + token(); }
function rnd()         { return Math.floor((Math.random() * nodes)+1); }

  mqtt = new Paho.MQTT.Client(host, 8083, client());
  mqtt.onConnectionLost = function (o) { console.log("connection lost: " + o.errorMessage); };
  mqtt.onMessageArrived = function (m) {
        var BERT = m.payloadBytes.buffer.slice(m.payloadBytes.byteOffset,
            m.payloadBytes.byteOffset + m.payloadBytes.length);
        try {
            erlang = dec(BERT);
            for (var i = 0; i < $bert.protos.length; i++) {
                p = $bert.protos[i]; if (p.on(erlang, p.do).status == "ok") return;
            }
        } catch (e) { console.log(e); }
  };

mqtt.connect(options);

