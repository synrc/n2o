var match, pl = /\+/g, search = /([^&=]+)=?([^&]*)/g,
    decode_uri = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
    query = window.location.search.substring(1),
    nodes = 4,
    params = {}; while (match = search.exec(query)) params[decode_uri(match[1])] = decode_uri(match[2]);
var l = location.pathname,
    x = l.substring(l.lastIndexOf("/") + 1),
    ll = x.lastIndexOf("."),
    module = x == "" ? "index" : (ll > 0 ? x.substring(0, ll) : x);

function gen_client() { return Math.random().toString(36).substring(2) + (new Date()).getTime().toString(36); }
function client() { var c = localStorage.getItem("client"), a;
                    if (null == c) { c = 'emqttd_' + gen_client(); }
                    localStorage.setItem("client", c); return c; }
function owner() { return '34239034'; }

var mqtt = mqtt || {};

(function(cl,page) {
    function token()      { return localStorage.getItem("token")  || ''; };
    function actions(pre) {
        let cl = '#' || client();
        return pre + "/" + page + "/" + cl;}
    function events(pre,srv)  {
        let o = owner(),
            cl = "" || "/" + client(),
            version = "1",
            s = srv || 'none',
            t = token(),
            tk = t ? "/"+t :'';
        return pre + "/" + o + "/"+ s + "/" + page + "/" + version +"/" + rnd() + cl; }
        
    function rnd()        { return Math.floor((Math.random() * nodes)+1); }
    function base()       { let d = {host: host, ws_port: 8083 },
                            b = sessionStorage.base || JSON.stringify(d);
                            try{return JSON.parse(b);}catch(e){return d;} }
    let c = null,
        b = {},
        opt = {
            timeout: 2,
            userName: page,
            password: token(),
            cleanSession: false,
            onFailure: failc,
            onSuccess: function() {
                console.log("MQTT Connected: ", b.host + ':' + b.ws_port);
                subscribe(actions("/actions")); }
        },
        sopt = {
            timeout: 2,
            qos: 2,
            invocationContext: { foo: true },
            onFailure: fails,
            onSuccess: init
        };

    function fails(m){ console.log("MQTT Subscription error:", m.errorMessage);}
    function failc(m){ console.log("MQTT Disconnected: ", m.errorMessage); } 
    function init(x) { send(enc(tuple(atom('init'), bin(token()))), page); }

    function connect()          { return c && c.connect(opt); }
    function disconnect()       { return c && c.disconnect(); }
    function subscribe(topic)   {
        console.log("MQTT Subscribe:", topic);
        return c && c.subscribe(topic, sopt); 
    }
    function send(payload, service, qos) {
        let s = service || page,
            et = events("/events", s);
        c && c.send(et, payload, qos || 2, false);
    }
    function receive(m) {
        var BERT = m.payloadBytes.buffer.slice(m.payloadBytes.byteOffset,
        m.payloadBytes.byteOffset + m.payloadBytes.length);

        try {
            var erlang = dec(BERT);
            for (var i = 0; i < $bert.protos.length; i++) {
                p = $bert.protos[i]; 
                if (p.on(erlang, p.do).status == "ok") return;
            }
        } catch (e) { console.log(e); }
    }

    function boot() {
        try { disconnect();} catch(e){};
        b = base();
        c = new Paho.MQTT.Client(b.host, b.ws_port, client());
        c.onConnectionLost = failc;
        c.onMessageArrived = receive;
        connect();
    }

    boot();

    mqtt.connect    = connect;
    mqtt.disconnect = disconnect;
    mqtt.send       = send;
    mqtt.subscribe  = subscribe;
    mqtt.reboot     = boot
})(mqtt, module);

var ws = ws || mqtt;
