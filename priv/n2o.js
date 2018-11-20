
// N2O CORE

var active = false,
    debug = false,
    session = "site-sid",
    protocol = window.location.protocol == 'https:' ? "wss://" : "ws://",
    querystring = window.location.pathname + window.location.search,
    host = window.location.hostname;

function client() { return ''; }
function token()  { return localStorage.getItem("token")  || ''; };
function qi(name) { return document.getElementById(name); }
function qs(name) { return document.querySelector(name); }
function qn(name) { return document.createElement(name); }
function is(x, num, name) { return x == undefined ? false : (x.t == 106 ? false : (x.v.length === num && x.v[0].v === name)); }
function co(name) { match = document.cookie.match(new RegExp(name + '=([^;]+)')); return match ? match[1] : undefined; }

function N2O_start() {
    ws = new bullet(protocol + host + (port==""?"":":"+port) + "/ws" + querystring);
    ws.onmessage = function (evt) { // formatters loop
    for (var i=0;i<protos.length;i++) { p = protos[i]; if (p.on(evt, p.do).status == "ok") return; } };
    ws.onopen = function() { if (!active) { ws.send('N2O,'+token()); console.log('WS Connect'); active=true; } };
    ws.onclose = function() { active = false; console.log('WS Disconnect'); }; next(); }

/// N2O Protocols

var $io = {}; $io.on = function onio(r, cb) {
    if (is(r, 3, 'io')) {
        if (r.v[2].v != undefined && r.v[2].v[1] != undefined &&
            r.v[2].v.length == 2 && (r.v[2].v[0].v == "Token" || r.v[2].v[0].v == "Auth")) {
            localStorage.setItem("token",utf8_arr(r.v[2].v[1].v));
        }
        try { eval(utf8_arr(r.v[1].v));
              if (typeof cb == 'function') cb(r);
              return { status: "ok" };
        } catch (e)  { console.log("Eval error: "+r);
                       return { status: '' }; }
    } else return { status: '' };
}

var $file = {}; $file.on = function onfile(r, cb) {
    if (is(r, 10, 'ftpack')) {
        if (typeof cb == 'function') cb(r); return { status: "ok" };
    } else return { status: '' };
}

// BERT Formatter

var $bert = {}; $bert.protos = [$io, $file]; $bert.on = function onbert(evt, cb) {
    if (Blob.prototype.isPrototypeOf(evt.data) &&
       (evt.data.length > 0 || evt.data.size > 0)) {
        var r = new FileReader();
        r.addEventListener("loadend", function () {
            try {
                erlang = dec(r.result);
                if (typeof cb == 'function') cb(erlang);
                for (var i = 0; i < $bert.protos.length; i++) {
                    p = $bert.protos[i];
                    if (p.on(erlang, p.do).status == "ok") return;
                }
            } catch (e) { console.log(e); }
        });
        r.readAsArrayBuffer(evt.data);
        return { status: "ok" };
    } else return { status: "error", desc: "data" };
}

var protos = [$bert];
