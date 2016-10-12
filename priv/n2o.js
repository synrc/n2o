
// N2O CORE

var active      = false,
    debug       = false,
    session     = "site-sid",
    protocol    = window.location.protocol == 'https:' ? "wss://" : "ws://",
    querystring = window.location.pathname + window.location.search,
    host        = null == transition.host ? window.location.hostname : transition.host,
    port        = null == transition.port ? window.location.port : transition.port;

function N2O_start() {
    ws = new bullet(protocol + host + (port==""?"":":"+port) + "/ws" + querystring);
    ws.onmessage = function (evt) { // formatters loop
    for (var i=0;i<protos.length;i++) { p = protos[i]; if (p.on(evt, p.do).status == "ok") return; } };
    ws.onopen = function() { if (!active) { console.log('Connect'); ws.send('N2O,'+transition.pid); active=true; } };
    ws.onclose = function() { active = false; console.log('Disconnect'); }; next(); }

function qi(name) { return document.getElementById(name); }
function qs(name) { return document.querySelector(name);  }
function qn(name) { return document.createElement(name);  }
function is(x,num,name) { return x.t==106?false:(x.v.length === num && x.v[0].v === name); }
function co(name) { match=document.cookie.match(new RegExp(name+'=([^;]+)')); return match?match[1]:undefined; }

/// N2O Protocols

var $io = {}; $io.on = function onio(r, cb) { if (is(r,3,'io')) {
    try { eval(utf8_dec(r.v[1].v)); if (typeof cb == 'function') cb(r); return { status: "ok" }; }
    catch (e) { console.log(e); return { status: '' }; } } else return { status: '' }; }

var $file = {}; $file.on = function onfile(r, cb) { if (is(r,10,'ftp')) {
    if (typeof cb == 'function') cb(r); return { status: "ok" }; } else return { status: ''}; }

var $bin = {}; $bin.on = function onbin(r, cb) { if (is(r,2,'bin')) {
    if (typeof cb == 'function') cb(r); return { status: "ok" }; } else return { status: '' }; }

// BERT Formatter

var $bert = {}; $bert.protos = [$io,$bin,$file]; $bert.on = function onbert(evt, cb) {
    if (Blob.prototype.isPrototypeOf(evt.data) && (evt.data.length > 0 || evt.data.size > 0)) {
        var r = new FileReader();
        r.addEventListener("loadend", function() {
            try { erlang = dec(r.result);
                  if (debug) console.log(JSON.stringify(erlang));
                  if (typeof cb  == 'function') cb(erlang);
                  for (var i=0;i<$bert.protos.length;i++) {
                    p = $bert.protos[i]; if (p.on(erlang, p.do).status == "ok") return; }
            } catch (e) { console.log(e); } });
        r.readAsArrayBuffer(evt.data);
        return { status: "ok" }; } else return { status: "error", desc: "data" }; }

var  protos = [ $bert ];
