
// N2O CORE

var active      = false,
    protocol    = window.location.protocol == 'https:' ? "wss://" : "ws://",
    querystring = window.location.pathname + window.location.search,
    host        = null == transition.host ? window.location.hostname : transition.host,
    port        = null == transition.port ? window.location.port : transition.port,
    protos      = [ $client, $binary, $bert ];

function N2O_start() {
  ws = new bullet(protocol + host + ":" + port + "/ws" + querystring);
  ws.onmessage = function (evt) { for (var i=0;i<protos.length;i++) { p = protos[i]; if (p.on(evt, p.do).status == "ok") return; } };
  ws.onopen = function() { if (!active) { console.log('Connect'); ws.send(['N2O', transition.pid]); active=true; } };
  ws.ondisconnect = function() { active = false; console.log('Disconnect'); };
}

//WebSocket = undefined; // test XHR fallback

function qi(name) { return document.getElementById(name); }
function qs(name) { return document.querySelector(name);  }
function qn(name) { return document.createElement(name);  }
