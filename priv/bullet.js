
// N2O Transports

$websocket = { heart: true, creator: function(url) { return new window.WebSocket(url); },
               onopen: nop, onmessage: nop, onclose: nop,
               onheartbeat: function() {     this.send('PING'); },
               send:  function(data)   { if (this.channel) this.channel.send(data); },
               close: function()       { if (this.channel) this.channel.close();    } };

// N2O Firestarter

ct = 0;
xport = null;
transports = [ $websocket ];
heartbeat = null;
reconnectDelay = 1000;

function nop() { }
function next() { if (transports.length <= ct) ct = 0; return transports[ct++]; }
function bullet(url) { xport = next(); xport.url = url; return up(); }
function reconnect() { ct++; setTimeout(function() { up(); }, reconnectDelay); }

function up() {
    xport.channel = xport.creator(xport.url);
    xport.channel.onmessage = function(e) { xport.onmessage(e); };
    xport.channel.onopen = function() {
        if (xport.heart) heartbeat = setInterval(function(){xport.onheartbeat();}, 4000);
        xport.onopen(); };
    xport.channel.onclose = function() { xport.onclose(); clearInterval(heartbeat); reconnect(); };
    return xport; }
