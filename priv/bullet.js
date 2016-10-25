
// WebSocket Transport

$ws = { heart: true, interval: 4000,
        creator: function(url) { return window.WebSocket ? new window.WebSocket(url) : false; },
        onheartbeat: function() { this.channel.send('PING'); } };

// N2O Reliable Connection

$conn = { onopen: nop, onmessage: nop, onclose: nop, onconnect: nop,
          send:  function(data)   { if (this.port.channel) this.port.channel.send(data); },
          close: function()       { if (this.port.channel) this.port.channel.close(); } };

ct = 0;
transports = [ $ws ];
heartbeat = null;
reconnectDelay = 1000;
maxReconnects = 100;

function nop() { }
function bullet(url) { $conn.url = url; return $conn; }
function xport() { return maxReconnects <= ct ? false : transports[ct++ % transports.length]; }
function reconnect() { setTimeout(function() { connect(); }, reconnectDelay); }
function next() { $conn.port = xport(); return $conn.port ? connect() : false; }
function connect() {
    $conn.port.channel = $conn.port.creator($conn.url);
    if (!$conn.port.channel) return next();
    $conn.port.channel.onmessage = function(e) { $conn.onmessage(e); };
    $conn.port.channel.onopen = function() {
        if ($conn.port.heart) heartbeat = setInterval(function(){$conn.port.onheartbeat();}, $conn.port.interval);
        $conn.onopen();
        $conn.onconnect(); };
    $conn.port.channel.onclose = function() { $conn.onclose(); clearInterval(heartbeat); reconnect(); };
    return $conn; }
