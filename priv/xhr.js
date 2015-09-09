
// N2O XHR Fallback

// WebSocket = undefined; // to test

$xhr = { heart: false, interval: 100, creator: function(url) { $conn.url = xhr_url(url);
         $xhr.channel = { send: xhr_send, close: xhr_close }; $conn.onopen();
         return $xhr.channel; }, onheartbeat: function() { xhr('POST',{});} };

transports = [$ws,$xhr];

function xhr_header(request) { request.setRequestHeader('Content-Type','application/x-www-form-urlencoded; charset=utf-8'); }
function xhr_url(url) { return url.replace('ws:', 'http:').replace('wss:', 'https:'); }
function xhr_close() { $conn.onclose(); clearInterval(heartbeat); }
function xhr_send(data) { return xhr('POST',data); }
function xhr_receive(data) { if (data.length != 0) $conn.onmessage({'data':data}); }
function xhr(method,data) {
    var request = new XMLHttpRequest();
    request.open(method,$conn.url,true);
    xhr_header(request);
    request.onload = function() { console.log(request.response); xhr_receive(request.response); };
    request.send(data);
    return true; }
