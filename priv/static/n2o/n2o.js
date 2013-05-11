var msg = 0;
var ws;
var utf8 = {};

function addStatus(text){
    var date = new Date();
    document.getElementById('n2ostatus').innerHTML =
    document.getElementById('n2ostatus').innerHTML + "E> " + text + "<br/>";
}

utf8.toByteArray = function(str) {
    var byteArray = [];
    for (var i = 0; i < str.length; i++)
        if (str.charCodeAt(i) <= 0x7F)
            byteArray.push(str.charCodeAt(i));
        else {
            var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
            for (var j = 0; j < h.length; j++)
                byteArray.push(parseInt(h[j], 16));
        }
    return byteArray;
};

function WebSocketsInit(){
//    if ("MozWebSocket" in window) { WebSocket = MozWebSocket; }
//    if ("WebSocket" in window) {
        ws = new $.bullet("ws://"+window.location.hostname+":"+window.location.port+"/websocket");
        initialized = false;
        ws.onopen = function() { if (!initialized) { ws.send("N2O"); initialized = true; } };
        ws.onmessage = function (evt) {
            msg = evt.data;
            var actions = msg;//Bert.decodebuf(msg);;
            addStatus("Received: '" + actions + "'");
            eval(actions);
        };
        ws.onclose = function() { addStatus("websocket was closed"); };
//    } else {
//        addStatus("sorry, your browser does not support websockets.");
//    }
}

WebSocketsInit();
