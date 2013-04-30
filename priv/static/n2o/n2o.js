var msg = 0;
var ws;

function addStatus(text){
    var date = new Date();
    document.getElementById('status').innerHTML =
    document.getElementById('status').innerHTML + "E> " + text + "<br/>";
}

function WebSocketsInit(){
    if ("MozWebSocket" in window) { WebSocket = MozWebSocket; }
    if ("WebSocket" in window) {
        ws = new WebSocket("ws://"+window.location.hostname+":"+window.location.port+"/websocket");
        ws.binaryType = 'arraybuffer';
        ws.onopen = function() { ws.send("N2O HELO!"); };
        ws.onmessage = function (evt) {
            msg = evt.data;
            var actions = Bert.decodebuf(msg);;
            addStatus("Received: '" + actions + "'");
            eval(actions);
        };
        ws.onclose = function() { addStatus("websocket was closed"); };
    } else {
        addStatus("sorry, your browser does not support websockets.");
    }
}

WebSocketsInit();
