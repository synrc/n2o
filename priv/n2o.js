var msg = 0;
var ws;
var utf8 = {};

//WebSocket = undefined; // test XHR fallback

function addStatus(text){
    var date = new Date();
    if (document.getElementById('n2ostatus')) {
        document.getElementById('n2ostatus').innerHTML =
            document.getElementById('n2ostatus').innerHTML + "E> " + text + "<br/>";
    }
}

utf8.toByteArray = function(str) {
    var byteArray = [];
    if (str !== undefined && str !== null)
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
    if ("MozWebSocket" in window) { WebSocket = MozWebSocket; }
    if ("WebSocket" in window) {
        ws = new bullet("ws://"+
          (null == transition.host ? window.location.hostname : transition.host)
               + ":"+ (null == transition.port ? window.location.port : transition.port)
             + "/ws" + window.location.pathname + window.location.search);
        initialized = false;
        ws.onmessage = function (evt) {

            try { // try to parse JSON envelop {eval:"",data:""}

                msg = JSON.parse(evt.data);

                if (typeof handle_web_socket == 'function' && msg.data) { // Data
//                    addStatus("Received: " + bert.decodebuf(msg.data));
                    handle_web_socket(msg.data);
                }

                if (msg.eval) { // Eval
//                    addStatus("Evaluate: " + msg.eval);
                    try{eval(msg.eval);}catch(e){console.log(e); console.log(msg.eval);};
                }

            } catch (ex) { // try to parse known binary formats

                console.log("JSON parsing failed: " + ex);
                console.log("MessageEvent: ");
                console.log(evt.data);

                var reader = new FileReader();
                reader.addEventListener("loadend", function() {

                    try { // BERT encoding

                        var erlang = dec(reader.result);
                        if (typeof handle_web_socket == 'function')
                             handle_web_socket(reader.result);
                        else console.log("Raw BERT Received: " + erlang);

                    } catch (x) { // Unknown Binaries

                        if (typeof handle_web_socket_blob == 'function')
                             handle_web_socket_blob(reader.result);
                        else {
                            if (reader.result.byteLength > 0) {
                                var dataView = new DataView(reader.result);
                                var s = dataView.getInt8(0).toString();
                                for (var i=1;i<reader.result.byteLength;i++)
                                    s = s + "," + dataView.getInt8(i).toString();
                                console.log("Unknown Raw Binary Received: [" + s + "]");
                            }
                        }
                    }

                });
                console.log(evt.data);
                reader.readAsArrayBuffer(evt.data);

            }

        };
        ws.onopen = function() { if (!initialized) { ws.send(['N2O', transition.pid]); initialized = true; } };
        ws.onclose = function() { addStatus("websocket was closed"); };
    } else {
        addStatus("sorry, your browser does not support websockets.");
    }
}

WebSocketsInit();

