var bert = require('../priv/protocols/bert.js');
var utf8 = require('../priv/utf8.js');
var fs = require('fs');

utf8_dec = utf8.dec;
utf8_toByteArray = utf8.enc;

print = function (x) { return "["+Array.apply([], x ).join(",")+"]"; }
pass = true;
counter = 0;

fs.readFileSync('bert.data').toString().split('\n').forEach(function (data) {
    if (data == "") return;
    pass = pass && (data==print(bert.enc(bert.dec(new Uint8Array(JSON.parse(data)).buffer))));
    if (pass) { console.log("OK: "+counter); }
    counter+=1;
    if (!pass) {
        console.log(data);
        console.log(print(bert.enc(bert.dec(new Uint8Array(JSON.parse(data)).buffer))));
        console.log(bert.dec(new Uint8Array(JSON.parse(data)).buffer));
        console.log("ERROR: "+data);
    }
});
