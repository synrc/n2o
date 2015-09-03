
// N2O Binary and File Transfer Protocol

function isFTP(x) { return (typeof x == 'object' && x.type == 'Tuple' &&
                    x.value[0].length == 12 && x.value[0][0] == 'ftp'); }

function isBIN(x) { return (typeof x == 'object' && x.type == 'Tuple' &&
                    x.value[0].length == 2 && x.value[0][0] == 'bin'); }

var $file = {};
$file.on = $bert.on;
$file.do = function onbinary(x)
{
    if (isFTP(x)) {
        if (debug) console.log(x);
        console.log("#ftp.source: " + x.value[0][4]);
        return { status: "ok" }
    } else if (isBIN(x)) {
        if (debug) console.log(x);
        console.log("#bin.data: " + x.value[0][1].value);
        return { status: "ok" }
    } else return { status: "error", desc: "not #ftp" };
}
