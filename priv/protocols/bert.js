function atom(o) { return { type: "Atom", value: o, toString: function() { return this.value; } }; };
function bin(o) { return { type: "Binary", value: o, toString: function() { return this.value.length > 200 ? "<<["+this.value.length+"]>>" : "<<'"+utf8_decode(this.value)+"'>>"; } }; };
function tuple() { return { type: "Tuple", value: arguments, toString: function() { var s = "";
    for (var i=0;i<this.value.length;i++) { if (s!=="") s+=","; s+=this.value[i]; }
    return "{" + s + "}"; } }; };
function dec(S) { return decode(0,0,new Uint8Array(S).buffer); };
function enc(s) { return new Blob([encodebuf(s).buffer]); };
function encodebuf(s) {
    var ori = encode(s), buf = new Uint8Array(new ArrayBuffer(ori.length));
    for (var i=0; i < buf.length; i++) { buf[i] = ori.charCodeAt(i); }
    return buf; }

var $tab=[['INTE', 97,dint,1],['LINT', 98,dint,4],['FLOA', 99,dflo,0],['ATOM',100,dstr,2],
          ['REFE',101,dnop,0],['PORT',102,dnop,0],['PID0',103,dnop,0],['TUPL',104,drun,1],
          ['TUPL',105,drun,4],['NIL0',106,dnil,4],['STRI',107,dstr,2],['LIST',108,drun,4],
          ['BINA',109,dstr,4],['BIGI',110,dnop,1],['LBIG',111,dnop,4],['NFUN',112,dnop,0],
          ['EXPO',113,dnop,0],['NREF',114,dnop,0],['SATO',115,dnop,0],['MAPS',116,dnop,0],
          ['FUNC',117,dnop,0],['AUT8',118,dnop,0],['SAU8',119,dnop,0]];

function itoa(x) { return String.fromCharCode(x); }
function ltoa(a) { for (var i = 0,s=""; i < a.length; i++) s += itoa(a[i]); return s; };
function itol(Int, Length) {
    var isNegative, OriginalInt, i, Rem, s = "";
    isNegative = (Int < 0);
    if (isNegative) { Int = Int * (0 - 1); }
    OriginalInt = Int;
    for (i = 0; i < Length; i++) {
        Rem = Int % 256;
        if (isNegative) { Rem = 255 - Rem; }
        s = String.fromCharCode(Rem) + s;
        Int = Math.floor(Int / 256); }
    if (Int > 0) { throw ("BERT: Range: " + OriginalInt); }
    return s; };

function encode(o) { return itoa(131) + en_inner(o); };
function en_inner(Obj) { if(Obj === undefined) return 106; var func = 'en_' + typeof(Obj); return eval(func)(Obj); };
function en_string(Obj) { return itoa(107) + itol(Obj.length, 2) + Obj; };
function en_boolean(Obj) { if (Obj) return en_inner(atom("true")); else return en_inner(atom("false")); };
function en_number(Obj) { var isi = (Obj % 1 === 0); if (!isi) { return en_float(Obj); }
    if (isi && Obj >= 0 && Obj < 256) { return itoa(97) + itol(Obj, 1); }
    return itoa(98) + itol(Obj, 4); };
function en_float(Obj) { var s = Obj.toExponential(); while (s.length < 31) { s += 0; } return 99 + s; };
function en_object(Obj) {
    if (Obj.type === "Atom") return en_atom(Obj);
    if (Obj.type === "Binary") return en_bin(Obj);
    if (Obj.type === "Tuple") return en_tuple(Obj);
    if (Obj.constructor.toString().indexOf("Array") !== -1) return en_array(Obj); };
function en_atom(Obj) { return itoa(100) + itol(Obj.value.length, 2) + Obj.value; };
function en_bin(Obj) { return itoa(109) + itol(Obj.value.length, 4) + Obj.value; };
function en_tuple(Obj) {
    var i, s = "";
    if (Obj.value.length < 256) { s += itoa(104) + itol(Obj.value.length, 1); }
    else { s += itoa(105) + itol(Obj.value.length, 4); }
    for (i = 0; i < Obj.value.length; i++) { s += en_inner(Obj.value[i]); }
    return s; };
function en_array(Obj) {
    var i, s = itoa(108) + itol(Obj.length, 4);
    for (i = 0; i < Obj.length; i++) { s += en_inner(Obj[i]); }
    s += itoa(106);
    return s; };

function decode(b,ix,d) { var s = new DataView(d);
    if (s.getUint8(ix++)!==131) throw("BERT"); return din(0,ix,s); };
function din(b,ix,s) { var c=s.getUint8(ix++),r,x=(c>96&&c<120)?$tab[c-97]:['U',0,dnop,0];
    r=x[2](x[3],ix,s); r.type=x[0]; return r; }
function dflo(b,ix,s) { return { value: [], size: ix+31}; }
function dint(b,ix,s) { return { value:b==1?s.getUint8(ix):s.getInt32(ix), size:ix+b }; }
function dnil(b,ix,s) { return { value:[],size:ix+1}; } function dnop(b,ix,s) { return { }; }
function dstr(b,ix,s) {
    var sz = b==2?s.getUint16(ix):s.getUint32(ix),r=[]; ix+=b;
    return { value: utf8_dec(new DataView(s.buffer.slice(ix,ix+sz))), size: ix+sz }; };
function drun(b,ix,s) {
    var sz = b==1?s.getUint8(ix):s.getUint32(ix),r=[]; ix+=b;
    for (var i=0;i<sz;i++) { e=din(0,ix,s); r.push(e); ix=e.size; }
    if (b==4 && s.getUint8(ix++) != 106) throw ("NIL");
    return { value: r, size: ix }; };

function isTUPLE(x,num,name) { return (x.value.length == num && x.value[0].value == name); }
var $io = {}; $io.on = function onio(x, cb) { if (isTUPLE(x,3,'io')) {
    try { console.log(x.value[1].value);
          eval(x.value[1].value);
          if (typeof cb == 'function') cb(x); } catch (e) { console.log(e); return { status: '' }; }
    return { status: "ok" }; } else return { status: '' }; }
var $file = {}; $file.on = function onfile(x, cb) { if (isTUPLE(x,12,'ftp')) {
    if (typeof cb == 'function') cb(x); return { status: "ok" }; } else return { status: ''}; }
var $bin = {}; $bin.on = function onbin(x, cb) { if (isTUPLE(x,2,'bin')) {
    if (typeof cb == 'function') cb(x); return { status: "ok" }; } else return { status: '' }; }

var $bert = {}; $bert.protos = [$io,$bin,$file];
$bert.on = function onbert(evt, cb) {
    if (Blob.prototype.isPrototypeOf(evt.data) && (evt.data.length > 0 || evt.data.size > 0)) {
        var reader = new FileReader();
        reader.addEventListener("loadend", function() {
            try { lastBuf = reader.result;
                  erlang = decode(0,0,reader.result);
                  if (debug) console.log(erlang);
                  if (typeof cb  == 'function') cb(erlang);
                  for (var i=0;i<$bert.protos.length;i++) {
                    p = $bert.protos[i]; if (p.on(erlang, p.do).status == "ok") return; }
            } catch (e) { console.log(e); } });
        reader.readAsArrayBuffer(evt.data);
        return { status: "ok" }; } else return { status: "error", desc: "data" }; }
