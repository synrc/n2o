
// API

function tuple() { return { t: 104, v: Array.apply(null, arguments) }; }
function list() { return { t: 108, v: Array.apply(null, arguments) }; }
function map() { return { t: 116, v: Array.apply(null, arguments) }; }
function atom(o) { return { t: 100, v: utf8_enc(o) }; }
function string(o) { return { t: 107, v: utf8_enc(o) }; }
function float(o) { return { t: 70, v: o }; }
function number(o) {
  var s, isInteger = (o % 1 === 0);
  if (isInteger && o >= 0 && o < 256) { return { t: 97, v: o };  }
  if (isInteger && o >= -134217728 && o <= 134217727) { return {t: 98, v: o}; }
  return {t: 110, v: o}; }
function bin(o) {
  return { t: 109, v: o instanceof ArrayBuffer ? new Uint8Array(o) :
                      o instanceof Uint8Array  ? o : utf8_enc(o) }; }

// ENCODER

function enc(o) { return fl([131, ein(o)]); }
function ein(o) { return Array.isArray(o) ? en_108({ t: 108, v: o }) : eval('en_' + o.t)(o); }
function en_undefined(o) { return [106]; }
function en_70(o) {
  var x = Array(8).fill(0).flat();
  write_Float(x,o.v,0,false,52,8);
  return [70].concat(x);
}
function en_97(o) { return [97, o.v]; }
function en_98(o) { return [98, o.v >>> 24, (o.v >>> 16) & 255, (o.v >>> 8) & 255, o.v & 255]; }
function en_99(o) {
  var obj = o.v.toExponential(20),
      match = /([^e]+)(e[+-])(\d+)/.exec(obj),
      exponentialPart = match[3].length == 1 ? "0" + match[3] : match[3],
      num = Array.from(bin(match[1] + match[2] + exponentialPart).v);
  return [o.t].concat(num).concat(Array(31 - num.length).fill(0).flat());
}
function en_106(o) { return [106]; }
function en_115(o) { return [115, o.v.length, ar(o)]; }
function en_119(o) { return [119, ar(o).length, ar(o)]; }
function en_118(o) { return [118, ar(o).length >>> 8, ar(o).length & 255, ar(o)]; }
function en_100(o) { return [100, o.v.length >>> 8, o.v.length & 255, ar(o)]; }
function en_107(o) { return [107, o.v.length >>> 8, o.v.length & 255, ar(o)]; }
function en_104(o) {
  var l = o.v.length, r = [];
  for (var i = 0; i < l; i++)r[i] = ein(o.v[i]);
  return [104, l, r];
}
function en_109(o) {
  var l = o.v instanceof ArrayBuffer ? o.v.byteLength : o.v.length;
  return [109, l >>> 24, (l >>> 16) & 255, (l >>> 8) & 255, l & 255, ar(o)];
}
function en_108(o) {
  var l = o.v.length, r = [];
  for (var i = 0; i < l; i++)r.push(ein(o.v[i]));
  return o.v.length == 0 ? [106] :
         [108, l >>> 24, (l >>> 16) & 255, (l >>> 8) & 255, l & 255, r, 106];
}
function en_116(o) {
  var l=o.v.length,x=[],r = [];
  for (var i = 0; i < l; i++) r.push([ein(o.v[i].k),ein(o.v[i].v)]);
  x = [116, l >>> 24, (l >>> 16) & 255, (l >>> 8) & 255, l & 255];
  return o.v.length == 0 ? x : [x,r];
}
function en_110(o) {
  var s=int_to_bytes(o.v); return [110,s.length,(o.v<0)?1:0,...s];
}

// DECODER

function nop(b) { return []; };
function big(b) {
  var sk = b == 1 ? sx.getUint8(ix++) : sx.getInt32((a = ix, ix += 4, a));
  var ret = 0, sig = sx.getUint8(ix++), count = sk;
  while (count-- > 0) { ret = 256 * ret + sx.getUint8(ix + count); }
  ix += sk; return ret * (sig == 0 ? 1 : -1);
}
function int(b) {
  return b == 1 ? sx.getUint8(ix++) : sx.getInt32((a = ix, ix += 4, a));
}
function dec(d) {
  sx = new DataView(d); ix = 0;
  if (sx.getUint8(ix++) !== 131) throw ("BERT?");
  return din();
}
function str(b) {
  var dv, sz = (b==2?sx.getUint16(ix):(b==1?sx.getUint8(ix):sx.getUint32(ix)));
  ix += b; var r = sx.buffer.slice(ix, ix += sz);
  return utf8_arr(r);
}
function run(b) {
  var sz = (b == 1 ? sx.getUint8(ix) : sx.getUint32(ix)), r = []; ix += b;
  for (var i = 0; i < sz; i++) r.push(din());
  return r;
}
function rut(b) {
  var sz = (b == 1 ? sx.getUint8(ix) : sx.getUint32(ix)), r = []; ix += b;
  for (var i = 0; i < sz; i++) r.push(din()); din();
  return r;
}
function dic(b) {
  var sz = sx.getUint32(ix), r = []; ix += 4;
  for (var i = 0; i < sz; i++) r.push({k:din(),v:din()});
  return r;
}
function iee(x) {
  return read_Float(new Uint8Array(sx.buffer.slice(ix,ix+=8)),0,false,52,8);
}
function flo(x) {
  return parseFloat(utf8_arr(sx.buffer.slice(ix, ix += 31)));
}
function arr(b) {
  var dv, sz = sx.getUint16(ix); ix += b;
  return new Uint8Array(sx.buffer.slice(ix, ix += sz));
}
function din() {
  var c = sx.getUint8(ix++), x; switch (c) {
    case  97: x = [int, 1]; break; case  98: x = [int, 4]; break;
    case  99: x = [flo, 0]; break; case  70: x = [iee, 0]; break;
    case 100: x = [str, 2]; break; case 104: x = [run, 1]; break;
    case 107: x = [arr, 2]; break; case 108: x = [rut, 4]; break;
    case 109: x = [str, 4]; break; case 110: x = [big, 1]; break;
    case 111: x = [big, 4]; break; case 115: x = [str, 1]; break;
    case 118: x = [str, 2]; break; case 119: x = [str, 1]; break;
    case 105: x = [run, 4]; break; case 116: x = [dic, 4]; break;
    default:  x = [nop, 0];
  } return { t: c, v: x[0](x[1]) };
}

// HELPERS

function int_to_bytes(Int) {
  if (Int % 1 !== 0) return [0];
  var isNegative, OriginalInt, i, Rem, s = [];
  isNegative = (Int < 0);
  if (isNegative) { Int = - Int - 1; }
  OriginalInt = Int;
  var length = 0;
  while (Int !== 0) { Rem = Int % 256;
    if (isNegative) { Rem = 255 - Rem; }
    s.push(Rem); Int = Math.floor(Int / 256); length++; }
  if (Int > 0) { throw ("Argument out of range: " + OriginalInt); }
  return s;
}
function uc(u1, u2) {
  if (u1.byteLength == 0) return u2; if (u2.byteLength == 0) return u1;
  var a = new Uint8Array(u1.byteLength + u2.byteLength);
  a.set(u1, 0); a.set(u2, u1.byteLength); return a;
}
function ar(o) {
  return o.v instanceof ArrayBuffer ? new Uint8Array(o.v) : o.v instanceof Uint8Array ? o.v :
    Array.isArray(o.v) ? new Uint8Array(o.v) : new Uint8Array(utf8_enc(o.v));
}
function fl(a) {
  return a.reduce(function (f, t) {
    return uc(f, t instanceof Uint8Array ? t :
      Array.isArray(t) ? fl(t) : new Uint8Array([t]));
  }, new Uint8Array());
}
