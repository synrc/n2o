try { module.exports = { dec: dec, enc: enc }; } catch (e) { }

// BERT Encoder

function uc(u1, u2) {
  if (u1.byteLength == 0) return u2; if (u2.byteLength == 0) return u1;
  var a = new Uint8Array(u1.byteLength + u2.byteLength);
  a.set(u1, 0); a.set(u2, u1.byteLength); return a;
};
function ar(o) {
  return o.v instanceof ArrayBuffer ? new Uint8Array(o.v) : o.v instanceof Uint8Array ? o.v :
    Array.isArray(o.v) ? new Uint8Array(o.v) : new Uint8Array(utf8_toByteArray(o.v).v);
}
function fl(a) {
  return a.reduce(function (f, t) {
    return uc(f, t instanceof Uint8Array ? t :
      Array.isArray(t) ? fl(t) : new Uint8Array([t]));
  }, new Uint8Array());
}
function atom(o) { return { t: 100, v: utf8_toByteArray(o).v }; }
function bin(o) {
  return {
    t: 109, v: o instanceof ArrayBuffer ?
      new Uint8Array(o) : o instanceof Uint8Array ? o : utf8_toByteArray(o).v
  };
}

function int_to_bytes(Int) {
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
};

function number(Obj) {
  var s, isInteger = (Obj % 1 === 0);
  if (isInteger && Obj >= 0 && Obj < 256) { return { t: 97, v: Obj };  }
  if (isInteger && Obj >= -134217728 && Obj <= 134217727) { return {t: 98, v: Obj}; }
  if (Number.isSafeInteger(Obj)) { return {t: 110, v: Obj}; } else {
     throw ("Need to impelement bigInt: " + Obj); } }

function tuple() { return { t: 104, v: Array.apply(null, arguments) }; }
function list() { return { t: 108, v: Array.apply(null, arguments) }; }
function enc(o) { return fl([131, ein(o)]); }
function ein(o) { return Array.isArray(o) ? en_108({ t: 108, v: o }) : eval('en_' + o.t)(o); }
function en_undefined(o) { return [106]; }
function en_98(o) { return [98, o.v >>> 24, (o.v >>> 16) & 255, (o.v >>> 8) & 255, o.v & 255]; }
function en_97(o) { return [97, o.v]; }
function en_106(o) { return [106]; }
function en_100(o) { return [100, o.v.length >>> 8, o.v.length & 255, ar(o)]; }
function en_107(o) { return [107, o.v.length >>> 8, o.v.length & 255, ar(o)]; }
function en_104(o) {
  var l = o.v.length, r = [];
  for (var i = 0; i < l; i++)r[i] = ein(o.v[i]); return [104, l, r];
}
function en_109(o) {
  var l = o.v instanceof ArrayBuffer ? o.v.byteLength : o.v.length;
  return [109, l >>> 24, (l >>> 16) & 255, (l >>> 8) & 255, l & 255, ar(o)];
}
function en_108(o) {
  var l = o.v.length, r = []; for (var i = 0; i < l; i++)r.push(ein(o.v[i]));
  return o.v.length == 0 ? [106] : [108, l >>> 24, (l >>> 16) & 255, (l >>> 8) & 255, l & 255, r, 106];
}
function en_110(o) { var s=int_to_bytes(o.v); return [110,s.length,(o.v<0)?1:0,...s]; }

// BERT Decoder

function nop(b) { return []; };
function big(b) {
  var sk = b == 1 ? sx.getUint8(ix++) : sx.getInt32((a = ix, ix += 4, a));
  var ret = 0, sig = sx.getUint8(ix++), count = sk;
  while (count-- > 0) {
    ret = 256 * ret + sx.getUint8(ix + count)
  }
  ix += sk;
  return ret * (sig == 0 ? 1 : -1);
}
function int(b) { return b == 1 ? sx.getUint8(ix++) : sx.getInt32((a = ix, ix += 4, a)); };
function dec(d) {
  sx = new DataView(d); ix = 0; if (sx.getUint8(ix++) !== 131)
    throw ("BERT?"); return din();
};
function str(b) {
  var dv, sz = (b == 2 ? sx.getUint16(ix) : sx.getInt32(ix)); ix += b;
  var r = sx.buffer.slice(ix, ix += sz); return utf8_dec(r);
};
function run(b) {
  var sz = (b == 1 ? sx.getUint8(ix) : sx.getUint32(ix)), r = []; ix += b;
  for (var i = 0; i < sz; i++) r.push(din()); if (b == 4) ix++; return r;
};

function arr(b) {
  var dv, sz = sx.getUint16(ix); ix += b; return new Uint8Array(sx.buffer.slice(ix, ix += sz));
};

function din() {
  var c = sx.getUint8(ix++), x; switch (c) {
    case  97: x = [int, 1]; break; case  98: x = [int, 4]; break;
    case 100: x = [str, 2]; break; case 104: x = [run, 1]; break;
    case 107: x = [arr, 2]; break; case 108: x = [run, 4]; break;
    case 109: x = [str, 4]; break; case 110: x = [big, 1]; break;
    case 111: x = [big, 4]; break; case 115: x = [str, 1]; break;
    case 118: x = [str, 2]; break; case 119: x = [str, 1]; break;
    default:  x = [nop, 0];
  } return { t: c, v: x[0](x[1]) };
};
