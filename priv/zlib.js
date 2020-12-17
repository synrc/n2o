function ZLib_inflate(source, options = {}) {
  let result = null;
  [firstByte, source] = source[0] == 131 && source[1] == 80 ? [source[0], source.slice(6)] : [null, source];
  let inflate = new Inflate(source, _decodeZlibHeader(source, 0).length, options);
  let buffer = inflate.decompress();
  inflate = null;
  if(firstByte) {
    result = new Uint8Array(buffer.length + 1);
    result.set([firstByte]);
    result.set(buffer, 1);
  } else {
    result = buffer;
  }
  return result;
}

function _decodeZlibHeader(source, cursor) {
  var ZLIB_COMPRESSION_METHOD_DEFLATE = 0x8;
  var cmf = source[cursor];
  var flg = source[cursor + 1];
  var checkSum = ((cmf << 8) + flg) % 31;
  var CINFO = (cmf >> 4) & 0x0f, CM = (cmf) & 0x0f;
  var FLEVEL = (flg >> 6) & 0x03, FDICT = (flg >> 5) & 0x01, FCHECK = (flg) & 0x1f;
  var zlibHeader = {CMF: {CINFO, CM}, FLG: {FLEVEL, FDICT, FCHECK}, length: 2};
  let error = checkSum !== 0 ? new TypeError("zlib header check-sum error") :
    zlibHeader.CMF.CM !== ZLIB_COMPRESSION_METHOD_DEFLATE ? new TypeError("zlib header unsupported compression method") :
      zlibHeader.FLG.FDICT ? new TypeError("zlib header FDICT is not supported") : null;
  if(error) throw error;
  return zlibHeader;
}

function Huffman(source) {
  var maxCodeLength = Math.max.apply(0, source);
  var minCodeLength = Math.min.apply(0, source);
  var skipLength = 2;
  var bitLength = 1;
  var huffmanCode = 0;
  var huffmanCodeTableSize = 1 << maxCodeLength;
  var huffmanCodeTable = new Uint32Array(huffmanCodeTableSize);

  while (bitLength <= maxCodeLength) {
    source.forEach((len, i) => {
      if (len === bitLength) {
        var code = huffmanCode;
        var j = 0, k = 0;
        for (; j < bitLength; ++j) { k = (k << 1) | (code & 1); code >>= 1; }
        var value = (bitLength << 16) | i;
        for (; k < huffmanCodeTableSize; k += skipLength) { huffmanCodeTable[k] = value; }
        ++huffmanCode;
      }
    });
    ++bitLength;
    huffmanCode <<= 1;
    skipLength  <<= 1;
  }
  return {huffmanCodeTable, maxCodeLength, minCodeLength};
}

function Inflate(input, cursor, options = {}) {
  this._streamBuffer = input;
  this._streamCursor = cursor;
  this._outputBuffer = new Uint8Array(options["bufferSize"] || 0x8000);
  this._outputCursor = 0;
  this._lastRLE      = 0;
  this._litlenTable  = null;
  this._bitStreamReaderBuffer = 0;
  this._bitStreamReaderBufferSize = 0;
}

Inflate.prototype.decompress = function Inflate_decompress() {
  while (_parseDeflatedBlock(this)) {}
  var result = this._outputBuffer.subarray(0, this._outputCursor);
  this._streamBuffer = null;
  this._outputBuffer = null;
  return result;
}

let _parseDeflatedBlock = that => {
  var bfinal = _readBits(that, 1);
  _readBits(that, 2) == BTYPE_FIXED_HUFFMAN ? _parseFixedHuffmanBlock(that) : _parseDynamicHuffmanBlock(that);
  return !bfinal;
}

let _expandOutputBuffer = that => {
  let newOutputBuffer = new Uint8Array(that._outputBuffer.length * 2);
  newOutputBuffer.set(that._outputBuffer);
  that._outputBuffer = newOutputBuffer;
  return that._outputBuffer;
}

let valuesConstructor = ({_bitStreamReaderBuffer, _bitStreamReaderBufferSize, _streamBuffer}) => {
  return [_bitStreamReaderBuffer, _bitStreamReaderBufferSize, _streamBuffer, _streamBuffer.length];
}

function _readBits(that, entity) {
  [bitsbuf, bitsbuflen, streamBuffer, streamLength] = valuesConstructor(that);
  let bitLength = entity instanceof Object ? entity.maxCodeLength : entity;
  let readerBuffer = null;
  let readerBufferSize = null;
  let result = null;
  while (bitsbuflen < bitLength) {
    if(that._streamCursor >= streamLength) break;
    bitsbuf |= streamBuffer[that._streamCursor++] << bitsbuflen;
    bitsbuflen += 8;
  }
  if(entity instanceof Object) {
    readerBuffer = bitsbuf >> (entity.huffmanCodeTable[bitsbuf & ((1 << bitLength) - 1)] >>> 16);
    readerBufferSize = bitsbuflen - (entity.huffmanCodeTable[bitsbuf & ((1 << bitLength) - 1)] >>> 16);
    result = entity.huffmanCodeTable[bitsbuf & ((1 << bitLength) - 1)] & 0xffff;
  } else {
    result = bitsbuf & ((1 << bitLength) - 1);
    bitsbuf >>>= bitLength;
    bitsbuflen -= bitLength
    readerBuffer = bitsbuf;
    readerBufferSize = bitsbuflen;
  }
  that._bitStreamReaderBuffer = readerBuffer;
  that._bitStreamReaderBufferSize = readerBufferSize;
  return result;
}

let _parseFixedHuffmanBlock = that => {
  that._litlenTable = FIXED_HUFFMAN_LENGTH_CODE_TABLE;
  _decodeHuffmanAdaptive(that, FIXED_HUFFMAN_DISTANCE_CODE_TABLE);
}

function _decodeHuffmanAdaptive(that, dist) {
  var outputBuffer = that._outputBuffer;
  var outputCursor = that._outputCursor;
  var outputBufferLength = outputBuffer.length;
  var huffmanCode  = 0;
  while ((huffmanCode = _readBits(that, that._litlenTable)) !== 256) {
    if (huffmanCode < 256) {
      outputBuffer = outputCursor >= outputBufferLength ? _expandOutputBuffer(that) : outputBuffer;
      outputBufferLength = outputBuffer.length;
      outputBuffer[outputCursor++] = huffmanCode;
    } else {
      var tableCursor = huffmanCode - 257;
      var huffmanCodeLength = HUFFMAN_LENGTH_CODE_TABLE[tableCursor];
      var huffmanBitsLength = HUFFMAN_LENGTH_EXTRA_BITS_TABLE[tableCursor];
      huffmanCodeLength += huffmanBitsLength > 0 ? _readBits(that, huffmanBitsLength) : 0;
      huffmanCode = _readBits(that, dist);
      var huffmanCodeDist = HUFFMAN_DIST_CODE_TABLE[huffmanCode];
      var huffmanBitsDist = HUFFMAN_DIST_EXTRA_BITS_TABLE[huffmanCode];
      huffmanCodeDist += huffmanBitsDist > 0 ? _readBits(that, huffmanBitsDist) : 0;
      outputBuffer = outputCursor + huffmanCodeLength > outputBufferLength ? _expandOutputBuffer(that) : outputBuffer;
      outputBufferLength = outputBuffer.length;
      while (huffmanCodeLength--) { outputBuffer[outputCursor] = outputBuffer[(outputCursor++) - huffmanCodeDist]; }
    }
  }
  while (that._bitStreamReaderBufferSize >= 8) {
    that._bitStreamReaderBufferSize -= 8;
    that._streamCursor--;
  }
  that._outputCursor = outputCursor;
}

function _parseDynamicHuffmanBlock(that) {
  var hlit  = _readBits(that, 5) + 257;
  var hdist = _readBits(that, 5) + 1;
  var hclen = _readBits(that, 4) + 4;
  var codeLengths = new Uint8Array(HUFFMAN_ORDER.length);
  for (var i = 0; i < hclen; ++i) { codeLengths[HUFFMAN_ORDER[i]] = _readBits(that, 3); }
  var codeLengthsTable = Huffman(codeLengths);
  var literalAndLengthCode = new Uint8Array(hlit);
  var distanceCodeLengths = new Uint8Array(hdist);
  that._lastRLE = 0;
  that._litlenTable = Huffman(_decodeDynamicHuffman(that, hlit, codeLengthsTable, literalAndLengthCode));

  _decodeHuffmanAdaptive(that, Huffman(_decodeDynamicHuffman(that, hdist, codeLengthsTable, distanceCodeLengths)));
}

function _decodeDynamicHuffman(that, loop, table, lengths) {
  var rle = that._lastRLE;
  for (var i = 0; i < loop; ) {
    var code = _readBits(that, table);
    var repeat = 0;
    switch (code) {
      case 16:
        repeat = 3 + _readBits(that, 2);
        while (repeat--) { lengths[i++] = rle; }
        break;
      case 17:
        repeat = 3 + _readBits(that, 3);
        while (repeat--) { lengths[i++] = 0; }
        rle = 0;
        break;
      case 18:
        repeat = 11 + _readBits(that, 7);
        while (repeat--) { lengths[i++] = 0; }
        rle = 0;
        break;
      default:
        lengths[i++] = code;
        rle = code;
    }
  }
  that._lastRLE = rle;
  return lengths;
}

var HUFFMAN_LENGTH_CODE_TABLE = new Uint16Array([
        0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008, 0x0009, 0x000a, 0x000b,
        0x000d, 0x000f, 0x0011, 0x0013, 0x0017, 0x001b, 0x001f, 0x0023, 0x002b,
        0x0033, 0x003b, 0x0043, 0x0053, 0x0063, 0x0073, 0x0083, 0x00a3, 0x00c3,
        0x00e3, 0x0102, 0x0102, 0x0102
    ]);
var HUFFMAN_DIST_CODE_TABLE = new Uint16Array([
        0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0007, 0x0009, 0x000d, 0x0011,
        0x0019, 0x0021, 0x0031, 0x0041, 0x0061, 0x0081, 0x00c1, 0x0101, 0x0181,
        0x0201, 0x0301, 0x0401, 0x0601, 0x0801, 0x0c01, 0x1001, 0x1801, 0x2001,
        0x3001, 0x4001, 0x6001
    ]);
var HUFFMAN_ORDER = new Uint16Array([16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]);
var HUFFMAN_LENGTH_EXTRA_BITS_TABLE = new Uint8Array([
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 0, 0
]);
var HUFFMAN_DIST_EXTRA_BITS_TABLE = new Uint8Array([
  0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13
]);
var FIXED_HUFFMAN_LENGTH_CODE_TABLE = (() => {
  let lengths = new Uint8Array(288).map((el, i) => (i <= 143) ? 8 : (i <= 255) ? 9 : (i <= 279) ? 7 : 8);
  return Huffman(lengths);
})();
var FIXED_HUFFMAN_DISTANCE_CODE_TABLE = (() => new Huffman(new Uint8Array(30).fill(5)))();
var BTYPE_UNCOMPRESSED    = 0;
var BTYPE_FIXED_HUFFMAN   = 1;
var BTYPE_DYNAMIC_HUFFMAN = 2;
var BTYPE_UNKNOWN         = 3;
