// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=[0],_1=[0,10],_2=[1,_1,_0],_3=0,_4=function(_5,_6,_){var _7=jsWriteHandle(E(_5)[1],toJSStr(E(_6)));return _3;},_8=function(_9,_a,_){var _b=E(_9),_c=jsWriteHandle(_b[1],toJSStr(E(_a)));return new F(function(){return _4(_b,_2,_);});},_d=function(_e){var _f=B(A(_e,[_])),_g=_f;return E(_g);},_h=function(_i){return new F(function(){return _d(function(_){var _=0;return new F(function(){return eval(_i);});});});},_j=function(_){var _=0;return new F(function(){return A(_h,["false",_]);});},_k=new T(function(){return B(_d(_j));}),_l=function(_){var _=0;return new F(function(){return A(_h,["true",_]);});},_m=new T(function(){return B(_d(_l));}),_n=new T(function(){return [0,"(function(a,b,c,d,e) { appl.wac.setup(a,b,c,d,e); })"];}),_o=function(_p,_q,_r,_s,_t,_){var _u=B(A(_h,[E(_n)[1],E(toJSStr(E(_p))),E(toJSStr(E(_q))),E(_r),E(toJSStr(E(_s))),!E(_t)?E(_k):E(_m),_])),_v=_u;return _3;},_w=new T(function(){return E(0);}),_x=new T(function(){return B(_h("(function(m){    return (function() {        return (function(){return E(B(A(m,[0])));});      });})"));}),_y=function(_z,_){return new F(function(){return A(_x,[E(_z),_]);});},_A=function(_B,_){return new F(function(){return _y(_B,_);});},_C=new T(function(){return B(_h("(function(n, action) { setTimeout(action, n); })"));}),_D=new T(function(){return B(A(_C,[E(1000)]));}),_E=new T(function(){return B(unCStr("Done!"));}),_F=new T(function(){return B(_h("(function(m) { appl.clickers[m].init_poise().execute() })"));}),_G=function(_){var _=0,_H=jsMkStdout(),_I=_H;return [0,_I];},_J=new T(function(){return B(_d(_G));}),_K=function(_L,_){var _M=E(_L);if(!_M[0]){return new F(function(){return _8(_J,_E,_);});}else{var _N=B(A(_F,[E(toJSStr(E(_M[1]))),_])),_O=_N,_P=B(A(_D,[B(_d(function(_){var _=0;return new F(function(){return _A(function(_){var _Q=B(_K(_M[2],_)),_R=_Q;return _w;},_);});})),_])),_S=_P;return _3;}},_T=new T(function(){return B(unCStr("Found it! Let\'s demonstrate."));}),_U=new T(function(){return B(unCStr("No solution found!"));}),_V=[1],_W=function(_X,_Y){while(1){var _Z=E(_X);if(!_Z[0]){return E(_Y)[0]==0?1:0;}else{var _10=E(_Y);if(!_10[0]){return 2;}else{var _11=E(_Z[1])[1],_12=E(_10[1])[1];if(_11!=_12){return _11>_12?2:0;}else{_X=_Z[2];_Y=_10[2];continue;}}}}},_13=false,_14=true,_15=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_16=function(_17){return new F(function(){return err(_15);});},_18=new T(function(){return B(_16(_));}),_19=function(_1a,_1b,_1c){var _1d=E(_1c);if(!_1d[0]){var _1e=_1d[1],_1f=E(_1b);if(!_1f[0]){var _1g=_1f[1],_1h=_1f[2];if(_1g<=(imul(3,_1e)|0)){return [0,(1+_1g|0)+_1e|0,E(E(_1a)),E(_1f),E(_1d)];}else{var _1i=E(_1f[3]);if(!_1i[0]){var _1j=_1i[1],_1k=E(_1f[4]);if(!_1k[0]){var _1l=_1k[1],_1m=_1k[2],_1n=_1k[3];if(_1l>=(imul(2,_1j)|0)){var _1o=function(_1p){var _1q=E(_1k[4]);return _1q[0]==0?[0,(1+_1g|0)+_1e|0,E(_1m),E([0,(1+_1j|0)+_1p|0,E(_1h),E(_1i),E(_1n)]),E([0,(1+_1e|0)+_1q[1]|0,E(E(_1a)),E(_1q),E(_1d)])]:[0,(1+_1g|0)+_1e|0,E(_1m),E([0,(1+_1j|0)+_1p|0,E(_1h),E(_1i),E(_1n)]),E([0,1+_1e|0,E(E(_1a)),E(_V),E(_1d)])];},_1r=E(_1n);return _1r[0]==0?B(_1o(_1r[1])):B(_1o(0));}else{return [0,(1+_1g|0)+_1e|0,E(_1h),E(_1i),E([0,(1+_1e|0)+_1l|0,E(E(_1a)),E(_1k),E(_1d)])];}}else{return E(_18);}}else{return E(_18);}}}else{return [0,1+_1e|0,E(E(_1a)),E(_V),E(_1d)];}}else{var _1s=E(_1b);if(!_1s[0]){var _1t=_1s[1],_1u=_1s[2],_1v=_1s[4],_1w=E(_1s[3]);if(!_1w[0]){var _1x=_1w[1],_1y=E(_1v);if(!_1y[0]){var _1z=_1y[1],_1A=_1y[2],_1B=_1y[3];if(_1z>=(imul(2,_1x)|0)){var _1C=function(_1D){var _1E=E(_1y[4]);return _1E[0]==0?[0,1+_1t|0,E(_1A),E([0,(1+_1x|0)+_1D|0,E(_1u),E(_1w),E(_1B)]),E([0,1+_1E[1]|0,E(E(_1a)),E(_1E),E(_V)])]:[0,1+_1t|0,E(_1A),E([0,(1+_1x|0)+_1D|0,E(_1u),E(_1w),E(_1B)]),E([0,1,E(E(_1a)),E(_V),E(_V)])];},_1F=E(_1B);return _1F[0]==0?B(_1C(_1F[1])):B(_1C(0));}else{return [0,1+_1t|0,E(_1u),E(_1w),E([0,1+_1z|0,E(E(_1a)),E(_1y),E(_V)])];}}else{return [0,3,E(_1u),E(_1w),E([0,1,E(E(_1a)),E(_V),E(_V)])];}}else{var _1G=E(_1v);return _1G[0]==0?[0,3,E(_1G[2]),E([0,1,E(_1u),E(_V),E(_V)]),E([0,1,E(E(_1a)),E(_V),E(_V)])]:[0,2,E(E(_1a)),E(_1s),E(_V)];}}else{return [0,1,E(E(_1a)),E(_V),E(_V)];}}},_1H=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_1I=function(_1J){return new F(function(){return err(_1H);});},_1K=new T(function(){return B(_1I(_));}),_1L=function(_1M,_1N,_1O){var _1P=E(_1N);if(!_1P[0]){var _1Q=_1P[1],_1R=E(_1O);if(!_1R[0]){var _1S=_1R[1],_1T=_1R[2];if(_1S<=(imul(3,_1Q)|0)){return [0,(1+_1Q|0)+_1S|0,E(E(_1M)),E(_1P),E(_1R)];}else{var _1U=E(_1R[3]);if(!_1U[0]){var _1V=_1U[1],_1W=_1U[2],_1X=_1U[3],_1Y=E(_1R[4]);if(!_1Y[0]){var _1Z=_1Y[1];if(_1V>=(imul(2,_1Z)|0)){var _20=function(_21){var _22=E(_1M),_23=E(_1U[4]);return _23[0]==0?[0,(1+_1Q|0)+_1S|0,E(_1W),E([0,(1+_1Q|0)+_21|0,E(_22),E(_1P),E(_1X)]),E([0,(1+_1Z|0)+_23[1]|0,E(_1T),E(_23),E(_1Y)])]:[0,(1+_1Q|0)+_1S|0,E(_1W),E([0,(1+_1Q|0)+_21|0,E(_22),E(_1P),E(_1X)]),E([0,1+_1Z|0,E(_1T),E(_V),E(_1Y)])];},_24=E(_1X);return _24[0]==0?B(_20(_24[1])):B(_20(0));}else{return [0,(1+_1Q|0)+_1S|0,E(_1T),E([0,(1+_1Q|0)+_1V|0,E(E(_1M)),E(_1P),E(_1U)]),E(_1Y)];}}else{return E(_1K);}}else{return E(_1K);}}}else{return [0,1+_1Q|0,E(E(_1M)),E(_1P),E(_V)];}}else{var _25=E(_1O);if(!_25[0]){var _26=_25[1],_27=_25[2],_28=_25[4],_29=E(_25[3]);if(!_29[0]){var _2a=_29[1],_2b=_29[2],_2c=_29[3],_2d=E(_28);if(!_2d[0]){var _2e=_2d[1];if(_2a>=(imul(2,_2e)|0)){var _2f=function(_2g){var _2h=E(_1M),_2i=E(_29[4]);return _2i[0]==0?[0,1+_26|0,E(_2b),E([0,1+_2g|0,E(_2h),E(_V),E(_2c)]),E([0,(1+_2e|0)+_2i[1]|0,E(_27),E(_2i),E(_2d)])]:[0,1+_26|0,E(_2b),E([0,1+_2g|0,E(_2h),E(_V),E(_2c)]),E([0,1+_2e|0,E(_27),E(_V),E(_2d)])];},_2j=E(_2c);return _2j[0]==0?B(_2f(_2j[1])):B(_2f(0));}else{return [0,1+_26|0,E(_27),E([0,1+_2a|0,E(E(_1M)),E(_V),E(_29)]),E(_2d)];}}else{return [0,3,E(_2b),E([0,1,E(E(_1M)),E(_V),E(_V)]),E([0,1,E(_27),E(_V),E(_V)])];}}else{var _2k=E(_28);return _2k[0]==0?[0,3,E(_27),E([0,1,E(E(_1M)),E(_V),E(_V)]),E(_2k)]:[0,2,E(E(_1M)),E(_V),E(_25)];}}else{return [0,1,E(E(_1M)),E(_V),E(_V)];}}},_2l=function(_2m,_2n,_2o,_2p,_2q,_2r){var _2s=E(_2r);if(!_2s[0]){var _2t=_2s[1],_2u=_2s[3],_2v=_2s[4],_2w=E(_2s[2]),_2x=_2w[5],_2y=function(_2z){return new F(function(){return _1L(_2w,_2u,B(_2l(_2m,_2n,_2o,_2p,_2q,_2v)));});},_2A=function(_2B){return new F(function(){return _19(_2w,B(_2l(_2m,_2n,_2o,_2p,_2q,_2u)),_2v);});};switch(B(_W(_2m,_2w[1]))){case 0:return new F(function(){return _2A(_);});break;case 1:switch(B(_W(_2n,_2w[2]))){case 0:return new F(function(){return _2A(_);});break;case 1:var _2C=E(_2o),_2D=_2C[1],_2E=E(_2w[3])[1];if(_2D>=_2E){if(_2D!=_2E){return new F(function(){return _2y(_);});}else{switch(B(_W(_2p,_2w[4]))){case 0:return new F(function(){return _2A(_);});break;case 1:return !E(_2q)?!E(_2x)?[0,_2t,E([0,_2m,_2n,_2C,_2p,_13]),E(_2u),E(_2v)]:B(_2A(_)):!E(_2x)?B(_2y(_)):[0,_2t,E([0,_2m,_2n,_2C,_2p,_14]),E(_2u),E(_2v)];default:return new F(function(){return _2y(_);});}}}else{return new F(function(){return _2A(_);});}break;default:return new F(function(){return _2y(_);});}break;default:return new F(function(){return _2y(_);});}}else{return [0,1,E([0,_2m,_2n,_2o,_2p,_2q]),E(_V),E(_V)];}},_2F=function(_2G,_2H,_2I,_2J,_2K,_2L){while(1){var _2M=E(_2L);if(!_2M[0]){var _2N=_2M[3],_2O=_2M[4],_2P=E(_2M[2]);switch(B(_W(_2G,_2P[1]))){case 0:_2J=_;_2L=_2N;continue;case 1:switch(B(_W(_2H,_2P[2]))){case 0:_2J=_;_2L=_2N;continue;case 1:var _2Q=E(_2P[3])[1];if(_2I>=_2Q){if(_2I!=_2Q){_2J=_;_2L=_2O;continue;}else{switch(B(_W(_2K,_2P[4]))){case 0:_2J=_;_2L=_2N;continue;case 1:if(!E(_2P[5])){return true;}else{_2J=_;_2L=_2N;continue;}break;default:_2J=_;_2L=_2O;continue;}}}else{_2J=_;_2L=_2N;continue;}break;default:_2J=_;_2L=_2O;continue;}break;default:_2J=_;_2L=_2O;continue;}}else{return false;}}},_2R=function(_2S,_2T,_2U,_2V,_2W,_2X){while(1){var _2Y=E(_2X);if(!_2Y[0]){var _2Z=_2Y[3],_30=_2Y[4],_31=E(_2Y[2]);switch(B(_W(_2S,_31[1]))){case 0:_2V=_;_2X=_2Z;continue;case 1:switch(B(_W(_2T,_31[2]))){case 0:_2V=_;_2X=_2Z;continue;case 1:var _32=E(_31[3])[1];if(_2U>=_32){if(_2U!=_32){_2V=_;_2X=_30;continue;}else{switch(B(_W(_2W,_31[4]))){case 0:_2V=_;_2X=_2Z;continue;case 1:if(!E(_31[5])){_2V=_;_2X=_30;continue;}else{return true;}break;default:_2V=_;_2X=_30;continue;}}}else{_2V=_;_2X=_2Z;continue;}break;default:_2V=_;_2X=_30;continue;}break;default:_2V=_;_2X=_30;continue;}}else{return false;}}},_33=function(_34,_35,_36,_37,_38,_39,_3a){while(1){var _3b=E(_3a);if(!_3b[0]){var _3c=_3b[3],_3d=_3b[4],_3e=E(_3b[2]),_3f=_3e[5];switch(B(_W(_34,_3e[1]))){case 0:_37=_;_3a=_3c;continue;case 1:switch(B(_W(_35,_3e[2]))){case 0:_37=_;_3a=_3c;continue;case 1:var _3g=E(_3e[3])[1];if(_36>=_3g){if(_36!=_3g){_37=_;_3a=_3d;continue;}else{switch(B(_W(_38,_3e[4]))){case 0:_37=_;_3a=_3c;continue;case 1:return !E(_39)?!E(_3f)?true:B(_2F(_34,_35,_36,_,_38,_3c)):!E(_3f)?B(_2R(_34,_35,_36,_,_38,_3d)):true;default:_37=_;_3a=_3d;continue;}}}else{_37=_;_3a=_3c;continue;}break;default:_37=_;_3a=_3d;continue;}break;default:_37=_;_3a=_3d;continue;}}else{return false;}}},_3h=function(_3i,_3j,_3k,_3l,_3m,_3n){while(1){var _3o=E(_3n);if(!_3o[0]){var _3p=_3o[3],_3q=_3o[4],_3r=E(_3o[2]),_3s=_3r[5];switch(B(_W(_3i,_3r[1]))){case 0:_3n=_3p;continue;case 1:switch(B(_W(_3j,_3r[2]))){case 0:_3n=_3p;continue;case 1:var _3t=E(_3k)[1],_3u=E(_3r[3])[1];if(_3t>=_3u){if(_3t!=_3u){return new F(function(){return _33(_3i,_3j,_3t,_,_3l,_3m,_3q);});}else{switch(B(_W(_3l,_3r[4]))){case 0:return new F(function(){return _33(_3i,_3j,_3t,_,_3l,_3m,_3p);});break;case 1:return !E(_3m)?!E(_3s)?true:B(_2F(_3i,_3j,_3t,_,_3l,_3p)):!E(_3s)?B(_2R(_3i,_3j,_3t,_,_3l,_3q)):true;default:return new F(function(){return _33(_3i,_3j,_3t,_,_3l,_3m,_3q);});}}}else{return new F(function(){return _33(_3i,_3j,_3t,_,_3l,_3m,_3p);});}break;default:_3n=_3q;continue;}break;default:_3n=_3q;continue;}}else{return false;}}},_3v=function(_3w,_3x){var _3y=E(_3w);return _3y[0]==0?E(_3x):[1,_3y[1],new T(function(){return B(_3v(_3y[2],_3x));})];},_3z=[0],_3A=function(_3B,_3C){while(1){var _3D=E(_3B);if(!_3D[0]){return E(_3C)[0]==0?true:false;}else{var _3E=E(_3C);if(!_3E[0]){return false;}else{if(E(_3D[1])[1]!=E(_3E[1])[1]){return false;}else{_3B=_3D[2];_3C=_3E[2];continue;}}}}},_3F=new T(function(){return B(unCStr("bGo"));}),_3G=function(_3H){var _3I=String(_3H),_3J=_3I;return new F(function(){return fromJSStr(_3J);});},_3K=new T(function(){return [0,"arr2lst"];}),_3L=function(_3M,_3N){return new F(function(){return _d(function(_){var _=0;return new F(function(){return A(_h,[E(_3K)[1],E(_3M),E(_3N),_]);});});});},_3O=function(_3P,_3Q){var _3R=E(_3Q);return _3R[0]==0?[0]:[1,new T(function(){return B(A(_3P,[_3R[1]]));}),new T(function(){return B(_3O(_3P,_3R[2]));})];},_3S=function(_){var _3T=B(A(_h,["(function(){ var out = []; for (var n in appl.clickers) { var c = appl.clickers[n], p = c.box.pens_held, top = (c.top_or_bottom === \'top\'); if (c.box && (p[1] && top) || (p[2] && !top)) out.push(n); }; return out; })",_])),_3U=_3T;return new T(function(){return B(_3O(_3G,B(_3L(_3U,0))));});},_3V=new T(function(){return B(unCStr("Control.Exception.Base"));}),_3W=new T(function(){return B(unCStr("base"));}),_3X=new T(function(){return B(unCStr("PatternMatchFail"));}),_3Y=new T(function(){var _3Z=hs_wordToWord64(18445595),_40=_3Z,_41=hs_wordToWord64(52003073),_42=_41;return [0,_40,_42,[0,_40,_42,_3W,_3V,_3X],_0];}),_43=function(_44){return E(_3Y);},_45=function(_46){return E(E(_46)[1]);},_47=function(_48,_49,_4a){var _4b=B(A(_48,[_])),_4c=B(A(_49,[_])),_4d=hs_eqWord64(_4b[1],_4c[1]),_4e=_4d;if(!E(_4e)){return [0];}else{var _4f=hs_eqWord64(_4b[2],_4c[2]),_4g=_4f;return E(_4g)==0?[0]:[1,_4a];}},_4h=function(_4i){var _4j=E(_4i);return new F(function(){return _47(B(_45(_4j[1])),_43,_4j[2]);});},_4k=function(_4l){return E(E(_4l)[1]);},_4m=function(_4n,_4o){return new F(function(){return _3v(E(_4n)[1],_4o);});},_4p=[0,44],_4q=[0,93],_4r=[0,91],_4s=function(_4t,_4u,_4v){var _4w=E(_4u);return _4w[0]==0?B(unAppCStr("[]",_4v)):[1,_4r,new T(function(){return B(A(_4t,[_4w[1],new T(function(){var _4x=function(_4y){var _4z=E(_4y);return _4z[0]==0?E([1,_4q,_4v]):[1,_4p,new T(function(){return B(A(_4t,[_4z[1],new T(function(){return B(_4x(_4z[2]));})]));})];};return B(_4x(_4w[2]));})]));})];},_4A=function(_4B,_4C){return new F(function(){return _4s(_4m,_4B,_4C);});},_4D=function(_4E,_4F,_4G){return new F(function(){return _3v(E(_4F)[1],_4G);});},_4H=[0,_4D,_4k,_4A],_4I=new T(function(){return [0,_43,_4H,_4J,_4h];}),_4J=function(_4K){return [0,_4I,_4K];},_4L=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_4M=function(_4N,_4O){return new F(function(){return die(new T(function(){return B(A(_4O,[_4N]));}));});},_4P=function(_4Q,_4R){var _4S=E(_4R);if(!_4S[0]){return [0,_0,_0];}else{var _4T=_4S[1];if(!B(A(_4Q,[_4T]))){return [0,_0,_4S];}else{var _4U=new T(function(){var _4V=B(_4P(_4Q,_4S[2]));return [0,_4V[1],_4V[2]];});return [0,[1,_4T,new T(function(){return E(E(_4U)[1]);})],new T(function(){return E(E(_4U)[2]);})];}}},_4W=[0,32],_4X=[0,10],_4Y=[1,_4X,_0],_4Z=function(_50){return E(E(_50)[1])==124?false:true;},_51=function(_52,_53){var _54=B(_4P(_4Z,B(unCStr(_52)))),_55=_54[1],_56=function(_57,_58){return new F(function(){return _3v(_57,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_3v(_53,new T(function(){return B(_3v(_58,_4Y));})));})));}));});},_59=E(_54[2]);if(!_59[0]){return new F(function(){return _56(_55,_0);});}else{return E(E(_59[1])[1])==124?B(_56(_55,[1,_4W,_59[2]])):B(_56(_55,_0));}},_5a=function(_5b){return new F(function(){return _4M([0,new T(function(){return B(_51(_5b,_4L));})],_4J);});},_5c=function(_5d){return new F(function(){return _5a("src/Haste/Foreign.hs:117:12-81|case");});},_5e=new T(function(){return B(_5c(_));}),_5f=function(_5g){return _5g>0;},_5h=function(_5i){var _5j=jsRound(_5i),_5k=_5j;return [0,_5k];},_5l=function(_){var _5m=B(A(_h,["(function() { return [appl.wac.pens.pen1.box.id, appl.wac.pens.pen2.box.id, appl.wac.last_pen_moved_ndx, appl.wac.last_move_box_id, appl.wac.rule60on]; })",_])),_5n=_5m,_5o=B(_3L(_5n,0));if(!_5o[0]){return E(_5e);}else{var _5p=E(_5o[2]);if(!_5p[0]){return E(_5e);}else{var _5q=E(_5p[2]);if(!_5q[0]){return E(_5e);}else{var _5r=E(_5q[2]);if(!_5r[0]){return E(_5e);}else{var _5s=E(_5r[2]);return _5s[0]==0?E(_5e):E(_5s[2])[0]==0?[0,new T(function(){return B(_3G(_5o[1]));}),new T(function(){return B(_3G(_5p[1]));}),new T(function(){return B(_5h(_5q[1]));}),new T(function(){return B(_3G(_5r[1]));}),new T(function(){return B(_5f(_5s[1]));})]:E(_5e);}}}}},_5t=function(_5u,_5v){while(1){var _5w=E(_5u);if(!_5w[0]){return E(_5v);}else{_5u=_5w[2];var _5x=[1,_5w[1],_5v];_5v=_5x;continue;}}},_5y=function(_5z,_5A,_){var _5B=E(_5z);if(!_5B[0]){return _3z;}else{var _5C=_5B[2],_5D=E(_5B[1]),_5E=E(_5D[1]),_5F=B(_o(_5E[1],_5E[2],E(_5E[3])[1],_5E[4],_5E[5],_)),_5G=_5F,_5H=E(_5D[2]),_5I=B(A(_F,[E(toJSStr(_5H)),_])),_5J=_5I,_5K=B(_5l(_)),_5L=_5K,_5M=E(_5L),_5N=_5M[1],_5O=_5M[2],_5P=_5M[3],_5Q=_5M[4],_5R=_5M[5],_5S=[1,_5H,_5D[3]],_5T=function(_5U){if(!B(_3h(_5N,_5O,_5P,_5Q,_5R,_5A))){var _5V=B(_3S(_)),_5W=_5V;return new F(function(){return _5y(B(_3v(_5C,new T(function(){return B(_3O(function(_5X){return [0,_5M,_5X,_5S];},_5W));}))),new T(function(){return B(_2l(_5N,_5O,_5P,_5Q,_5R,_5A));}),_);});}else{return new F(function(){return _5y(_5C,_5A,_);});}};return !B(_3A(_3F,_5N))?B(_5T(_)):!B(_3A(_3F,_5O))?B(_5T(_)):[1,new T(function(){return B(_5t(_5S,_0));})];}},_5Y=function(_){var _5Z=B(_5l(_)),_60=_5Z,_61=B(_3S(_)),_62=_61;return new F(function(){return _5y(B(_3O(function(_63){return [0,_60,_63,_0];},_62)),_V,_);});},_64=function(_){var _65=B(_5l(_)),_66=_65,_67=B(_5Y(_)),_68=_67,_69=E(_68);if(!_69[0]){return new F(function(){return _8(_J,_U,_);});}else{var _6a=B(_8(_J,_T,_)),_6b=_6a,_6c=E(_66),_6d=B(_o(_6c[1],_6c[2],E(_6c[3])[1],_6c[4],_6c[5],_)),_6e=_6d;return new F(function(){return _K(_69[1],_);});}},_6f=new T(function(){return [0,"(function(s,f){Haste[s] = f;})"];}),_6g=new T(function(){return B(_h(E(_6f)[1]));}),_6h=function(_6i,_6j){return function(_6k,_){var _6l=B(A(new T(function(){return B(A(_6g,[E(E(_6j)[1])]));}),[B(A(_6i,[_6k])),_])),_6m=_6l;return _3;};},_6n=new T(function(){return [0,"search"];}),_6o=function(_6p){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _A(function(_){var _6q=B(A(_6p,[_])),_6r=_6q;return _w;},_);});});});},_6s=new T(function(){return B(_6h(_6o,_6n));}),_6t=function(_){var _6u=B(_5l(_)),_6v=_6u;return new T(function(){var _6w=E(_6v);return !B(_3A(_3F,_6w[1]))?false:B(_3A(_3F,_6w[2]));});},_6x=new T(function(){return B(unCStr("Loaded! Run Haste.search() to find the solution."));}),_6y=new T(function(){return [0,"isVictory"];}),_6z=function(_6A){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _A(function(_){var _6B=B(A(_6A,[_])),_6C=_6B;return new T(function(){return !E(_6C)?E(_k):E(_m);});},_);});});});},_6D=new T(function(){return B(_6h(_6z,_6y));}),_6E=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_6F=new T(function(){return B(err(_6E));}),_6G=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_6H=new T(function(){return B(err(_6G));}),_6I=function(_6J,_6K){while(1){var _6L=E(_6J);if(!_6L[0]){return E(_6H);}else{var _6M=E(_6K);if(!_6M){return E(_6L[1]);}else{_6J=_6L[2];_6K=_6M-1|0;continue;}}}},_6N=function(_6O,_6P){while(1){var _6Q=E(_6O);if(!_6Q[0]){return E(_6P);}else{_6O=_6Q[2];var _6R=_6P+1|0;_6P=_6R;continue;}}},_6S=function(_){var _6T=B(_6U(_)),_6V=_6T;return _w;},_6W=function(_){var _=0;return new F(function(){return _A(_6S,_);});},_6U=function(_){var _6X=B(_3S(_)),_6Y=_6X,_6Z=E(_6Y);if(!_6Z[0]){return _3;}else{var _70=B(A(_h,["(Math.random)",_])),_71=_70,_72=B(_6N(_6Z,0))*_71,_73=_72&4294967295,_74=function(_75){if(_75>=0){var _76=B(A(_F,[E(toJSStr(B(_6I(_6Z,_75)))),_])),_77=_76,_78=B(A(_C,[E(1000),B(_d(_6W)),_])),_79=_78;return _3;}else{return E(_6F);}};if(_72>=_73){return new F(function(){return _74(_73);});}else{return new F(function(){return _74(_73-1|0);});}}},_7a=new T(function(){return [0,"randomWalk"];}),_7b=new T(function(){return B(_6h(_6o,_7a));}),_7c=function(_7d,_){var _7e=B(A(_F,[E(toJSStr(E(_7d))),_])),_7f=_7e;return _3;},_7g=new T(function(){return [0,"executeMove"];}),_7h=new T(function(){return B(_h("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_7i=function(_7j,_){return new F(function(){return A(_7h,[E(_7j),_]);});},_7k=function(_B,_){return new F(function(){return _7i(_B,_);});},_7l=function(_7m){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _7k(function(_7n){return new F(function(){return _d(function(_){var _=0,_7o=B(A(_7m,[B(_3G(_7n)),_])),_7p=_7o;return E(_w);});});},_);});});});},_7q=new T(function(){return B(_6h(_7l,_7g));}),_7r=new T(function(){return [0,"possibleMoves"];}),_7s=new T(function(){return B(_h("lst2arr"));}),_7t=function(_7u){return E(toJSStr(E(_7u)));},_7v=function(_7w){return new F(function(){return _d(function(_){var _=0;return new F(function(){return A(_7s,[B(_3O(_7t,_7w)),_]);});});});},_7x=function(_7y){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _A(function(_){var _7z=B(A(_7y,[_])),_7A=_7z;return new T(function(){return B(_7v(_7A));});},_);});});});},_7B=new T(function(){return B(_6h(_7x,_7r));}),_7C=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_7D=new T(function(){return B(err(_7C));}),_7E=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_7F=new T(function(){return B(err(_7E));}),_7G=new T(function(){return B(unCStr("Box"));}),_7H=new T(function(){return B(_5a("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_7I=function(_7J,_7K){while(1){var _7L=(function(_7M,_7N){var _7O=E(_7M);switch(_7O[0]){case 0:var _7P=E(_7N);if(!_7P[0]){return [0];}else{_7J=B(A(_7O[1],[_7P[1]]));_7K=_7P[2];return null;}break;case 1:var _7Q=B(A(_7O[1],[_7N])),_7R=_7N;_7J=_7Q;_7K=_7R;return null;case 2:return [0];case 3:return [1,[0,_7O[1],_7N],new T(function(){return B(_7I(_7O[2],_7N));})];default:return E(_7O[1]);}})(_7J,_7K);if(_7L!=null){return _7L;}}},_7S=function(_7T,_7U){var _7V=function(_7W){var _7X=E(_7U);if(_7X[0]==3){return [3,_7X[1],new T(function(){return B(_7S(_7T,_7X[2]));})];}else{var _7Y=E(_7T);if(_7Y[0]==2){return E(_7X);}else{var _7Z=E(_7X);if(_7Z[0]==2){return E(_7Y);}else{var _80=function(_81){var _82=E(_7Z);if(_82[0]==4){return [1,function(_83){return [4,new T(function(){return B(_3v(B(_7I(_7Y,_83)),_82[1]));})];}];}else{var _84=E(_7Y);if(_84[0]==1){var _85=_84[1],_86=E(_82);return _86[0]==0?[1,function(_87){return new F(function(){return _7S(B(A(_85,[_87])),_86);});}]:[1,function(_88){return new F(function(){return _7S(B(A(_85,[_88])),new T(function(){return B(A(_86[1],[_88]));}));});}];}else{var _89=E(_82);return _89[0]==0?E(_7H):[1,function(_8a){return new F(function(){return _7S(_84,new T(function(){return B(A(_89[1],[_8a]));}));});}];}}},_8b=E(_7Y);switch(_8b[0]){case 1:var _8c=E(_7Z);if(_8c[0]==4){return [1,function(_8d){return [4,new T(function(){return B(_3v(B(_7I(B(A(_8b[1],[_8d])),_8d)),_8c[1]));})];}];}else{return new F(function(){return _80(_);});}break;case 4:var _8e=_8b[1],_8f=E(_7Z);switch(_8f[0]){case 0:return [1,function(_8g){return [4,new T(function(){return B(_3v(_8e,new T(function(){return B(_7I(_8f,_8g));})));})];}];case 1:return [1,function(_8h){return [4,new T(function(){return B(_3v(_8e,new T(function(){return B(_7I(B(A(_8f[1],[_8h])),_8h));})));})];}];default:return [4,new T(function(){return B(_3v(_8e,_8f[1]));})];}break;default:return new F(function(){return _80(_);});}}}}},_8i=E(_7T);switch(_8i[0]){case 0:var _8j=E(_7U);if(!_8j[0]){return [0,function(_8k){return new F(function(){return _7S(B(A(_8i[1],[_8k])),new T(function(){return B(A(_8j[1],[_8k]));}));});}];}else{return new F(function(){return _7V(_);});}break;case 3:return [3,_8i[1],new T(function(){return B(_7S(_8i[2],_7U));})];default:return new F(function(){return _7V(_);});}},_8l=function(_8m,_8n){return new F(function(){return _8o(_8n);});},_8p=[0,41],_8q=[1,_8p,_0],_8r=[0,40],_8s=[1,_8r,_0],_8t=function(_8u,_8v){return E(_8u)[1]!=E(_8v)[1];},_8w=function(_8x,_8y){return E(_8x)[1]==E(_8y)[1];},_8z=[0,_8w,_8t],_8A=function(_8B,_8C){while(1){var _8D=E(_8B);if(!_8D[0]){return E(_8C)[0]==0?true:false;}else{var _8E=E(_8C);if(!_8E[0]){return false;}else{if(E(_8D[1])[1]!=E(_8E[1])[1]){return false;}else{_8B=_8D[2];_8C=_8E[2];continue;}}}}},_8F=function(_8G,_8H){return !B(_8A(_8G,_8H))?true:false;},_8I=[0,_8A,_8F],_8J=function(_8K,_8L){var _8M=E(_8K);switch(_8M[0]){case 0:return [0,function(_8N){return new F(function(){return _8J(B(A(_8M[1],[_8N])),_8L);});}];case 1:return [1,function(_8O){return new F(function(){return _8J(B(A(_8M[1],[_8O])),_8L);});}];case 2:return [2];case 3:return new F(function(){return _7S(B(A(_8L,[_8M[1]])),new T(function(){return B(_8J(_8M[2],_8L));}));});break;default:var _8P=function(_8Q){var _8R=E(_8Q);if(!_8R[0]){return [0];}else{var _8S=E(_8R[1]);return new F(function(){return _3v(B(_7I(B(A(_8L,[_8S[1]])),_8S[2])),new T(function(){return B(_8P(_8R[2]));}));});}},_8T=B(_8P(_8M[1]));return _8T[0]==0?[2]:[4,_8T];}},_8U=[2],_8V=function(_8W){return [3,_8W,_8U];},_8X=function(_8Y,_8Z){var _90=E(_8Y);if(!_90){return new F(function(){return A(_8Z,[_3]);});}else{return [0,function(_91){return E(new T(function(){return B(_8X(_90-1|0,_8Z));}));}];}},_92=function(_93,_94,_95){return function(_96){return new F(function(){return A(function(_97,_98,_99){while(1){var _9a=(function(_9b,_9c,_9d){var _9e=E(_9b);switch(_9e[0]){case 0:var _9f=E(_9c);if(!_9f[0]){return E(_94);}else{_97=B(A(_9e[1],[_9f[1]]));_98=_9f[2];var _9g=_9d+1|0;_99=_9g;return null;}break;case 1:var _9h=B(A(_9e[1],[_9c])),_9i=_9c,_9g=_9d;_97=_9h;_98=_9i;_99=_9g;return null;case 2:return E(_94);case 3:return function(_9j){return new F(function(){return _8X(_9d,function(_9k){return E(new T(function(){return B(_8J(_9e,_9j));}));});});};default:return function(_9l){return new F(function(){return _8J(_9e,_9l);});};}})(_97,_98,_99);if(_9a!=null){return _9a;}}},[new T(function(){return B(A(_93,[_8V]));}),_96,0,_95]);});};},_9m=function(_9n){return new F(function(){return A(_9n,[_0]);});},_9o=function(_9p,_9q){var _9r=function(_9s){var _9t=E(_9s);if(!_9t[0]){return E(_9m);}else{var _9u=_9t[1];return !B(A(_9p,[_9u]))?E(_9m):function(_9v){return [0,function(_9w){return E(new T(function(){return B(A(new T(function(){return B(_9r(_9t[2]));}),[function(_9x){return new F(function(){return A(_9v,[[1,_9u,_9x]]);});}]));}));}];};}};return function(_9y){return new F(function(){return A(_9r,[_9y,_9q]);});};},_9z=[6],_9A=function(_9B){return E(_9B);},_9C=new T(function(){return B(unCStr("valDig: Bad base"));}),_9D=new T(function(){return B(err(_9C));}),_9E=function(_9F,_9G){var _9H=function(_9I,_9J){var _9K=E(_9I);if(!_9K[0]){return function(_9L){return new F(function(){return A(_9L,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{var _9M=E(_9K[1])[1],_9N=function(_9O){return function(_9P){return [0,function(_9Q){return E(new T(function(){return B(A(new T(function(){return B(_9H(_9K[2],function(_9R){return new F(function(){return A(_9J,[[1,_9O,_9R]]);});}));}),[_9P]));}));}];};};switch(E(E(_9F)[1])){case 8:if(48>_9M){return function(_9S){return new F(function(){return A(_9S,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{if(_9M>55){return function(_9T){return new F(function(){return A(_9T,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{return new F(function(){return _9N([0,_9M-48|0]);});}}break;case 10:if(48>_9M){return function(_9U){return new F(function(){return A(_9U,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{if(_9M>57){return function(_9V){return new F(function(){return A(_9V,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{return new F(function(){return _9N([0,_9M-48|0]);});}}break;case 16:if(48>_9M){if(97>_9M){if(65>_9M){return function(_9W){return new F(function(){return A(_9W,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{if(_9M>70){return function(_9X){return new F(function(){return A(_9X,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{return new F(function(){return _9N([0,(_9M-65|0)+10|0]);});}}}else{if(_9M>102){if(65>_9M){return function(_9Y){return new F(function(){return A(_9Y,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{if(_9M>70){return function(_9Z){return new F(function(){return A(_9Z,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{return new F(function(){return _9N([0,(_9M-65|0)+10|0]);});}}}else{return new F(function(){return _9N([0,(_9M-97|0)+10|0]);});}}}else{if(_9M>57){if(97>_9M){if(65>_9M){return function(_a0){return new F(function(){return A(_a0,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{if(_9M>70){return function(_a1){return new F(function(){return A(_a1,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{return new F(function(){return _9N([0,(_9M-65|0)+10|0]);});}}}else{if(_9M>102){if(65>_9M){return function(_a2){return new F(function(){return A(_a2,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{if(_9M>70){return function(_a3){return new F(function(){return A(_a3,[new T(function(){return B(A(_9J,[_0]));})]);});};}else{return new F(function(){return _9N([0,(_9M-65|0)+10|0]);});}}}else{return new F(function(){return _9N([0,(_9M-97|0)+10|0]);});}}}else{return new F(function(){return _9N([0,_9M-48|0]);});}}break;default:return E(_9D);}}};return function(_a4){return new F(function(){return A(_9H,[_a4,_9A,function(_a5){var _a6=E(_a5);return _a6[0]==0?[2]:B(A(_9G,[_a6]));}]);});};},_a7=[0,10],_a8=[0,1],_a9=[0,2147483647],_aa=function(_ab,_ac){while(1){var _ad=E(_ab);if(!_ad[0]){var _ae=_ad[1],_af=E(_ac);if(!_af[0]){var _ag=_af[1],_ah=addC(_ae,_ag);if(!E(_ah[2])){return [0,_ah[1]];}else{_ab=[1,I_fromInt(_ae)];_ac=[1,I_fromInt(_ag)];continue;}}else{_ab=[1,I_fromInt(_ae)];_ac=_af;continue;}}else{var _ai=E(_ac);if(!_ai[0]){_ab=_ad;_ac=[1,I_fromInt(_ai[1])];continue;}else{return [1,I_add(_ad[1],_ai[1])];}}}},_aj=new T(function(){return B(_aa(_a9,_a8));}),_ak=function(_al){var _am=E(_al);if(!_am[0]){var _an=E(_am[1]);return _an==(-2147483648)?E(_aj):[0, -_an];}else{return [1,I_negate(_am[1])];}},_ao=[0,10],_ap=[0,0],_aq=function(_ar){return [0,_ar];},_as=function(_at,_au){while(1){var _av=E(_at);if(!_av[0]){var _aw=_av[1],_ax=E(_au);if(!_ax[0]){var _ay=_ax[1];if(!(imul(_aw,_ay)|0)){return [0,imul(_aw,_ay)|0];}else{_at=[1,I_fromInt(_aw)];_au=[1,I_fromInt(_ay)];continue;}}else{_at=[1,I_fromInt(_aw)];_au=_ax;continue;}}else{var _az=E(_au);if(!_az[0]){_at=_av;_au=[1,I_fromInt(_az[1])];continue;}else{return [1,I_mul(_av[1],_az[1])];}}}},_aA=function(_aB,_aC,_aD){while(1){var _aE=E(_aD);if(!_aE[0]){return E(_aC);}else{var _aF=B(_aa(B(_as(_aC,_aB)),B(_aq(E(_aE[1])[1]))));_aD=_aE[2];_aC=_aF;continue;}}},_aG=function(_aH){var _aI=new T(function(){return B(_7S(B(_7S([0,function(_aJ){return E(E(_aJ)[1])==45?[1,B(_9E(_a7,function(_aK){return new F(function(){return A(_aH,[[1,new T(function(){return B(_ak(B(_aA(_ao,_ap,_aK))));})]]);});}))]:[2];}],[0,function(_aL){return E(E(_aL)[1])==43?[1,B(_9E(_a7,function(_aM){return new F(function(){return A(_aH,[[1,new T(function(){return B(_aA(_ao,_ap,_aM));})]]);});}))]:[2];}])),new T(function(){return [1,B(_9E(_a7,function(_aN){return new F(function(){return A(_aH,[[1,new T(function(){return B(_aA(_ao,_ap,_aN));})]]);});}))];})));});return new F(function(){return _7S([0,function(_aO){return E(E(_aO)[1])==101?E(_aI):[2];}],[0,function(_aP){return E(E(_aP)[1])==69?E(_aI):[2];}]);});},_aQ=function(_aR){return new F(function(){return A(_aR,[_3z]);});},_aS=function(_aT){return new F(function(){return A(_aT,[_3z]);});},_aU=function(_aV){return function(_aW){return E(E(_aW)[1])==46?[1,B(_9E(_a7,function(_aX){return new F(function(){return A(_aV,[[1,_aX]]);});}))]:[2];};},_aY=function(_aZ){return [0,B(_aU(_aZ))];},_b0=function(_b1){return new F(function(){return _9E(_a7,function(_b2){return [1,B(_92(_aY,_aQ,function(_b3){return [1,B(_92(_aG,_aS,function(_b4){return new F(function(){return A(_b1,[[5,[1,_b2,_b3,_b4]]]);});}))];}))];});});},_b5=function(_b6){return [1,B(_b0(_b6))];},_b7=function(_b8){return E(E(_b8)[1]);},_b9=function(_ba,_bb,_bc){while(1){var _bd=E(_bc);if(!_bd[0]){return false;}else{if(!B(A(_b7,[_ba,_bb,_bd[1]]))){_bc=_bd[2];continue;}else{return true;}}}},_be=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_bf=function(_bg){return new F(function(){return _b9(_8z,_bg,_be);});},_bh=[0,8],_bi=[0,16],_bj=function(_bk){var _bl=function(_bm){return new F(function(){return A(_bk,[[5,[0,_bh,_bm]]]);});},_bn=function(_bo){return new F(function(){return A(_bk,[[5,[0,_bi,_bo]]]);});};return function(_bp){return E(E(_bp)[1])==48?E([0,function(_bq){switch(E(E(_bq)[1])){case 79:return [1,B(_9E(_bh,_bl))];case 88:return [1,B(_9E(_bi,_bn))];case 111:return [1,B(_9E(_bh,_bl))];case 120:return [1,B(_9E(_bi,_bn))];default:return [2];}}]):[2];};},_br=function(_bs){return [0,B(_bj(_bs))];},_bt=function(_bu){var _bv=new T(function(){return B(A(_bu,[_bh]));}),_bw=new T(function(){return B(A(_bu,[_bi]));});return function(_bx){switch(E(E(_bx)[1])){case 79:return E(_bv);case 88:return E(_bw);case 111:return E(_bv);case 120:return E(_bw);default:return [2];}};},_by=function(_bz){return [0,B(_bt(_bz))];},_bA=[0,92],_bB=function(_bC){return new F(function(){return A(_bC,[_a7]);});},_bD=function(_bE,_bF){var _bG=jsShowI(_bE),_bH=_bG;return new F(function(){return _3v(fromJSStr(_bH),_bF);});},_bI=[0,41],_bJ=[0,40],_bK=function(_bL,_bM,_bN){if(_bM>=0){return new F(function(){return _bD(_bM,_bN);});}else{return _bL<=6?B(_bD(_bM,_bN)):[1,_bJ,new T(function(){var _bO=jsShowI(_bM),_bP=_bO;return B(_3v(fromJSStr(_bP),[1,_bI,_bN]));})];}},_bQ=function(_bR){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_bK(9,_bR,_0));}))));});},_bS=function(_bT){var _bU=E(_bT);return _bU[0]==0?E(_bU[1]):I_toInt(_bU[1]);},_bV=function(_bW,_bX){var _bY=E(_bW);if(!_bY[0]){var _bZ=_bY[1],_c0=E(_bX);return _c0[0]==0?_bZ<=_c0[1]:I_compareInt(_c0[1],_bZ)>=0;}else{var _c1=_bY[1],_c2=E(_bX);return _c2[0]==0?I_compareInt(_c1,_c2[1])<=0:I_compare(_c1,_c2[1])<=0;}},_c3=function(_c4){return [2];},_c5=function(_c6){var _c7=E(_c6);if(!_c7[0]){return E(_c3);}else{var _c8=_c7[1],_c9=E(_c7[2]);return _c9[0]==0?E(_c8):function(_ca){return new F(function(){return _7S(B(A(_c8,[_ca])),new T(function(){return B(A(new T(function(){return B(_c5(_c9));}),[_ca]));}));});};}},_cb=function(_cc){return [2];},_cd=function(_ce,_cf){var _cg=function(_ch,_ci){var _cj=E(_ch);if(!_cj[0]){return function(_ck){return new F(function(){return A(_ck,[_ce]);});};}else{var _cl=E(_ci);return _cl[0]==0?E(_cb):E(_cj[1])[1]!=E(_cl[1])[1]?E(_cb):function(_cm){return [0,function(_cn){return E(new T(function(){return B(A(new T(function(){return B(_cg(_cj[2],_cl[2]));}),[_cm]));}));}];};}};return function(_co){return new F(function(){return A(_cg,[_ce,_co,_cf]);});};},_cp=new T(function(){return B(unCStr("SOH"));}),_cq=[0,1],_cr=function(_cs){return [1,B(_cd(_cp,function(_ct){return E(new T(function(){return B(A(_cs,[_cq]));}));}))];},_cu=new T(function(){return B(unCStr("SO"));}),_cv=[0,14],_cw=function(_cx){return [1,B(_cd(_cu,function(_cy){return E(new T(function(){return B(A(_cx,[_cv]));}));}))];},_cz=function(_cA){return [1,B(_92(_cr,_cw,_cA))];},_cB=new T(function(){return B(unCStr("NUL"));}),_cC=[0,0],_cD=function(_cE){return [1,B(_cd(_cB,function(_cF){return E(new T(function(){return B(A(_cE,[_cC]));}));}))];},_cG=new T(function(){return B(unCStr("STX"));}),_cH=[0,2],_cI=function(_cJ){return [1,B(_cd(_cG,function(_cK){return E(new T(function(){return B(A(_cJ,[_cH]));}));}))];},_cL=new T(function(){return B(unCStr("ETX"));}),_cM=[0,3],_cN=function(_cO){return [1,B(_cd(_cL,function(_cP){return E(new T(function(){return B(A(_cO,[_cM]));}));}))];},_cQ=new T(function(){return B(unCStr("EOT"));}),_cR=[0,4],_cS=function(_cT){return [1,B(_cd(_cQ,function(_cU){return E(new T(function(){return B(A(_cT,[_cR]));}));}))];},_cV=new T(function(){return B(unCStr("ENQ"));}),_cW=[0,5],_cX=function(_cY){return [1,B(_cd(_cV,function(_cZ){return E(new T(function(){return B(A(_cY,[_cW]));}));}))];},_d0=new T(function(){return B(unCStr("ACK"));}),_d1=[0,6],_d2=function(_d3){return [1,B(_cd(_d0,function(_d4){return E(new T(function(){return B(A(_d3,[_d1]));}));}))];},_d5=new T(function(){return B(unCStr("BEL"));}),_d6=[0,7],_d7=function(_d8){return [1,B(_cd(_d5,function(_d9){return E(new T(function(){return B(A(_d8,[_d6]));}));}))];},_da=new T(function(){return B(unCStr("BS"));}),_db=[0,8],_dc=function(_dd){return [1,B(_cd(_da,function(_de){return E(new T(function(){return B(A(_dd,[_db]));}));}))];},_df=new T(function(){return B(unCStr("HT"));}),_dg=[0,9],_dh=function(_di){return [1,B(_cd(_df,function(_dj){return E(new T(function(){return B(A(_di,[_dg]));}));}))];},_dk=new T(function(){return B(unCStr("LF"));}),_dl=[0,10],_dm=function(_dn){return [1,B(_cd(_dk,function(_do){return E(new T(function(){return B(A(_dn,[_dl]));}));}))];},_dp=new T(function(){return B(unCStr("VT"));}),_dq=[0,11],_dr=function(_ds){return [1,B(_cd(_dp,function(_dt){return E(new T(function(){return B(A(_ds,[_dq]));}));}))];},_du=new T(function(){return B(unCStr("FF"));}),_dv=[0,12],_dw=function(_dx){return [1,B(_cd(_du,function(_dy){return E(new T(function(){return B(A(_dx,[_dv]));}));}))];},_dz=new T(function(){return B(unCStr("CR"));}),_dA=[0,13],_dB=function(_dC){return [1,B(_cd(_dz,function(_dD){return E(new T(function(){return B(A(_dC,[_dA]));}));}))];},_dE=new T(function(){return B(unCStr("SI"));}),_dF=[0,15],_dG=function(_dH){return [1,B(_cd(_dE,function(_dI){return E(new T(function(){return B(A(_dH,[_dF]));}));}))];},_dJ=new T(function(){return B(unCStr("DLE"));}),_dK=[0,16],_dL=function(_dM){return [1,B(_cd(_dJ,function(_dN){return E(new T(function(){return B(A(_dM,[_dK]));}));}))];},_dO=new T(function(){return B(unCStr("DC1"));}),_dP=[0,17],_dQ=function(_dR){return [1,B(_cd(_dO,function(_dS){return E(new T(function(){return B(A(_dR,[_dP]));}));}))];},_dT=new T(function(){return B(unCStr("DC2"));}),_dU=[0,18],_dV=function(_dW){return [1,B(_cd(_dT,function(_dX){return E(new T(function(){return B(A(_dW,[_dU]));}));}))];},_dY=new T(function(){return B(unCStr("DC3"));}),_dZ=[0,19],_e0=function(_e1){return [1,B(_cd(_dY,function(_e2){return E(new T(function(){return B(A(_e1,[_dZ]));}));}))];},_e3=new T(function(){return B(unCStr("DC4"));}),_e4=[0,20],_e5=function(_e6){return [1,B(_cd(_e3,function(_e7){return E(new T(function(){return B(A(_e6,[_e4]));}));}))];},_e8=new T(function(){return B(unCStr("NAK"));}),_e9=[0,21],_ea=function(_eb){return [1,B(_cd(_e8,function(_ec){return E(new T(function(){return B(A(_eb,[_e9]));}));}))];},_ed=new T(function(){return B(unCStr("SYN"));}),_ee=[0,22],_ef=function(_eg){return [1,B(_cd(_ed,function(_eh){return E(new T(function(){return B(A(_eg,[_ee]));}));}))];},_ei=new T(function(){return B(unCStr("ETB"));}),_ej=[0,23],_ek=function(_el){return [1,B(_cd(_ei,function(_em){return E(new T(function(){return B(A(_el,[_ej]));}));}))];},_en=new T(function(){return B(unCStr("CAN"));}),_eo=[0,24],_ep=function(_eq){return [1,B(_cd(_en,function(_er){return E(new T(function(){return B(A(_eq,[_eo]));}));}))];},_es=new T(function(){return B(unCStr("EM"));}),_et=[0,25],_eu=function(_ev){return [1,B(_cd(_es,function(_ew){return E(new T(function(){return B(A(_ev,[_et]));}));}))];},_ex=new T(function(){return B(unCStr("SUB"));}),_ey=[0,26],_ez=function(_eA){return [1,B(_cd(_ex,function(_eB){return E(new T(function(){return B(A(_eA,[_ey]));}));}))];},_eC=new T(function(){return B(unCStr("ESC"));}),_eD=[0,27],_eE=function(_eF){return [1,B(_cd(_eC,function(_eG){return E(new T(function(){return B(A(_eF,[_eD]));}));}))];},_eH=new T(function(){return B(unCStr("FS"));}),_eI=[0,28],_eJ=function(_eK){return [1,B(_cd(_eH,function(_eL){return E(new T(function(){return B(A(_eK,[_eI]));}));}))];},_eM=new T(function(){return B(unCStr("GS"));}),_eN=[0,29],_eO=function(_eP){return [1,B(_cd(_eM,function(_eQ){return E(new T(function(){return B(A(_eP,[_eN]));}));}))];},_eR=new T(function(){return B(unCStr("RS"));}),_eS=[0,30],_eT=function(_eU){return [1,B(_cd(_eR,function(_eV){return E(new T(function(){return B(A(_eU,[_eS]));}));}))];},_eW=new T(function(){return B(unCStr("US"));}),_eX=[0,31],_eY=function(_eZ){return [1,B(_cd(_eW,function(_f0){return E(new T(function(){return B(A(_eZ,[_eX]));}));}))];},_f1=new T(function(){return B(unCStr("SP"));}),_f2=[0,32],_f3=function(_f4){return [1,B(_cd(_f1,function(_f5){return E(new T(function(){return B(A(_f4,[_f2]));}));}))];},_f6=new T(function(){return B(unCStr("DEL"));}),_f7=[0,127],_f8=function(_f9){return [1,B(_cd(_f6,function(_fa){return E(new T(function(){return B(A(_f9,[_f7]));}));}))];},_fb=[1,_f8,_0],_fc=[1,_f3,_fb],_fd=[1,_eY,_fc],_fe=[1,_eT,_fd],_ff=[1,_eO,_fe],_fg=[1,_eJ,_ff],_fh=[1,_eE,_fg],_fi=[1,_ez,_fh],_fj=[1,_eu,_fi],_fk=[1,_ep,_fj],_fl=[1,_ek,_fk],_fm=[1,_ef,_fl],_fn=[1,_ea,_fm],_fo=[1,_e5,_fn],_fp=[1,_e0,_fo],_fq=[1,_dV,_fp],_fr=[1,_dQ,_fq],_fs=[1,_dL,_fr],_ft=[1,_dG,_fs],_fu=[1,_dB,_ft],_fv=[1,_dw,_fu],_fw=[1,_dr,_fv],_fx=[1,_dm,_fw],_fy=[1,_dh,_fx],_fz=[1,_dc,_fy],_fA=[1,_d7,_fz],_fB=[1,_d2,_fA],_fC=[1,_cX,_fB],_fD=[1,_cS,_fC],_fE=[1,_cN,_fD],_fF=[1,_cI,_fE],_fG=[1,_cD,_fF],_fH=[1,_cz,_fG],_fI=new T(function(){return B(_c5(_fH));}),_fJ=[0,1114111],_fK=[0,34],_fL=[0,39],_fM=function(_fN){var _fO=new T(function(){return B(A(_fN,[_d6]));}),_fP=new T(function(){return B(A(_fN,[_db]));}),_fQ=new T(function(){return B(A(_fN,[_dg]));}),_fR=new T(function(){return B(A(_fN,[_dl]));}),_fS=new T(function(){return B(A(_fN,[_dq]));}),_fT=new T(function(){return B(A(_fN,[_dv]));}),_fU=new T(function(){return B(A(_fN,[_dA]));});return new F(function(){return _7S([0,function(_fV){switch(E(E(_fV)[1])){case 34:return E(new T(function(){return B(A(_fN,[_fK]));}));case 39:return E(new T(function(){return B(A(_fN,[_fL]));}));case 92:return E(new T(function(){return B(A(_fN,[_bA]));}));case 97:return E(_fO);case 98:return E(_fP);case 102:return E(_fT);case 110:return E(_fR);case 114:return E(_fU);case 116:return E(_fQ);case 118:return E(_fS);default:return [2];}}],new T(function(){return B(_7S([1,B(_92(_by,_bB,function(_fW){return [1,B(_9E(_fW,function(_fX){var _fY=B(_aA(new T(function(){return B(_aq(E(_fW)[1]));}),_ap,_fX));return !B(_bV(_fY,_fJ))?[2]:B(A(_fN,[new T(function(){var _fZ=B(_bS(_fY));if(_fZ>>>0>1114111){var _g0=B(_bQ(_fZ));}else{var _g0=[0,_fZ];}var _g1=_g0,_g2=_g1,_g3=_g2;return _g3;})]));}))];}))],new T(function(){return B(_7S([0,function(_g4){return E(E(_g4)[1])==94?E([0,function(_g5){switch(E(E(_g5)[1])){case 64:return E(new T(function(){return B(A(_fN,[_cC]));}));case 65:return E(new T(function(){return B(A(_fN,[_cq]));}));case 66:return E(new T(function(){return B(A(_fN,[_cH]));}));case 67:return E(new T(function(){return B(A(_fN,[_cM]));}));case 68:return E(new T(function(){return B(A(_fN,[_cR]));}));case 69:return E(new T(function(){return B(A(_fN,[_cW]));}));case 70:return E(new T(function(){return B(A(_fN,[_d1]));}));case 71:return E(_fO);case 72:return E(_fP);case 73:return E(_fQ);case 74:return E(_fR);case 75:return E(_fS);case 76:return E(_fT);case 77:return E(_fU);case 78:return E(new T(function(){return B(A(_fN,[_cv]));}));case 79:return E(new T(function(){return B(A(_fN,[_dF]));}));case 80:return E(new T(function(){return B(A(_fN,[_dK]));}));case 81:return E(new T(function(){return B(A(_fN,[_dP]));}));case 82:return E(new T(function(){return B(A(_fN,[_dU]));}));case 83:return E(new T(function(){return B(A(_fN,[_dZ]));}));case 84:return E(new T(function(){return B(A(_fN,[_e4]));}));case 85:return E(new T(function(){return B(A(_fN,[_e9]));}));case 86:return E(new T(function(){return B(A(_fN,[_ee]));}));case 87:return E(new T(function(){return B(A(_fN,[_ej]));}));case 88:return E(new T(function(){return B(A(_fN,[_eo]));}));case 89:return E(new T(function(){return B(A(_fN,[_et]));}));case 90:return E(new T(function(){return B(A(_fN,[_ey]));}));case 91:return E(new T(function(){return B(A(_fN,[_eD]));}));case 92:return E(new T(function(){return B(A(_fN,[_eI]));}));case 93:return E(new T(function(){return B(A(_fN,[_eN]));}));case 94:return E(new T(function(){return B(A(_fN,[_eS]));}));case 95:return E(new T(function(){return B(A(_fN,[_eX]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_fI,[_fN]));})));})));}));});},_g6=function(_g7){return new F(function(){return A(_g7,[_3]);});},_g8=function(_g9){var _ga=E(_g9);if(!_ga[0]){return E(_g6);}else{var _gb=_ga[2],_gc=E(E(_ga[1])[1]);switch(_gc){case 9:return function(_gd){return [0,function(_ge){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gd]));}));}];};case 10:return function(_gf){return [0,function(_gg){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gf]));}));}];};case 11:return function(_gh){return [0,function(_gi){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gh]));}));}];};case 12:return function(_gj){return [0,function(_gk){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gj]));}));}];};case 13:return function(_gl){return [0,function(_gm){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gl]));}));}];};case 32:return function(_gn){return [0,function(_go){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gn]));}));}];};case 160:return function(_gp){return [0,function(_gq){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gp]));}));}];};default:var _gr=u_iswspace(_gc),_gs=_gr;return E(_gs)==0?E(_g6):function(_gt){return [0,function(_gu){return E(new T(function(){return B(A(new T(function(){return B(_g8(_gb));}),[_gt]));}));}];};}}},_gv=function(_gw){var _gx=new T(function(){return B(_gv(_gw));}),_gy=[1,function(_gz){return new F(function(){return A(_g8,[_gz,function(_gA){return E([0,function(_gB){return E(E(_gB)[1])==92?E(_gx):[2];}]);}]);});}];return new F(function(){return _7S([0,function(_gC){return E(E(_gC)[1])==92?E([0,function(_gD){var _gE=E(E(_gD)[1]);switch(_gE){case 9:return E(_gy);case 10:return E(_gy);case 11:return E(_gy);case 12:return E(_gy);case 13:return E(_gy);case 32:return E(_gy);case 38:return E(_gx);case 160:return E(_gy);default:var _gF=u_iswspace(_gE),_gG=_gF;return E(_gG)==0?[2]:E(_gy);}}]):[2];}],[0,function(_gH){var _gI=E(_gH);return E(_gI[1])==92?E(new T(function(){return B(_fM(function(_gJ){return new F(function(){return A(_gw,[[0,_gJ,_14]]);});}));})):B(A(_gw,[[0,_gI,_13]]));}]);});},_gK=function(_gL,_gM){return new F(function(){return _gv(function(_gN){var _gO=E(_gN),_gP=E(_gO[1]);if(E(_gP[1])==34){if(!E(_gO[2])){return E(new T(function(){return B(A(_gM,[[1,new T(function(){return B(A(_gL,[_0]));})]]));}));}else{return new F(function(){return _gK(function(_gQ){return new F(function(){return A(_gL,[[1,_gP,_gQ]]);});},_gM);});}}else{return new F(function(){return _gK(function(_gR){return new F(function(){return A(_gL,[[1,_gP,_gR]]);});},_gM);});}});});},_gS=new T(function(){return B(unCStr("_\'"));}),_gT=function(_gU){var _gV=u_iswalnum(_gU),_gW=_gV;return E(_gW)==0?B(_b9(_8z,[0,_gU],_gS)):true;},_gX=function(_gY){return new F(function(){return _gT(E(_gY)[1]);});},_gZ=new T(function(){return B(unCStr(",;()[]{}`"));}),_h0=new T(function(){return B(unCStr(".."));}),_h1=new T(function(){return B(unCStr("::"));}),_h2=new T(function(){return B(unCStr("->"));}),_h3=[0,64],_h4=[1,_h3,_0],_h5=[0,126],_h6=[1,_h5,_0],_h7=new T(function(){return B(unCStr("=>"));}),_h8=[1,_h7,_0],_h9=[1,_h6,_h8],_ha=[1,_h4,_h9],_hb=[1,_h2,_ha],_hc=new T(function(){return B(unCStr("<-"));}),_hd=[1,_hc,_hb],_he=[0,124],_hf=[1,_he,_0],_hg=[1,_hf,_hd],_hh=[1,_bA,_0],_hi=[1,_hh,_hg],_hj=[0,61],_hk=[1,_hj,_0],_hl=[1,_hk,_hi],_hm=[1,_h1,_hl],_hn=[1,_h0,_hm],_ho=function(_hp){return new F(function(){return _7S([1,function(_hq){return E(_hq)[0]==0?E(new T(function(){return B(A(_hp,[_9z]));})):[2];}],new T(function(){return B(_7S([0,function(_hr){return E(E(_hr)[1])==39?E([0,function(_hs){var _ht=E(_hs);switch(E(_ht[1])){case 39:return [2];case 92:return E(new T(function(){return B(_fM(function(_hu){return [0,function(_hv){return E(E(_hv)[1])==39?E(new T(function(){return B(A(_hp,[[0,_hu]]));})):[2];}];}));}));default:return [0,function(_hw){return E(E(_hw)[1])==39?E(new T(function(){return B(A(_hp,[[0,_ht]]));})):[2];}];}}]):[2];}],new T(function(){return B(_7S([0,function(_hx){return E(E(_hx)[1])==34?E(new T(function(){return B(_gK(_9A,_hp));})):[2];}],new T(function(){return B(_7S([0,function(_hy){return !B(_b9(_8z,_hy,_gZ))?[2]:B(A(_hp,[[2,[1,_hy,_0]]]));}],new T(function(){return B(_7S([0,function(_hz){return !B(_b9(_8z,_hz,_be))?[2]:[1,B(_9o(_bf,function(_hA){var _hB=[1,_hz,_hA];return !B(_b9(_8I,_hB,_hn))?B(A(_hp,[[4,_hB]])):B(A(_hp,[[2,_hB]]));}))];}],new T(function(){return B(_7S([0,function(_hC){var _hD=E(_hC),_hE=_hD[1],_hF=u_iswalpha(_hE),_hG=_hF;return E(_hG)==0?E(_hE)==95?[1,B(_9o(_gX,function(_hH){return new F(function(){return A(_hp,[[3,[1,_hD,_hH]]]);});}))]:[2]:[1,B(_9o(_gX,function(_hI){return new F(function(){return A(_hp,[[3,[1,_hD,_hI]]]);});}))];}],new T(function(){return [1,B(_92(_br,_b5,_hp))];})));})));})));})));})));}));});},_hJ=[0,0],_hK=function(_hL,_hM){return function(_hN){return new F(function(){return A(_g8,[_hN,function(_hO){return E(new T(function(){return B(_ho(function(_hP){var _hQ=E(_hP);return _hQ[0]==2?!B(_3A(_hQ[1],_8s))?[2]:E(new T(function(){return B(A(_hL,[_hJ,function(_hR){return [1,function(_hS){return new F(function(){return A(_g8,[_hS,function(_hT){return E(new T(function(){return B(_ho(function(_hU){var _hV=E(_hU);return _hV[0]==2?!B(_3A(_hV[1],_8q))?[2]:E(new T(function(){return B(A(_hM,[_hR]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_hW=function(_hX){return new F(function(){return _7S([1,function(_hY){return new F(function(){return A(_g8,[_hY,function(_hZ){return E(new T(function(){return B(_ho(function(_i0){var _i1=E(_i0);return _i1[0]==0?B(A(_hX,[_i1[1]])):[2];}));}));}]);});}],new T(function(){return [1,B(_hK(_i2,_hX))];}));});},_i2=function(_i3,_i4){return new F(function(){return _hW(_i4);});},_i5=[0,91],_i6=[1,_i5,_0],_i7=function(_i8,_i9){var _ia=function(_ib,_ic){return [1,function(_id){return new F(function(){return A(_g8,[_id,function(_ie){return E(new T(function(){return B(_ho(function(_if){var _ig=E(_if);if(_ig[0]==2){var _ih=E(_ig[1]);if(!_ih[0]){return [2];}else{var _ii=_ih[2];switch(E(E(_ih[1])[1])){case 44:return E(_ii)[0]==0?!E(_ib)?[2]:E(new T(function(){return B(A(_i8,[_hJ,function(_ij){return new F(function(){return _ia(_14,function(_ik){return new F(function(){return A(_ic,[[1,_ij,_ik]]);});});});}]));})):[2];case 93:return E(_ii)[0]==0?E(new T(function(){return B(A(_ic,[_0]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_il=function(_im){return new F(function(){return _7S([1,function(_in){return new F(function(){return A(_g8,[_in,function(_io){return E(new T(function(){return B(_ho(function(_ip){var _iq=E(_ip);return _iq[0]==2?!B(_3A(_iq[1],_i6))?[2]:E(new T(function(){return B(_7S(B(_ia(_13,_im)),new T(function(){return B(A(_i8,[_hJ,function(_ir){return new F(function(){return _ia(_14,function(_is){return new F(function(){return A(_im,[[1,_ir,_is]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_hK(function(_it,_iu){return new F(function(){return _il(_iu);});},_im))];}));});};return new F(function(){return _il(_i9);});},_8o=function(_iv){return new F(function(){return _7S(B(_7S([1,function(_iw){return new F(function(){return A(_g8,[_iw,function(_ix){return E(new T(function(){return B(_ho(function(_iy){var _iz=E(_iy);return _iz[0]==1?B(A(_iv,[_iz[1]])):[2];}));}));}]);});}],new T(function(){return B(_i7(_i2,_iv));}))),new T(function(){return [1,B(_hK(_8l,_iv))];}));});},_iA=function(_iB,_iC){return _iB>10?[2]:[1,function(_iD){return new F(function(){return A(_g8,[_iD,function(_iE){return E(new T(function(){return B(_ho(function(_iF){var _iG=E(_iF);return _iG[0]==3?!B(_3A(_iG[1],_7G))?[2]:E(new T(function(){return B(_8o(function(_iH){return new F(function(){return A(_iC,[_iH]);});}));})):[2];}));}));}]);});}];},_iI=function(_iJ,_iK){return new F(function(){return _iA(E(_iJ)[1],_iK);});},_iL=function(_iM,_iN,_iO){var _iP=function(_iQ,_iR){return new F(function(){return _7S([1,function(_iS){return new F(function(){return A(_g8,[_iS,function(_iT){return E(new T(function(){return B(_ho(function(_iU){var _iV=E(_iU);if(_iV[0]==4){var _iW=E(_iV[1]);if(!_iW[0]){return new F(function(){return A(_iM,[_iV,_iQ,_iR]);});}else{return E(E(_iW[1])[1])==45?E(_iW[2])[0]==0?E([1,function(_iX){return new F(function(){return A(_g8,[_iX,function(_iY){return E(new T(function(){return B(_ho(function(_iZ){return new F(function(){return A(_iM,[_iZ,_iQ,function(_j0){return new F(function(){return A(_iR,[new T(function(){return [0, -E(_j0)[1]];})]);});}]);});}));}));}]);});}]):B(A(_iM,[_iV,_iQ,_iR])):B(A(_iM,[_iV,_iQ,_iR]));}}else{return new F(function(){return A(_iM,[_iV,_iQ,_iR]);});}}));}));}]);});}],new T(function(){return [1,B(_hK(_iP,_iR))];}));});};return new F(function(){return _iP(_iN,_iO);});},_j1=function(_j2,_j3){return [2];},_j4=function(_j5){var _j6=E(_j5);return _j6[0]==0?[1,new T(function(){return B(_aA(new T(function(){return B(_aq(E(_j6[1])[1]));}),_ap,_j6[2]));})]:E(_j6[2])[0]==0?E(_j6[3])[0]==0?[1,new T(function(){return B(_aA(_ao,_ap,_j6[1]));})]:[0]:[0];},_j7=function(_j8){var _j9=E(_j8);if(_j9[0]==5){var _ja=B(_j4(_j9[1]));return _ja[0]==0?E(_j1):function(_jb,_jc){return new F(function(){return A(_jc,[new T(function(){return [0,B(_bS(_ja[1]))];})]);});};}else{return E(_j1);}},_jd=new T(function(){return B(unCStr("Pen"));}),_je=[0,11],_jf=function(_jg,_jh){return _jg>10?[2]:[1,function(_ji){return new F(function(){return A(_g8,[_ji,function(_jj){return E(new T(function(){return B(_ho(function(_jk){var _jl=E(_jk);return _jl[0]==3?!B(_3A(_jl[1],_jd))?[2]:E(new T(function(){return B(_iL(_j7,_je,function(_jm){return new F(function(){return A(_jh,[_jm]);});}));})):[2];}));}));}]);});}];},_jn=function(_jo,_jp){return new F(function(){return _jf(E(_jo)[1],_jp);});},_jq=function(_jr,_js){return new F(function(){return _jt(_js);});},_ju=new T(function(){return B(unCStr("True"));}),_jv=new T(function(){return B(unCStr("False"));}),_jw=function(_jx){return function(_jy){return new F(function(){return A(_g8,[_jy,function(_jz){return E(new T(function(){return B(_ho(function(_jA){var _jB=E(_jA);if(_jB[0]==3){var _jC=_jB[1];return !B(_3A(_jC,_jv))?!B(_3A(_jC,_ju))?[2]:E(new T(function(){return B(A(_jx,[_14]));})):E(new T(function(){return B(A(_jx,[_13]));}));}else{return [2];}}));}));}]);});};},_jt=function(_jD){return new F(function(){return _7S([1,B(_jw(_jD))],new T(function(){return [1,B(_hK(_jq,_jD))];}));});},_jE=new T(function(){return B(unCStr("CowState"));}),_jF=[0,123],_jG=[1,_jF,_0],_jH=new T(function(){return B(unCStr("pen1"));}),_jI=[0,61],_jJ=[1,_jI,_0],_jK=[0,44],_jL=[1,_jK,_0],_jM=new T(function(){return B(unCStr("pen2"));}),_jN=new T(function(){return B(unCStr("lastPen"));}),_jO=new T(function(){return B(unCStr("lastBox"));}),_jP=new T(function(){return B(unCStr("is60"));}),_jQ=[0,125],_jR=[1,_jQ,_0],_jS=function(_jT,_jU){var _jV=function(_jW){return function(_jX){return new F(function(){return _7S(B(A(new T(function(){return B(A(_jT,[_jW]));}),[_jX])),new T(function(){return [1,B(_hK(_jV,_jX))];}));});};};return new F(function(){return _jV(_jU);});},_jY=function(_jZ,_k0){return _jZ>11?[2]:[1,function(_k1){return new F(function(){return A(_g8,[_k1,function(_k2){return E(new T(function(){return B(_ho(function(_k3){var _k4=E(_k3);return _k4[0]==3?!B(_3A(_k4[1],_jE))?[2]:E([1,function(_k5){return new F(function(){return A(_g8,[_k5,function(_k6){return E(new T(function(){return B(_ho(function(_k7){var _k8=E(_k7);return _k8[0]==2?!B(_3A(_k8[1],_jG))?[2]:E([1,function(_k9){return new F(function(){return A(_g8,[_k9,function(_ka){return E(new T(function(){return B(_ho(function(_kb){var _kc=E(_kb);return _kc[0]==3?!B(_3A(_kc[1],_jH))?[2]:E([1,function(_kd){return new F(function(){return A(_g8,[_kd,function(_ke){return E(new T(function(){return B(_ho(function(_kf){var _kg=E(_kf);return _kg[0]==2?!B(_3A(_kg[1],_jJ))?[2]:E(new T(function(){return B(A(_jS,[_iI,_hJ,function(_kh){return [1,function(_ki){return new F(function(){return A(_g8,[_ki,function(_kj){return E(new T(function(){return B(_ho(function(_kk){var _kl=E(_kk);return _kl[0]==2?!B(_3A(_kl[1],_jL))?[2]:E([1,function(_km){return new F(function(){return A(_g8,[_km,function(_kn){return E(new T(function(){return B(_ho(function(_ko){var _kp=E(_ko);return _kp[0]==3?!B(_3A(_kp[1],_jM))?[2]:E([1,function(_kq){return new F(function(){return A(_g8,[_kq,function(_kr){return E(new T(function(){return B(_ho(function(_ks){var _kt=E(_ks);return _kt[0]==2?!B(_3A(_kt[1],_jJ))?[2]:E(new T(function(){return B(A(_jS,[_iI,_hJ,function(_ku){return [1,function(_kv){return new F(function(){return A(_g8,[_kv,function(_kw){return E(new T(function(){return B(_ho(function(_kx){var _ky=E(_kx);return _ky[0]==2?!B(_3A(_ky[1],_jL))?[2]:E([1,function(_kz){return new F(function(){return A(_g8,[_kz,function(_kA){return E(new T(function(){return B(_ho(function(_kB){var _kC=E(_kB);return _kC[0]==3?!B(_3A(_kC[1],_jN))?[2]:E([1,function(_kD){return new F(function(){return A(_g8,[_kD,function(_kE){return E(new T(function(){return B(_ho(function(_kF){var _kG=E(_kF);return _kG[0]==2?!B(_3A(_kG[1],_jJ))?[2]:E(new T(function(){return B(A(_jS,[_jn,_hJ,function(_kH){return [1,function(_kI){return new F(function(){return A(_g8,[_kI,function(_kJ){return E(new T(function(){return B(_ho(function(_kK){var _kL=E(_kK);return _kL[0]==2?!B(_3A(_kL[1],_jL))?[2]:E([1,function(_kM){return new F(function(){return A(_g8,[_kM,function(_kN){return E(new T(function(){return B(_ho(function(_kO){var _kP=E(_kO);return _kP[0]==3?!B(_3A(_kP[1],_jO))?[2]:E([1,function(_kQ){return new F(function(){return A(_g8,[_kQ,function(_kR){return E(new T(function(){return B(_ho(function(_kS){var _kT=E(_kS);return _kT[0]==2?!B(_3A(_kT[1],_jJ))?[2]:E(new T(function(){return B(A(_jS,[_iI,_hJ,function(_kU){return [1,function(_kV){return new F(function(){return A(_g8,[_kV,function(_kW){return E(new T(function(){return B(_ho(function(_kX){var _kY=E(_kX);return _kY[0]==2?!B(_3A(_kY[1],_jL))?[2]:E([1,function(_kZ){return new F(function(){return A(_g8,[_kZ,function(_l0){return E(new T(function(){return B(_ho(function(_l1){var _l2=E(_l1);return _l2[0]==3?!B(_3A(_l2[1],_jP))?[2]:E([1,function(_l3){return new F(function(){return A(_g8,[_l3,function(_l4){return E(new T(function(){return B(_ho(function(_l5){var _l6=E(_l5);return _l6[0]==2?!B(_3A(_l6[1],_jJ))?[2]:E(new T(function(){return B(_jt(function(_l7){return [1,function(_l8){return new F(function(){return A(_g8,[_l8,function(_l9){return E(new T(function(){return B(_ho(function(_la){var _lb=E(_la);return _lb[0]==2?!B(_3A(_lb[1],_jR))?[2]:E(new T(function(){return B(A(_k0,[[0,_kh,_ku,_kH,_kU,_l7]]));})):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];},_lc=function(_ld,_le){return new F(function(){return _jY(E(_ld)[1],_le);});},_lf=function(_lg){return [1,function(_lh){return new F(function(){return A(_g8,[_lh,function(_li){return E([3,_lg,_8U]);}]);});}];},_lj=new T(function(){return B(A(_jS,[_lc,_hJ,_lf]));}),_lk=function(_ll){while(1){var _lm=(function(_ln){var _lo=E(_ln);if(!_lo[0]){return [0];}else{var _lp=_lo[2],_lq=E(_lo[1]);if(!E(_lq[2])[0]){return [1,_lq[1],new T(function(){return B(_lk(_lp));})];}else{_ll=_lp;return null;}}})(_ll);if(_lm!=null){return _lm;}}},_lr=function(_ls,_){var _lt=B(_lk(B(_7I(_lj,_ls))));if(!_lt[0]){return E(_7F);}else{if(!E(_lt[2])[0]){var _lu=E(_lt[1]);return new F(function(){return _o(_lu[1],_lu[2],E(_lu[3])[1],_lu[4],_lu[5],_);});}else{return E(_7D);}}},_lv=new T(function(){return [0,"restoreState"];}),_lw=function(_lx){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _7k(function(_ly){return new F(function(){return _d(function(_){var _=0,_lz=B(A(_lx,[B(_3G(_ly)),_])),_lA=_lz;return E(_w);});});},_);});});});},_lB=new T(function(){return B(_6h(_lw,_lv));}),_lC=new T(function(){return B(unCStr("CowState {"));}),_lD=new T(function(){return B(unCStr("is60 = "));}),_lE=new T(function(){return B(unCStr("lastBox = "));}),_lF=new T(function(){return B(unCStr("lastPen = "));}),_lG=new T(function(){return B(unCStr("pen2 = "));}),_lH=new T(function(){return B(unCStr(", "));}),_lI=new T(function(){return B(unCStr("pen1 = "));}),_lJ=new T(function(){return B(unCStr("Box "));}),_lK=[0,34],_lL=new T(function(){return B(unCStr("ACK"));}),_lM=new T(function(){return B(unCStr("BEL"));}),_lN=new T(function(){return B(unCStr("BS"));}),_lO=new T(function(){return B(unCStr("SP"));}),_lP=[1,_lO,_0],_lQ=new T(function(){return B(unCStr("US"));}),_lR=[1,_lQ,_lP],_lS=new T(function(){return B(unCStr("RS"));}),_lT=[1,_lS,_lR],_lU=new T(function(){return B(unCStr("GS"));}),_lV=[1,_lU,_lT],_lW=new T(function(){return B(unCStr("FS"));}),_lX=[1,_lW,_lV],_lY=new T(function(){return B(unCStr("ESC"));}),_lZ=[1,_lY,_lX],_m0=new T(function(){return B(unCStr("SUB"));}),_m1=[1,_m0,_lZ],_m2=new T(function(){return B(unCStr("EM"));}),_m3=[1,_m2,_m1],_m4=new T(function(){return B(unCStr("CAN"));}),_m5=[1,_m4,_m3],_m6=new T(function(){return B(unCStr("ETB"));}),_m7=[1,_m6,_m5],_m8=new T(function(){return B(unCStr("SYN"));}),_m9=[1,_m8,_m7],_ma=new T(function(){return B(unCStr("NAK"));}),_mb=[1,_ma,_m9],_mc=new T(function(){return B(unCStr("DC4"));}),_md=[1,_mc,_mb],_me=new T(function(){return B(unCStr("DC3"));}),_mf=[1,_me,_md],_mg=new T(function(){return B(unCStr("DC2"));}),_mh=[1,_mg,_mf],_mi=new T(function(){return B(unCStr("DC1"));}),_mj=[1,_mi,_mh],_mk=new T(function(){return B(unCStr("DLE"));}),_ml=[1,_mk,_mj],_mm=new T(function(){return B(unCStr("SI"));}),_mn=[1,_mm,_ml],_mo=new T(function(){return B(unCStr("SO"));}),_mp=[1,_mo,_mn],_mq=new T(function(){return B(unCStr("CR"));}),_mr=[1,_mq,_mp],_ms=new T(function(){return B(unCStr("FF"));}),_mt=[1,_ms,_mr],_mu=new T(function(){return B(unCStr("VT"));}),_mv=[1,_mu,_mt],_mw=new T(function(){return B(unCStr("LF"));}),_mx=[1,_mw,_mv],_my=new T(function(){return B(unCStr("HT"));}),_mz=[1,_my,_mx],_mA=[1,_lN,_mz],_mB=[1,_lM,_mA],_mC=[1,_lL,_mB],_mD=new T(function(){return B(unCStr("ENQ"));}),_mE=[1,_mD,_mC],_mF=new T(function(){return B(unCStr("EOT"));}),_mG=[1,_mF,_mE],_mH=new T(function(){return B(unCStr("ETX"));}),_mI=[1,_mH,_mG],_mJ=new T(function(){return B(unCStr("STX"));}),_mK=[1,_mJ,_mI],_mL=new T(function(){return B(unCStr("SOH"));}),_mM=[1,_mL,_mK],_mN=new T(function(){return B(unCStr("NUL"));}),_mO=[1,_mN,_mM],_mP=[0,92],_mQ=new T(function(){return B(unCStr("\\DEL"));}),_mR=new T(function(){return B(unCStr("\\a"));}),_mS=new T(function(){return B(unCStr("\\\\"));}),_mT=new T(function(){return B(unCStr("\\SO"));}),_mU=new T(function(){return B(unCStr("\\r"));}),_mV=new T(function(){return B(unCStr("\\f"));}),_mW=new T(function(){return B(unCStr("\\v"));}),_mX=new T(function(){return B(unCStr("\\n"));}),_mY=new T(function(){return B(unCStr("\\t"));}),_mZ=new T(function(){return B(unCStr("\\b"));}),_n0=function(_n1,_n2){if(_n1<=127){var _n3=E(_n1);switch(_n3){case 92:return new F(function(){return _3v(_mS,_n2);});break;case 127:return new F(function(){return _3v(_mQ,_n2);});break;default:if(_n3<32){var _n4=E(_n3);switch(_n4){case 7:return new F(function(){return _3v(_mR,_n2);});break;case 8:return new F(function(){return _3v(_mZ,_n2);});break;case 9:return new F(function(){return _3v(_mY,_n2);});break;case 10:return new F(function(){return _3v(_mX,_n2);});break;case 11:return new F(function(){return _3v(_mW,_n2);});break;case 12:return new F(function(){return _3v(_mV,_n2);});break;case 13:return new F(function(){return _3v(_mU,_n2);});break;case 14:return new F(function(){return _3v(_mT,new T(function(){var _n5=E(_n2);if(!_n5[0]){var _n6=[0];}else{var _n6=E(E(_n5[1])[1])==72?B(unAppCStr("\\&",_n5)):E(_n5);}return _n6;}));});break;default:return new F(function(){return _3v([1,_mP,new T(function(){var _n7=_n4;return _n7>=0?B(_6I(_mO,_n7)):E(_6F);})],_n2);});}}else{return [1,[0,_n3],_n2];}}}else{return [1,_mP,new T(function(){var _n8=jsShowI(_n1),_n9=_n8;return B(_3v(fromJSStr(_n9),new T(function(){var _na=E(_n2);if(!_na[0]){var _nb=[0];}else{var _nc=E(_na[1])[1];if(_nc<48){var _nd=E(_na);}else{var _nd=_nc>57?E(_na):B(unAppCStr("\\&",_na));}var _ne=_nd,_nf=_ne,_nb=_nf;}return _nb;})));})];}},_ng=new T(function(){return B(unCStr("\\\""));}),_nh=function(_ni,_nj){var _nk=E(_ni);if(!_nk[0]){return E(_nj);}else{var _nl=_nk[2],_nm=E(E(_nk[1])[1]);if(_nm==34){return new F(function(){return _3v(_ng,new T(function(){return B(_nh(_nl,_nj));}));});}else{return new F(function(){return _n0(_nm,new T(function(){return B(_nh(_nl,_nj));}));});}}},_nn=function(_no,_np,_nq){return _no<11?B(_3v(_lJ,[1,_lK,new T(function(){return B(_nh(_np,[1,_lK,_nq]));})])):[1,_bJ,new T(function(){return B(_3v(_lJ,[1,_lK,new T(function(){return B(_nh(_np,[1,_lK,[1,_bI,_nq]]));})]));})];},_nr=new T(function(){return B(unCStr("Pen "));}),_ns=function(_nt,_nu,_nv){return _nt<11?B(_3v(_nr,new T(function(){return B(_bK(11,E(_nu)[1],_nv));}))):[1,_bJ,new T(function(){return B(_3v(_nr,new T(function(){return B(_bK(11,E(_nu)[1],[1,_bI,_nv]));})));})];},_nw=new T(function(){return B(unCStr("True"));}),_nx=new T(function(){return B(unCStr("False"));}),_ny=function(_nz,_nA,_nB,_nC,_nD,_nE,_nF){var _nG=function(_nH){return new F(function(){return _3v(_lC,new T(function(){return B(_3v(_lI,new T(function(){return B(_nn(0,_nA,new T(function(){return B(_3v(_lH,new T(function(){return B(_3v(_lG,new T(function(){return B(_nn(0,_nB,new T(function(){return B(_3v(_lH,new T(function(){return B(_3v(_lF,new T(function(){return B(_ns(0,_nC,new T(function(){return B(_3v(_lH,new T(function(){return B(_3v(_lE,new T(function(){return B(_nn(0,_nD,new T(function(){return B(_3v(_lH,new T(function(){return B(_3v(_lD,new T(function(){return !E(_nE)?B(_3v(_nx,[1,_jQ,_nH])):B(_3v(_nw,[1,_jQ,_nH]));})));})));})));})));})));})));})));})));})));})));})));})));})));}));});};return _nz<11?B(_nG(_nF)):[1,_bJ,new T(function(){return B(_nG([1,_bI,_nF]));})];},_nI=function(_nJ){var _nK=E(_nJ);return new F(function(){return _ny(0,_nK[1],_nK[2],_nK[3],_nK[4],_nK[5],_0);});},_nL=function(_){var _nM=B(_5l(_)),_nN=_nM;return new T(function(){return B(_nI(_nN));});},_nO=new T(function(){return [0,"printState"];}),_nP=function(_nQ){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _A(function(_){var _nR=B(A(_nQ,[_])),_nS=_nR;return new T(function(){return B(_7t(_nS));});},_);});});});},_nT=new T(function(){return B(_6h(_nP,_nO));}),_nU=new T(function(){return [0,"showBacktrack"];}),_nV=function(_){var _=0;return new F(function(){return A(_h,["null",_]);});},_nW=new T(function(){return B(_d(_nV));}),_nX=function(_nY){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _A(function(_){var _nZ=B(A(_nY,[_])),_o0=_nZ;return new T(function(){var _o1=E(_o0);return _o1[0]==0?E(_nW):B(_7v(_o1[1]));});},_);});});});},_o2=new T(function(){return B(_6h(_nX,_nU));}),_o3=function(_){var _o4=B(A(_nT,[_nL,_])),_o5=_o4,_o6=B(A(_lB,[_lr,_])),_o7=_o6,_o8=B(A(_7B,[_3S,_])),_o9=_o8,_oa=B(A(_7q,[_7c,_])),_ob=_oa,_oc=B(A(_7b,[_6U,_])),_od=_oc,_oe=B(A(_6D,[_6t,_])),_of=_oe,_og=B(A(_6s,[_64,_])),_oh=_og,_oi=B(A(_o2,[_5Y,_])),_oj=_oi;return new F(function(){return _8(_J,_6x,_);});},_ok=function(_){return new F(function(){return _o3(_);});};
var hasteMain = function() {B(A(_ok, [0]));};hasteMain()