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

var _0=[0],_1=[0,10],_2=[1,_1,_0],_3=0,_4=function(_5,_6,_){var _7=jsWriteHandle(E(_5)[1],toJSStr(E(_6)));return _3;},_8=function(_9,_a,_){var _b=E(_9),_c=jsWriteHandle(_b[1],toJSStr(E(_a)));return new F(function(){return _4(_b,_2,_);});},_d=function(_e){var _f=B(A(_e,[_])),_g=_f;return E(_g);},_h=function(_i){return new F(function(){return _d(function(_){var _=0;return new F(function(){return eval(_i);});});});},_j=function(_){var _=0;return new F(function(){return A(_h,["false",_]);});},_k=new T(function(){return B(_d(_j));}),_l=function(_){var _=0;return new F(function(){return A(_h,["true",_]);});},_m=new T(function(){return B(_d(_l));}),_n=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_o=new T(function(){return B(err(_n));}),_p=new T(function(){return [0,"(function(a,b,c,d,e) { appl.wac.setup(a,b,c,d,e); })"];}),_q=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_r=new T(function(){return B(err(_q));}),_s=new T(function(){return B(unCStr("Box"));}),_t=function(_u,_v){var _w=E(_u);return _w[0]==0?E(_v):[1,_w[1],new T(function(){return B(_t(_w[2],_v));})];},_x=new T(function(){return B(unCStr("Control.Exception.Base"));}),_y=new T(function(){return B(unCStr("base"));}),_z=new T(function(){return B(unCStr("PatternMatchFail"));}),_A=new T(function(){var _B=hs_wordToWord64(18445595),_C=_B,_D=hs_wordToWord64(52003073),_E=_D;return [0,_C,_E,[0,_C,_E,_y,_x,_z],_0];}),_F=function(_G){return E(_A);},_H=function(_I){return E(E(_I)[1]);},_J=function(_K,_L,_M){var _N=B(A(_K,[_])),_O=B(A(_L,[_])),_P=hs_eqWord64(_N[1],_O[1]),_Q=_P;if(!E(_Q)){return [0];}else{var _R=hs_eqWord64(_N[2],_O[2]),_S=_R;return E(_S)==0?[0]:[1,_M];}},_T=function(_U){var _V=E(_U);return new F(function(){return _J(B(_H(_V[1])),_F,_V[2]);});},_W=function(_X){return E(E(_X)[1]);},_Y=function(_Z,_10){return new F(function(){return _t(E(_Z)[1],_10);});},_11=[0,44],_12=[0,93],_13=[0,91],_14=function(_15,_16,_17){var _18=E(_16);return _18[0]==0?B(unAppCStr("[]",_17)):[1,_13,new T(function(){return B(A(_15,[_18[1],new T(function(){var _19=function(_1a){var _1b=E(_1a);return _1b[0]==0?E([1,_12,_17]):[1,_11,new T(function(){return B(A(_15,[_1b[1],new T(function(){return B(_19(_1b[2]));})]));})];};return B(_19(_18[2]));})]));})];},_1c=function(_1d,_1e){return new F(function(){return _14(_Y,_1d,_1e);});},_1f=function(_1g,_1h,_1i){return new F(function(){return _t(E(_1h)[1],_1i);});},_1j=[0,_1f,_W,_1c],_1k=new T(function(){return [0,_F,_1j,_1l,_T];}),_1l=function(_1m){return [0,_1k,_1m];},_1n=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_1o=function(_1p,_1q){return new F(function(){return die(new T(function(){return B(A(_1q,[_1p]));}));});},_1r=function(_1s,_1t){var _1u=E(_1t);if(!_1u[0]){return [0,_0,_0];}else{var _1v=_1u[1];if(!B(A(_1s,[_1v]))){return [0,_0,_1u];}else{var _1w=new T(function(){var _1x=B(_1r(_1s,_1u[2]));return [0,_1x[1],_1x[2]];});return [0,[1,_1v,new T(function(){return E(E(_1w)[1]);})],new T(function(){return E(E(_1w)[2]);})];}}},_1y=[0,32],_1z=[0,10],_1A=[1,_1z,_0],_1B=function(_1C){return E(E(_1C)[1])==124?false:true;},_1D=function(_1E,_1F){var _1G=B(_1r(_1B,B(unCStr(_1E)))),_1H=_1G[1],_1I=function(_1J,_1K){return new F(function(){return _t(_1J,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_t(_1F,new T(function(){return B(_t(_1K,_1A));})));})));}));});},_1L=E(_1G[2]);if(!_1L[0]){return new F(function(){return _1I(_1H,_0);});}else{return E(E(_1L[1])[1])==124?B(_1I(_1H,[1,_1y,_1L[2]])):B(_1I(_1H,_0));}},_1M=function(_1N){return new F(function(){return _1o([0,new T(function(){return B(_1D(_1N,_1n));})],_1l);});},_1O=new T(function(){return B(_1M("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_1P=function(_1Q,_1R){while(1){var _1S=(function(_1T,_1U){var _1V=E(_1T);switch(_1V[0]){case 0:var _1W=E(_1U);if(!_1W[0]){return [0];}else{_1Q=B(A(_1V[1],[_1W[1]]));_1R=_1W[2];return null;}break;case 1:var _1X=B(A(_1V[1],[_1U])),_1Y=_1U;_1Q=_1X;_1R=_1Y;return null;case 2:return [0];case 3:return [1,[0,_1V[1],_1U],new T(function(){return B(_1P(_1V[2],_1U));})];default:return E(_1V[1]);}})(_1Q,_1R);if(_1S!=null){return _1S;}}},_1Z=function(_20,_21){var _22=function(_23){var _24=E(_21);if(_24[0]==3){return [3,_24[1],new T(function(){return B(_1Z(_20,_24[2]));})];}else{var _25=E(_20);if(_25[0]==2){return E(_24);}else{var _26=E(_24);if(_26[0]==2){return E(_25);}else{var _27=function(_28){var _29=E(_26);if(_29[0]==4){return [1,function(_2a){return [4,new T(function(){return B(_t(B(_1P(_25,_2a)),_29[1]));})];}];}else{var _2b=E(_25);if(_2b[0]==1){var _2c=_2b[1],_2d=E(_29);return _2d[0]==0?[1,function(_2e){return new F(function(){return _1Z(B(A(_2c,[_2e])),_2d);});}]:[1,function(_2f){return new F(function(){return _1Z(B(A(_2c,[_2f])),new T(function(){return B(A(_2d[1],[_2f]));}));});}];}else{var _2g=E(_29);return _2g[0]==0?E(_1O):[1,function(_2h){return new F(function(){return _1Z(_2b,new T(function(){return B(A(_2g[1],[_2h]));}));});}];}}},_2i=E(_25);switch(_2i[0]){case 1:var _2j=E(_26);if(_2j[0]==4){return [1,function(_2k){return [4,new T(function(){return B(_t(B(_1P(B(A(_2i[1],[_2k])),_2k)),_2j[1]));})];}];}else{return new F(function(){return _27(_);});}break;case 4:var _2l=_2i[1],_2m=E(_26);switch(_2m[0]){case 0:return [1,function(_2n){return [4,new T(function(){return B(_t(_2l,new T(function(){return B(_1P(_2m,_2n));})));})];}];case 1:return [1,function(_2o){return [4,new T(function(){return B(_t(_2l,new T(function(){return B(_1P(B(A(_2m[1],[_2o])),_2o));})));})];}];default:return [4,new T(function(){return B(_t(_2l,_2m[1]));})];}break;default:return new F(function(){return _27(_);});}}}}},_2p=E(_20);switch(_2p[0]){case 0:var _2q=E(_21);if(!_2q[0]){return [0,function(_2r){return new F(function(){return _1Z(B(A(_2p[1],[_2r])),new T(function(){return B(A(_2q[1],[_2r]));}));});}];}else{return new F(function(){return _22(_);});}break;case 3:return [3,_2p[1],new T(function(){return B(_1Z(_2p[2],_21));})];default:return new F(function(){return _22(_);});}},_2s=function(_2t,_2u){return new F(function(){return _2v(_2u);});},_2w=[0,41],_2x=[1,_2w,_0],_2y=[0,40],_2z=[1,_2y,_0],_2A=function(_2B,_2C){while(1){var _2D=E(_2B);if(!_2D[0]){return E(_2C)[0]==0?true:false;}else{var _2E=E(_2C);if(!_2E[0]){return false;}else{if(E(_2D[1])[1]!=E(_2E[1])[1]){return false;}else{_2B=_2D[2];_2C=_2E[2];continue;}}}}},_2F=function(_2G,_2H){return E(_2G)[1]!=E(_2H)[1];},_2I=function(_2J,_2K){return E(_2J)[1]==E(_2K)[1];},_2L=[0,_2I,_2F],_2M=function(_2N,_2O){while(1){var _2P=E(_2N);if(!_2P[0]){return E(_2O)[0]==0?true:false;}else{var _2Q=E(_2O);if(!_2Q[0]){return false;}else{if(E(_2P[1])[1]!=E(_2Q[1])[1]){return false;}else{_2N=_2P[2];_2O=_2Q[2];continue;}}}}},_2R=function(_2S,_2T){return !B(_2M(_2S,_2T))?true:false;},_2U=[0,_2M,_2R],_2V=function(_2W,_2X){var _2Y=E(_2W);switch(_2Y[0]){case 0:return [0,function(_2Z){return new F(function(){return _2V(B(A(_2Y[1],[_2Z])),_2X);});}];case 1:return [1,function(_30){return new F(function(){return _2V(B(A(_2Y[1],[_30])),_2X);});}];case 2:return [2];case 3:return new F(function(){return _1Z(B(A(_2X,[_2Y[1]])),new T(function(){return B(_2V(_2Y[2],_2X));}));});break;default:var _31=function(_32){var _33=E(_32);if(!_33[0]){return [0];}else{var _34=E(_33[1]);return new F(function(){return _t(B(_1P(B(A(_2X,[_34[1]])),_34[2])),new T(function(){return B(_31(_33[2]));}));});}},_35=B(_31(_2Y[1]));return _35[0]==0?[2]:[4,_35];}},_36=[2],_37=function(_38){return [3,_38,_36];},_39=function(_3a,_3b){var _3c=E(_3a);if(!_3c){return new F(function(){return A(_3b,[_3]);});}else{return [0,function(_3d){return E(new T(function(){return B(_39(_3c-1|0,_3b));}));}];}},_3e=function(_3f,_3g,_3h){return function(_3i){return new F(function(){return A(function(_3j,_3k,_3l){while(1){var _3m=(function(_3n,_3o,_3p){var _3q=E(_3n);switch(_3q[0]){case 0:var _3r=E(_3o);if(!_3r[0]){return E(_3g);}else{_3j=B(A(_3q[1],[_3r[1]]));_3k=_3r[2];var _3s=_3p+1|0;_3l=_3s;return null;}break;case 1:var _3t=B(A(_3q[1],[_3o])),_3u=_3o,_3s=_3p;_3j=_3t;_3k=_3u;_3l=_3s;return null;case 2:return E(_3g);case 3:return function(_3v){return new F(function(){return _39(_3p,function(_3w){return E(new T(function(){return B(_2V(_3q,_3v));}));});});};default:return function(_3x){return new F(function(){return _2V(_3q,_3x);});};}})(_3j,_3k,_3l);if(_3m!=null){return _3m;}}},[new T(function(){return B(A(_3f,[_37]));}),_3i,0,_3h]);});};},_3y=function(_3z){return new F(function(){return A(_3z,[_0]);});},_3A=function(_3B,_3C){var _3D=function(_3E){var _3F=E(_3E);if(!_3F[0]){return E(_3y);}else{var _3G=_3F[1];return !B(A(_3B,[_3G]))?E(_3y):function(_3H){return [0,function(_3I){return E(new T(function(){return B(A(new T(function(){return B(_3D(_3F[2]));}),[function(_3J){return new F(function(){return A(_3H,[[1,_3G,_3J]]);});}]));}));}];};}};return function(_3K){return new F(function(){return A(_3D,[_3K,_3C]);});};},_3L=[6],_3M=function(_3N){return E(_3N);},_3O=new T(function(){return B(unCStr("valDig: Bad base"));}),_3P=new T(function(){return B(err(_3O));}),_3Q=function(_3R,_3S){var _3T=function(_3U,_3V){var _3W=E(_3U);if(!_3W[0]){return function(_3X){return new F(function(){return A(_3X,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{var _3Y=E(_3W[1])[1],_3Z=function(_40){return function(_41){return [0,function(_42){return E(new T(function(){return B(A(new T(function(){return B(_3T(_3W[2],function(_43){return new F(function(){return A(_3V,[[1,_40,_43]]);});}));}),[_41]));}));}];};};switch(E(E(_3R)[1])){case 8:if(48>_3Y){return function(_44){return new F(function(){return A(_44,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{if(_3Y>55){return function(_45){return new F(function(){return A(_45,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{return new F(function(){return _3Z([0,_3Y-48|0]);});}}break;case 10:if(48>_3Y){return function(_46){return new F(function(){return A(_46,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{if(_3Y>57){return function(_47){return new F(function(){return A(_47,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{return new F(function(){return _3Z([0,_3Y-48|0]);});}}break;case 16:if(48>_3Y){if(97>_3Y){if(65>_3Y){return function(_48){return new F(function(){return A(_48,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{if(_3Y>70){return function(_49){return new F(function(){return A(_49,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{return new F(function(){return _3Z([0,(_3Y-65|0)+10|0]);});}}}else{if(_3Y>102){if(65>_3Y){return function(_4a){return new F(function(){return A(_4a,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{if(_3Y>70){return function(_4b){return new F(function(){return A(_4b,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{return new F(function(){return _3Z([0,(_3Y-65|0)+10|0]);});}}}else{return new F(function(){return _3Z([0,(_3Y-97|0)+10|0]);});}}}else{if(_3Y>57){if(97>_3Y){if(65>_3Y){return function(_4c){return new F(function(){return A(_4c,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{if(_3Y>70){return function(_4d){return new F(function(){return A(_4d,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{return new F(function(){return _3Z([0,(_3Y-65|0)+10|0]);});}}}else{if(_3Y>102){if(65>_3Y){return function(_4e){return new F(function(){return A(_4e,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{if(_3Y>70){return function(_4f){return new F(function(){return A(_4f,[new T(function(){return B(A(_3V,[_0]));})]);});};}else{return new F(function(){return _3Z([0,(_3Y-65|0)+10|0]);});}}}else{return new F(function(){return _3Z([0,(_3Y-97|0)+10|0]);});}}}else{return new F(function(){return _3Z([0,_3Y-48|0]);});}}break;default:return E(_3P);}}};return function(_4g){return new F(function(){return A(_3T,[_4g,_3M,function(_4h){var _4i=E(_4h);return _4i[0]==0?[2]:B(A(_3S,[_4i]));}]);});};},_4j=[0,10],_4k=[0,1],_4l=[0,2147483647],_4m=function(_4n,_4o){while(1){var _4p=E(_4n);if(!_4p[0]){var _4q=_4p[1],_4r=E(_4o);if(!_4r[0]){var _4s=_4r[1],_4t=addC(_4q,_4s);if(!E(_4t[2])){return [0,_4t[1]];}else{_4n=[1,I_fromInt(_4q)];_4o=[1,I_fromInt(_4s)];continue;}}else{_4n=[1,I_fromInt(_4q)];_4o=_4r;continue;}}else{var _4u=E(_4o);if(!_4u[0]){_4n=_4p;_4o=[1,I_fromInt(_4u[1])];continue;}else{return [1,I_add(_4p[1],_4u[1])];}}}},_4v=new T(function(){return B(_4m(_4l,_4k));}),_4w=function(_4x){var _4y=E(_4x);if(!_4y[0]){var _4z=E(_4y[1]);return _4z==(-2147483648)?E(_4v):[0, -_4z];}else{return [1,I_negate(_4y[1])];}},_4A=[0,10],_4B=[0,0],_4C=function(_4D){return [0,_4D];},_4E=function(_4F,_4G){while(1){var _4H=E(_4F);if(!_4H[0]){var _4I=_4H[1],_4J=E(_4G);if(!_4J[0]){var _4K=_4J[1];if(!(imul(_4I,_4K)|0)){return [0,imul(_4I,_4K)|0];}else{_4F=[1,I_fromInt(_4I)];_4G=[1,I_fromInt(_4K)];continue;}}else{_4F=[1,I_fromInt(_4I)];_4G=_4J;continue;}}else{var _4L=E(_4G);if(!_4L[0]){_4F=_4H;_4G=[1,I_fromInt(_4L[1])];continue;}else{return [1,I_mul(_4H[1],_4L[1])];}}}},_4M=function(_4N,_4O,_4P){while(1){var _4Q=E(_4P);if(!_4Q[0]){return E(_4O);}else{var _4R=B(_4m(B(_4E(_4O,_4N)),B(_4C(E(_4Q[1])[1]))));_4P=_4Q[2];_4O=_4R;continue;}}},_4S=function(_4T){var _4U=new T(function(){return B(_1Z(B(_1Z([0,function(_4V){return E(E(_4V)[1])==45?[1,B(_3Q(_4j,function(_4W){return new F(function(){return A(_4T,[[1,new T(function(){return B(_4w(B(_4M(_4A,_4B,_4W))));})]]);});}))]:[2];}],[0,function(_4X){return E(E(_4X)[1])==43?[1,B(_3Q(_4j,function(_4Y){return new F(function(){return A(_4T,[[1,new T(function(){return B(_4M(_4A,_4B,_4Y));})]]);});}))]:[2];}])),new T(function(){return [1,B(_3Q(_4j,function(_4Z){return new F(function(){return A(_4T,[[1,new T(function(){return B(_4M(_4A,_4B,_4Z));})]]);});}))];})));});return new F(function(){return _1Z([0,function(_50){return E(E(_50)[1])==101?E(_4U):[2];}],[0,function(_51){return E(E(_51)[1])==69?E(_4U):[2];}]);});},_52=[0],_53=function(_54){return new F(function(){return A(_54,[_52]);});},_55=function(_56){return new F(function(){return A(_56,[_52]);});},_57=function(_58){return function(_59){return E(E(_59)[1])==46?[1,B(_3Q(_4j,function(_5a){return new F(function(){return A(_58,[[1,_5a]]);});}))]:[2];};},_5b=function(_5c){return [0,B(_57(_5c))];},_5d=function(_5e){return new F(function(){return _3Q(_4j,function(_5f){return [1,B(_3e(_5b,_53,function(_5g){return [1,B(_3e(_4S,_55,function(_5h){return new F(function(){return A(_5e,[[5,[1,_5f,_5g,_5h]]]);});}))];}))];});});},_5i=function(_5j){return [1,B(_5d(_5j))];},_5k=function(_5l){return E(E(_5l)[1]);},_5m=function(_5n,_5o,_5p){while(1){var _5q=E(_5p);if(!_5q[0]){return false;}else{if(!B(A(_5k,[_5n,_5o,_5q[1]]))){_5p=_5q[2];continue;}else{return true;}}}},_5r=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_5s=function(_5t){return new F(function(){return _5m(_2L,_5t,_5r);});},_5u=[0,8],_5v=[0,16],_5w=function(_5x){var _5y=function(_5z){return new F(function(){return A(_5x,[[5,[0,_5u,_5z]]]);});},_5A=function(_5B){return new F(function(){return A(_5x,[[5,[0,_5v,_5B]]]);});};return function(_5C){return E(E(_5C)[1])==48?E([0,function(_5D){switch(E(E(_5D)[1])){case 79:return [1,B(_3Q(_5u,_5y))];case 88:return [1,B(_3Q(_5v,_5A))];case 111:return [1,B(_3Q(_5u,_5y))];case 120:return [1,B(_3Q(_5v,_5A))];default:return [2];}}]):[2];};},_5E=function(_5F){return [0,B(_5w(_5F))];},_5G=false,_5H=true,_5I=function(_5J){var _5K=new T(function(){return B(A(_5J,[_5u]));}),_5L=new T(function(){return B(A(_5J,[_5v]));});return function(_5M){switch(E(E(_5M)[1])){case 79:return E(_5K);case 88:return E(_5L);case 111:return E(_5K);case 120:return E(_5L);default:return [2];}};},_5N=function(_5O){return [0,B(_5I(_5O))];},_5P=[0,92],_5Q=function(_5R){return new F(function(){return A(_5R,[_4j]);});},_5S=function(_5T,_5U){var _5V=jsShowI(_5T),_5W=_5V;return new F(function(){return _t(fromJSStr(_5W),_5U);});},_5X=[0,41],_5Y=[0,40],_5Z=function(_60,_61,_62){if(_61>=0){return new F(function(){return _5S(_61,_62);});}else{return _60<=6?B(_5S(_61,_62)):[1,_5Y,new T(function(){var _63=jsShowI(_61),_64=_63;return B(_t(fromJSStr(_64),[1,_5X,_62]));})];}},_65=function(_66){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_5Z(9,_66,_0));}))));});},_67=function(_68){var _69=E(_68);return _69[0]==0?E(_69[1]):I_toInt(_69[1]);},_6a=function(_6b,_6c){var _6d=E(_6b);if(!_6d[0]){var _6e=_6d[1],_6f=E(_6c);return _6f[0]==0?_6e<=_6f[1]:I_compareInt(_6f[1],_6e)>=0;}else{var _6g=_6d[1],_6h=E(_6c);return _6h[0]==0?I_compareInt(_6g,_6h[1])<=0:I_compare(_6g,_6h[1])<=0;}},_6i=function(_6j){return [2];},_6k=function(_6l){var _6m=E(_6l);if(!_6m[0]){return E(_6i);}else{var _6n=_6m[1],_6o=E(_6m[2]);return _6o[0]==0?E(_6n):function(_6p){return new F(function(){return _1Z(B(A(_6n,[_6p])),new T(function(){return B(A(new T(function(){return B(_6k(_6o));}),[_6p]));}));});};}},_6q=function(_6r){return [2];},_6s=function(_6t,_6u){var _6v=function(_6w,_6x){var _6y=E(_6w);if(!_6y[0]){return function(_6z){return new F(function(){return A(_6z,[_6t]);});};}else{var _6A=E(_6x);return _6A[0]==0?E(_6q):E(_6y[1])[1]!=E(_6A[1])[1]?E(_6q):function(_6B){return [0,function(_6C){return E(new T(function(){return B(A(new T(function(){return B(_6v(_6y[2],_6A[2]));}),[_6B]));}));}];};}};return function(_6D){return new F(function(){return A(_6v,[_6t,_6D,_6u]);});};},_6E=new T(function(){return B(unCStr("SOH"));}),_6F=[0,1],_6G=function(_6H){return [1,B(_6s(_6E,function(_6I){return E(new T(function(){return B(A(_6H,[_6F]));}));}))];},_6J=new T(function(){return B(unCStr("SO"));}),_6K=[0,14],_6L=function(_6M){return [1,B(_6s(_6J,function(_6N){return E(new T(function(){return B(A(_6M,[_6K]));}));}))];},_6O=function(_6P){return [1,B(_3e(_6G,_6L,_6P))];},_6Q=new T(function(){return B(unCStr("NUL"));}),_6R=[0,0],_6S=function(_6T){return [1,B(_6s(_6Q,function(_6U){return E(new T(function(){return B(A(_6T,[_6R]));}));}))];},_6V=new T(function(){return B(unCStr("STX"));}),_6W=[0,2],_6X=function(_6Y){return [1,B(_6s(_6V,function(_6Z){return E(new T(function(){return B(A(_6Y,[_6W]));}));}))];},_70=new T(function(){return B(unCStr("ETX"));}),_71=[0,3],_72=function(_73){return [1,B(_6s(_70,function(_74){return E(new T(function(){return B(A(_73,[_71]));}));}))];},_75=new T(function(){return B(unCStr("EOT"));}),_76=[0,4],_77=function(_78){return [1,B(_6s(_75,function(_79){return E(new T(function(){return B(A(_78,[_76]));}));}))];},_7a=new T(function(){return B(unCStr("ENQ"));}),_7b=[0,5],_7c=function(_7d){return [1,B(_6s(_7a,function(_7e){return E(new T(function(){return B(A(_7d,[_7b]));}));}))];},_7f=new T(function(){return B(unCStr("ACK"));}),_7g=[0,6],_7h=function(_7i){return [1,B(_6s(_7f,function(_7j){return E(new T(function(){return B(A(_7i,[_7g]));}));}))];},_7k=new T(function(){return B(unCStr("BEL"));}),_7l=[0,7],_7m=function(_7n){return [1,B(_6s(_7k,function(_7o){return E(new T(function(){return B(A(_7n,[_7l]));}));}))];},_7p=new T(function(){return B(unCStr("BS"));}),_7q=[0,8],_7r=function(_7s){return [1,B(_6s(_7p,function(_7t){return E(new T(function(){return B(A(_7s,[_7q]));}));}))];},_7u=new T(function(){return B(unCStr("HT"));}),_7v=[0,9],_7w=function(_7x){return [1,B(_6s(_7u,function(_7y){return E(new T(function(){return B(A(_7x,[_7v]));}));}))];},_7z=new T(function(){return B(unCStr("LF"));}),_7A=[0,10],_7B=function(_7C){return [1,B(_6s(_7z,function(_7D){return E(new T(function(){return B(A(_7C,[_7A]));}));}))];},_7E=new T(function(){return B(unCStr("VT"));}),_7F=[0,11],_7G=function(_7H){return [1,B(_6s(_7E,function(_7I){return E(new T(function(){return B(A(_7H,[_7F]));}));}))];},_7J=new T(function(){return B(unCStr("FF"));}),_7K=[0,12],_7L=function(_7M){return [1,B(_6s(_7J,function(_7N){return E(new T(function(){return B(A(_7M,[_7K]));}));}))];},_7O=new T(function(){return B(unCStr("CR"));}),_7P=[0,13],_7Q=function(_7R){return [1,B(_6s(_7O,function(_7S){return E(new T(function(){return B(A(_7R,[_7P]));}));}))];},_7T=new T(function(){return B(unCStr("SI"));}),_7U=[0,15],_7V=function(_7W){return [1,B(_6s(_7T,function(_7X){return E(new T(function(){return B(A(_7W,[_7U]));}));}))];},_7Y=new T(function(){return B(unCStr("DLE"));}),_7Z=[0,16],_80=function(_81){return [1,B(_6s(_7Y,function(_82){return E(new T(function(){return B(A(_81,[_7Z]));}));}))];},_83=new T(function(){return B(unCStr("DC1"));}),_84=[0,17],_85=function(_86){return [1,B(_6s(_83,function(_87){return E(new T(function(){return B(A(_86,[_84]));}));}))];},_88=new T(function(){return B(unCStr("DC2"));}),_89=[0,18],_8a=function(_8b){return [1,B(_6s(_88,function(_8c){return E(new T(function(){return B(A(_8b,[_89]));}));}))];},_8d=new T(function(){return B(unCStr("DC3"));}),_8e=[0,19],_8f=function(_8g){return [1,B(_6s(_8d,function(_8h){return E(new T(function(){return B(A(_8g,[_8e]));}));}))];},_8i=new T(function(){return B(unCStr("DC4"));}),_8j=[0,20],_8k=function(_8l){return [1,B(_6s(_8i,function(_8m){return E(new T(function(){return B(A(_8l,[_8j]));}));}))];},_8n=new T(function(){return B(unCStr("NAK"));}),_8o=[0,21],_8p=function(_8q){return [1,B(_6s(_8n,function(_8r){return E(new T(function(){return B(A(_8q,[_8o]));}));}))];},_8s=new T(function(){return B(unCStr("SYN"));}),_8t=[0,22],_8u=function(_8v){return [1,B(_6s(_8s,function(_8w){return E(new T(function(){return B(A(_8v,[_8t]));}));}))];},_8x=new T(function(){return B(unCStr("ETB"));}),_8y=[0,23],_8z=function(_8A){return [1,B(_6s(_8x,function(_8B){return E(new T(function(){return B(A(_8A,[_8y]));}));}))];},_8C=new T(function(){return B(unCStr("CAN"));}),_8D=[0,24],_8E=function(_8F){return [1,B(_6s(_8C,function(_8G){return E(new T(function(){return B(A(_8F,[_8D]));}));}))];},_8H=new T(function(){return B(unCStr("EM"));}),_8I=[0,25],_8J=function(_8K){return [1,B(_6s(_8H,function(_8L){return E(new T(function(){return B(A(_8K,[_8I]));}));}))];},_8M=new T(function(){return B(unCStr("SUB"));}),_8N=[0,26],_8O=function(_8P){return [1,B(_6s(_8M,function(_8Q){return E(new T(function(){return B(A(_8P,[_8N]));}));}))];},_8R=new T(function(){return B(unCStr("ESC"));}),_8S=[0,27],_8T=function(_8U){return [1,B(_6s(_8R,function(_8V){return E(new T(function(){return B(A(_8U,[_8S]));}));}))];},_8W=new T(function(){return B(unCStr("FS"));}),_8X=[0,28],_8Y=function(_8Z){return [1,B(_6s(_8W,function(_90){return E(new T(function(){return B(A(_8Z,[_8X]));}));}))];},_91=new T(function(){return B(unCStr("GS"));}),_92=[0,29],_93=function(_94){return [1,B(_6s(_91,function(_95){return E(new T(function(){return B(A(_94,[_92]));}));}))];},_96=new T(function(){return B(unCStr("RS"));}),_97=[0,30],_98=function(_99){return [1,B(_6s(_96,function(_9a){return E(new T(function(){return B(A(_99,[_97]));}));}))];},_9b=new T(function(){return B(unCStr("US"));}),_9c=[0,31],_9d=function(_9e){return [1,B(_6s(_9b,function(_9f){return E(new T(function(){return B(A(_9e,[_9c]));}));}))];},_9g=new T(function(){return B(unCStr("SP"));}),_9h=[0,32],_9i=function(_9j){return [1,B(_6s(_9g,function(_9k){return E(new T(function(){return B(A(_9j,[_9h]));}));}))];},_9l=new T(function(){return B(unCStr("DEL"));}),_9m=[0,127],_9n=function(_9o){return [1,B(_6s(_9l,function(_9p){return E(new T(function(){return B(A(_9o,[_9m]));}));}))];},_9q=[1,_9n,_0],_9r=[1,_9i,_9q],_9s=[1,_9d,_9r],_9t=[1,_98,_9s],_9u=[1,_93,_9t],_9v=[1,_8Y,_9u],_9w=[1,_8T,_9v],_9x=[1,_8O,_9w],_9y=[1,_8J,_9x],_9z=[1,_8E,_9y],_9A=[1,_8z,_9z],_9B=[1,_8u,_9A],_9C=[1,_8p,_9B],_9D=[1,_8k,_9C],_9E=[1,_8f,_9D],_9F=[1,_8a,_9E],_9G=[1,_85,_9F],_9H=[1,_80,_9G],_9I=[1,_7V,_9H],_9J=[1,_7Q,_9I],_9K=[1,_7L,_9J],_9L=[1,_7G,_9K],_9M=[1,_7B,_9L],_9N=[1,_7w,_9M],_9O=[1,_7r,_9N],_9P=[1,_7m,_9O],_9Q=[1,_7h,_9P],_9R=[1,_7c,_9Q],_9S=[1,_77,_9R],_9T=[1,_72,_9S],_9U=[1,_6X,_9T],_9V=[1,_6S,_9U],_9W=[1,_6O,_9V],_9X=new T(function(){return B(_6k(_9W));}),_9Y=[0,1114111],_9Z=[0,34],_a0=[0,39],_a1=function(_a2){var _a3=new T(function(){return B(A(_a2,[_7l]));}),_a4=new T(function(){return B(A(_a2,[_7q]));}),_a5=new T(function(){return B(A(_a2,[_7v]));}),_a6=new T(function(){return B(A(_a2,[_7A]));}),_a7=new T(function(){return B(A(_a2,[_7F]));}),_a8=new T(function(){return B(A(_a2,[_7K]));}),_a9=new T(function(){return B(A(_a2,[_7P]));});return new F(function(){return _1Z([0,function(_aa){switch(E(E(_aa)[1])){case 34:return E(new T(function(){return B(A(_a2,[_9Z]));}));case 39:return E(new T(function(){return B(A(_a2,[_a0]));}));case 92:return E(new T(function(){return B(A(_a2,[_5P]));}));case 97:return E(_a3);case 98:return E(_a4);case 102:return E(_a8);case 110:return E(_a6);case 114:return E(_a9);case 116:return E(_a5);case 118:return E(_a7);default:return [2];}}],new T(function(){return B(_1Z([1,B(_3e(_5N,_5Q,function(_ab){return [1,B(_3Q(_ab,function(_ac){var _ad=B(_4M(new T(function(){return B(_4C(E(_ab)[1]));}),_4B,_ac));return !B(_6a(_ad,_9Y))?[2]:B(A(_a2,[new T(function(){var _ae=B(_67(_ad));if(_ae>>>0>1114111){var _af=B(_65(_ae));}else{var _af=[0,_ae];}var _ag=_af,_ah=_ag,_ai=_ah;return _ai;})]));}))];}))],new T(function(){return B(_1Z([0,function(_aj){return E(E(_aj)[1])==94?E([0,function(_ak){switch(E(E(_ak)[1])){case 64:return E(new T(function(){return B(A(_a2,[_6R]));}));case 65:return E(new T(function(){return B(A(_a2,[_6F]));}));case 66:return E(new T(function(){return B(A(_a2,[_6W]));}));case 67:return E(new T(function(){return B(A(_a2,[_71]));}));case 68:return E(new T(function(){return B(A(_a2,[_76]));}));case 69:return E(new T(function(){return B(A(_a2,[_7b]));}));case 70:return E(new T(function(){return B(A(_a2,[_7g]));}));case 71:return E(_a3);case 72:return E(_a4);case 73:return E(_a5);case 74:return E(_a6);case 75:return E(_a7);case 76:return E(_a8);case 77:return E(_a9);case 78:return E(new T(function(){return B(A(_a2,[_6K]));}));case 79:return E(new T(function(){return B(A(_a2,[_7U]));}));case 80:return E(new T(function(){return B(A(_a2,[_7Z]));}));case 81:return E(new T(function(){return B(A(_a2,[_84]));}));case 82:return E(new T(function(){return B(A(_a2,[_89]));}));case 83:return E(new T(function(){return B(A(_a2,[_8e]));}));case 84:return E(new T(function(){return B(A(_a2,[_8j]));}));case 85:return E(new T(function(){return B(A(_a2,[_8o]));}));case 86:return E(new T(function(){return B(A(_a2,[_8t]));}));case 87:return E(new T(function(){return B(A(_a2,[_8y]));}));case 88:return E(new T(function(){return B(A(_a2,[_8D]));}));case 89:return E(new T(function(){return B(A(_a2,[_8I]));}));case 90:return E(new T(function(){return B(A(_a2,[_8N]));}));case 91:return E(new T(function(){return B(A(_a2,[_8S]));}));case 92:return E(new T(function(){return B(A(_a2,[_8X]));}));case 93:return E(new T(function(){return B(A(_a2,[_92]));}));case 94:return E(new T(function(){return B(A(_a2,[_97]));}));case 95:return E(new T(function(){return B(A(_a2,[_9c]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_9X,[_a2]));})));})));}));});},_al=function(_am){return new F(function(){return A(_am,[_3]);});},_an=function(_ao){var _ap=E(_ao);if(!_ap[0]){return E(_al);}else{var _aq=_ap[2],_ar=E(E(_ap[1])[1]);switch(_ar){case 9:return function(_as){return [0,function(_at){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_as]));}));}];};case 10:return function(_au){return [0,function(_av){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_au]));}));}];};case 11:return function(_aw){return [0,function(_ax){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_aw]));}));}];};case 12:return function(_ay){return [0,function(_az){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_ay]));}));}];};case 13:return function(_aA){return [0,function(_aB){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_aA]));}));}];};case 32:return function(_aC){return [0,function(_aD){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_aC]));}));}];};case 160:return function(_aE){return [0,function(_aF){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_aE]));}));}];};default:var _aG=u_iswspace(_ar),_aH=_aG;return E(_aH)==0?E(_al):function(_aI){return [0,function(_aJ){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq));}),[_aI]));}));}];};}}},_aK=function(_aL){var _aM=new T(function(){return B(_aK(_aL));}),_aN=[1,function(_aO){return new F(function(){return A(_an,[_aO,function(_aP){return E([0,function(_aQ){return E(E(_aQ)[1])==92?E(_aM):[2];}]);}]);});}];return new F(function(){return _1Z([0,function(_aR){return E(E(_aR)[1])==92?E([0,function(_aS){var _aT=E(E(_aS)[1]);switch(_aT){case 9:return E(_aN);case 10:return E(_aN);case 11:return E(_aN);case 12:return E(_aN);case 13:return E(_aN);case 32:return E(_aN);case 38:return E(_aM);case 160:return E(_aN);default:var _aU=u_iswspace(_aT),_aV=_aU;return E(_aV)==0?[2]:E(_aN);}}]):[2];}],[0,function(_aW){var _aX=E(_aW);return E(_aX[1])==92?E(new T(function(){return B(_a1(function(_aY){return new F(function(){return A(_aL,[[0,_aY,_5H]]);});}));})):B(A(_aL,[[0,_aX,_5G]]));}]);});},_aZ=function(_b0,_b1){return new F(function(){return _aK(function(_b2){var _b3=E(_b2),_b4=E(_b3[1]);if(E(_b4[1])==34){if(!E(_b3[2])){return E(new T(function(){return B(A(_b1,[[1,new T(function(){return B(A(_b0,[_0]));})]]));}));}else{return new F(function(){return _aZ(function(_b5){return new F(function(){return A(_b0,[[1,_b4,_b5]]);});},_b1);});}}else{return new F(function(){return _aZ(function(_b6){return new F(function(){return A(_b0,[[1,_b4,_b6]]);});},_b1);});}});});},_b7=new T(function(){return B(unCStr("_\'"));}),_b8=function(_b9){var _ba=u_iswalnum(_b9),_bb=_ba;return E(_bb)==0?B(_5m(_2L,[0,_b9],_b7)):true;},_bc=function(_bd){return new F(function(){return _b8(E(_bd)[1]);});},_be=new T(function(){return B(unCStr(",;()[]{}`"));}),_bf=new T(function(){return B(unCStr(".."));}),_bg=new T(function(){return B(unCStr("::"));}),_bh=new T(function(){return B(unCStr("->"));}),_bi=[0,64],_bj=[1,_bi,_0],_bk=[0,126],_bl=[1,_bk,_0],_bm=new T(function(){return B(unCStr("=>"));}),_bn=[1,_bm,_0],_bo=[1,_bl,_bn],_bp=[1,_bj,_bo],_bq=[1,_bh,_bp],_br=new T(function(){return B(unCStr("<-"));}),_bs=[1,_br,_bq],_bt=[0,124],_bu=[1,_bt,_0],_bv=[1,_bu,_bs],_bw=[1,_5P,_0],_bx=[1,_bw,_bv],_by=[0,61],_bz=[1,_by,_0],_bA=[1,_bz,_bx],_bB=[1,_bg,_bA],_bC=[1,_bf,_bB],_bD=function(_bE){return new F(function(){return _1Z([1,function(_bF){return E(_bF)[0]==0?E(new T(function(){return B(A(_bE,[_3L]));})):[2];}],new T(function(){return B(_1Z([0,function(_bG){return E(E(_bG)[1])==39?E([0,function(_bH){var _bI=E(_bH);switch(E(_bI[1])){case 39:return [2];case 92:return E(new T(function(){return B(_a1(function(_bJ){return [0,function(_bK){return E(E(_bK)[1])==39?E(new T(function(){return B(A(_bE,[[0,_bJ]]));})):[2];}];}));}));default:return [0,function(_bL){return E(E(_bL)[1])==39?E(new T(function(){return B(A(_bE,[[0,_bI]]));})):[2];}];}}]):[2];}],new T(function(){return B(_1Z([0,function(_bM){return E(E(_bM)[1])==34?E(new T(function(){return B(_aZ(_3M,_bE));})):[2];}],new T(function(){return B(_1Z([0,function(_bN){return !B(_5m(_2L,_bN,_be))?[2]:B(A(_bE,[[2,[1,_bN,_0]]]));}],new T(function(){return B(_1Z([0,function(_bO){return !B(_5m(_2L,_bO,_5r))?[2]:[1,B(_3A(_5s,function(_bP){var _bQ=[1,_bO,_bP];return !B(_5m(_2U,_bQ,_bC))?B(A(_bE,[[4,_bQ]])):B(A(_bE,[[2,_bQ]]));}))];}],new T(function(){return B(_1Z([0,function(_bR){var _bS=E(_bR),_bT=_bS[1],_bU=u_iswalpha(_bT),_bV=_bU;return E(_bV)==0?E(_bT)==95?[1,B(_3A(_bc,function(_bW){return new F(function(){return A(_bE,[[3,[1,_bS,_bW]]]);});}))]:[2]:[1,B(_3A(_bc,function(_bX){return new F(function(){return A(_bE,[[3,[1,_bS,_bX]]]);});}))];}],new T(function(){return [1,B(_3e(_5E,_5i,_bE))];})));})));})));})));})));}));});},_bY=[0,0],_bZ=function(_c0,_c1){return function(_c2){return new F(function(){return A(_an,[_c2,function(_c3){return E(new T(function(){return B(_bD(function(_c4){var _c5=E(_c4);return _c5[0]==2?!B(_2A(_c5[1],_2z))?[2]:E(new T(function(){return B(A(_c0,[_bY,function(_c6){return [1,function(_c7){return new F(function(){return A(_an,[_c7,function(_c8){return E(new T(function(){return B(_bD(function(_c9){var _ca=E(_c9);return _ca[0]==2?!B(_2A(_ca[1],_2x))?[2]:E(new T(function(){return B(A(_c1,[_c6]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_cb=function(_cc){return new F(function(){return _1Z([1,function(_cd){return new F(function(){return A(_an,[_cd,function(_ce){return E(new T(function(){return B(_bD(function(_cf){var _cg=E(_cf);return _cg[0]==0?B(A(_cc,[_cg[1]])):[2];}));}));}]);});}],new T(function(){return [1,B(_bZ(_ch,_cc))];}));});},_ch=function(_ci,_cj){return new F(function(){return _cb(_cj);});},_ck=[0,91],_cl=[1,_ck,_0],_cm=function(_cn,_co){var _cp=function(_cq,_cr){return [1,function(_cs){return new F(function(){return A(_an,[_cs,function(_ct){return E(new T(function(){return B(_bD(function(_cu){var _cv=E(_cu);if(_cv[0]==2){var _cw=E(_cv[1]);if(!_cw[0]){return [2];}else{var _cx=_cw[2];switch(E(E(_cw[1])[1])){case 44:return E(_cx)[0]==0?!E(_cq)?[2]:E(new T(function(){return B(A(_cn,[_bY,function(_cy){return new F(function(){return _cp(_5H,function(_cz){return new F(function(){return A(_cr,[[1,_cy,_cz]]);});});});}]));})):[2];case 93:return E(_cx)[0]==0?E(new T(function(){return B(A(_cr,[_0]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_cA=function(_cB){return new F(function(){return _1Z([1,function(_cC){return new F(function(){return A(_an,[_cC,function(_cD){return E(new T(function(){return B(_bD(function(_cE){var _cF=E(_cE);return _cF[0]==2?!B(_2A(_cF[1],_cl))?[2]:E(new T(function(){return B(_1Z(B(_cp(_5G,_cB)),new T(function(){return B(A(_cn,[_bY,function(_cG){return new F(function(){return _cp(_5H,function(_cH){return new F(function(){return A(_cB,[[1,_cG,_cH]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_bZ(function(_cI,_cJ){return new F(function(){return _cA(_cJ);});},_cB))];}));});};return new F(function(){return _cA(_co);});},_2v=function(_cK){return new F(function(){return _1Z(B(_1Z([1,function(_cL){return new F(function(){return A(_an,[_cL,function(_cM){return E(new T(function(){return B(_bD(function(_cN){var _cO=E(_cN);return _cO[0]==1?B(A(_cK,[_cO[1]])):[2];}));}));}]);});}],new T(function(){return B(_cm(_ch,_cK));}))),new T(function(){return [1,B(_bZ(_2s,_cK))];}));});},_cP=function(_cQ,_cR){return _cQ>10?[2]:[1,function(_cS){return new F(function(){return A(_an,[_cS,function(_cT){return E(new T(function(){return B(_bD(function(_cU){var _cV=E(_cU);return _cV[0]==3?!B(_2A(_cV[1],_s))?[2]:E(new T(function(){return B(_2v(function(_cW){return new F(function(){return A(_cR,[_cW]);});}));})):[2];}));}));}]);});}];},_cX=function(_cY,_cZ){return new F(function(){return _cP(E(_cY)[1],_cZ);});},_d0=function(_d1,_d2,_d3){var _d4=function(_d5,_d6){return new F(function(){return _1Z([1,function(_d7){return new F(function(){return A(_an,[_d7,function(_d8){return E(new T(function(){return B(_bD(function(_d9){var _da=E(_d9);if(_da[0]==4){var _db=E(_da[1]);if(!_db[0]){return new F(function(){return A(_d1,[_da,_d5,_d6]);});}else{return E(E(_db[1])[1])==45?E(_db[2])[0]==0?E([1,function(_dc){return new F(function(){return A(_an,[_dc,function(_dd){return E(new T(function(){return B(_bD(function(_de){return new F(function(){return A(_d1,[_de,_d5,function(_df){return new F(function(){return A(_d6,[new T(function(){return [0, -E(_df)[1]];})]);});}]);});}));}));}]);});}]):B(A(_d1,[_da,_d5,_d6])):B(A(_d1,[_da,_d5,_d6]));}}else{return new F(function(){return A(_d1,[_da,_d5,_d6]);});}}));}));}]);});}],new T(function(){return [1,B(_bZ(_d4,_d6))];}));});};return new F(function(){return _d4(_d2,_d3);});},_dg=function(_dh,_di){return [2];},_dj=function(_dk){var _dl=E(_dk);return _dl[0]==0?[1,new T(function(){return B(_4M(new T(function(){return B(_4C(E(_dl[1])[1]));}),_4B,_dl[2]));})]:E(_dl[2])[0]==0?E(_dl[3])[0]==0?[1,new T(function(){return B(_4M(_4A,_4B,_dl[1]));})]:[0]:[0];},_dm=function(_dn){var _do=E(_dn);if(_do[0]==5){var _dp=B(_dj(_do[1]));return _dp[0]==0?E(_dg):function(_dq,_dr){return new F(function(){return A(_dr,[new T(function(){return [0,B(_67(_dp[1]))];})]);});};}else{return E(_dg);}},_ds=new T(function(){return B(unCStr("Pen"));}),_dt=[0,11],_du=function(_dv,_dw){return _dv>10?[2]:[1,function(_dx){return new F(function(){return A(_an,[_dx,function(_dy){return E(new T(function(){return B(_bD(function(_dz){var _dA=E(_dz);return _dA[0]==3?!B(_2A(_dA[1],_ds))?[2]:E(new T(function(){return B(_d0(_dm,_dt,function(_dB){return new F(function(){return A(_dw,[_dB]);});}));})):[2];}));}));}]);});}];},_dC=function(_dD,_dE){return new F(function(){return _du(E(_dD)[1],_dE);});},_dF=function(_dG,_dH){return new F(function(){return _dI(_dH);});},_dJ=new T(function(){return B(unCStr("True"));}),_dK=new T(function(){return B(unCStr("False"));}),_dL=function(_dM){return function(_dN){return new F(function(){return A(_an,[_dN,function(_dO){return E(new T(function(){return B(_bD(function(_dP){var _dQ=E(_dP);if(_dQ[0]==3){var _dR=_dQ[1];return !B(_2A(_dR,_dK))?!B(_2A(_dR,_dJ))?[2]:E(new T(function(){return B(A(_dM,[_5H]));})):E(new T(function(){return B(A(_dM,[_5G]));}));}else{return [2];}}));}));}]);});};},_dI=function(_dS){return new F(function(){return _1Z([1,B(_dL(_dS))],new T(function(){return [1,B(_bZ(_dF,_dS))];}));});},_dT=new T(function(){return B(unCStr("CowState"));}),_dU=[0,123],_dV=[1,_dU,_0],_dW=new T(function(){return B(unCStr("pen1"));}),_dX=[0,61],_dY=[1,_dX,_0],_dZ=[0,44],_e0=[1,_dZ,_0],_e1=new T(function(){return B(unCStr("pen2"));}),_e2=new T(function(){return B(unCStr("lastPen"));}),_e3=new T(function(){return B(unCStr("lastBox"));}),_e4=new T(function(){return B(unCStr("is60"));}),_e5=[0,125],_e6=[1,_e5,_0],_e7=function(_e8,_e9){var _ea=function(_eb){return function(_ec){return new F(function(){return _1Z(B(A(new T(function(){return B(A(_e8,[_eb]));}),[_ec])),new T(function(){return [1,B(_bZ(_ea,_ec))];}));});};};return new F(function(){return _ea(_e9);});},_ed=function(_ee,_ef){return _ee>11?[2]:[1,function(_eg){return new F(function(){return A(_an,[_eg,function(_eh){return E(new T(function(){return B(_bD(function(_ei){var _ej=E(_ei);return _ej[0]==3?!B(_2A(_ej[1],_dT))?[2]:E([1,function(_ek){return new F(function(){return A(_an,[_ek,function(_el){return E(new T(function(){return B(_bD(function(_em){var _en=E(_em);return _en[0]==2?!B(_2A(_en[1],_dV))?[2]:E([1,function(_eo){return new F(function(){return A(_an,[_eo,function(_ep){return E(new T(function(){return B(_bD(function(_eq){var _er=E(_eq);return _er[0]==3?!B(_2A(_er[1],_dW))?[2]:E([1,function(_es){return new F(function(){return A(_an,[_es,function(_et){return E(new T(function(){return B(_bD(function(_eu){var _ev=E(_eu);return _ev[0]==2?!B(_2A(_ev[1],_dY))?[2]:E(new T(function(){return B(A(_e7,[_cX,_bY,function(_ew){return [1,function(_ex){return new F(function(){return A(_an,[_ex,function(_ey){return E(new T(function(){return B(_bD(function(_ez){var _eA=E(_ez);return _eA[0]==2?!B(_2A(_eA[1],_e0))?[2]:E([1,function(_eB){return new F(function(){return A(_an,[_eB,function(_eC){return E(new T(function(){return B(_bD(function(_eD){var _eE=E(_eD);return _eE[0]==3?!B(_2A(_eE[1],_e1))?[2]:E([1,function(_eF){return new F(function(){return A(_an,[_eF,function(_eG){return E(new T(function(){return B(_bD(function(_eH){var _eI=E(_eH);return _eI[0]==2?!B(_2A(_eI[1],_dY))?[2]:E(new T(function(){return B(A(_e7,[_cX,_bY,function(_eJ){return [1,function(_eK){return new F(function(){return A(_an,[_eK,function(_eL){return E(new T(function(){return B(_bD(function(_eM){var _eN=E(_eM);return _eN[0]==2?!B(_2A(_eN[1],_e0))?[2]:E([1,function(_eO){return new F(function(){return A(_an,[_eO,function(_eP){return E(new T(function(){return B(_bD(function(_eQ){var _eR=E(_eQ);return _eR[0]==3?!B(_2A(_eR[1],_e2))?[2]:E([1,function(_eS){return new F(function(){return A(_an,[_eS,function(_eT){return E(new T(function(){return B(_bD(function(_eU){var _eV=E(_eU);return _eV[0]==2?!B(_2A(_eV[1],_dY))?[2]:E(new T(function(){return B(A(_e7,[_dC,_bY,function(_eW){return [1,function(_eX){return new F(function(){return A(_an,[_eX,function(_eY){return E(new T(function(){return B(_bD(function(_eZ){var _f0=E(_eZ);return _f0[0]==2?!B(_2A(_f0[1],_e0))?[2]:E([1,function(_f1){return new F(function(){return A(_an,[_f1,function(_f2){return E(new T(function(){return B(_bD(function(_f3){var _f4=E(_f3);return _f4[0]==3?!B(_2A(_f4[1],_e3))?[2]:E([1,function(_f5){return new F(function(){return A(_an,[_f5,function(_f6){return E(new T(function(){return B(_bD(function(_f7){var _f8=E(_f7);return _f8[0]==2?!B(_2A(_f8[1],_dY))?[2]:E(new T(function(){return B(A(_e7,[_cX,_bY,function(_f9){return [1,function(_fa){return new F(function(){return A(_an,[_fa,function(_fb){return E(new T(function(){return B(_bD(function(_fc){var _fd=E(_fc);return _fd[0]==2?!B(_2A(_fd[1],_e0))?[2]:E([1,function(_fe){return new F(function(){return A(_an,[_fe,function(_ff){return E(new T(function(){return B(_bD(function(_fg){var _fh=E(_fg);return _fh[0]==3?!B(_2A(_fh[1],_e4))?[2]:E([1,function(_fi){return new F(function(){return A(_an,[_fi,function(_fj){return E(new T(function(){return B(_bD(function(_fk){var _fl=E(_fk);return _fl[0]==2?!B(_2A(_fl[1],_dY))?[2]:E(new T(function(){return B(_dI(function(_fm){return [1,function(_fn){return new F(function(){return A(_an,[_fn,function(_fo){return E(new T(function(){return B(_bD(function(_fp){var _fq=E(_fp);return _fq[0]==2?!B(_2A(_fq[1],_e6))?[2]:E(new T(function(){return B(A(_ef,[[0,_ew,_eJ,_eW,_f9,_fm]]));})):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];},_fr=function(_fs,_ft){return new F(function(){return _ed(E(_fs)[1],_ft);});},_fu=function(_fv){return [1,function(_fw){return new F(function(){return A(_an,[_fw,function(_fx){return E([3,_fv,_36]);}]);});}];},_fy=new T(function(){return B(A(_e7,[_fr,_bY,_fu]));}),_fz=function(_fA){while(1){var _fB=(function(_fC){var _fD=E(_fC);if(!_fD[0]){return [0];}else{var _fE=_fD[2],_fF=E(_fD[1]);if(!E(_fF[2])[0]){return [1,_fF[1],new T(function(){return B(_fz(_fE));})];}else{_fA=_fE;return null;}}})(_fA);if(_fB!=null){return _fB;}}},_fG=function(_fH,_){var _fI=B(_fz(B(_1P(_fy,_fH))));if(!_fI[0]){return E(_r);}else{if(!E(_fI[2])[0]){var _fJ=E(_fI[1]),_fK=B(A(_h,[E(_p)[1],E(toJSStr(E(_fJ[1]))),E(toJSStr(E(_fJ[2]))),E(E(_fJ[3])[1]),E(toJSStr(E(_fJ[4]))),!E(_fJ[5])?E(_k):E(_m),_])),_fL=_fK;return _3;}else{return E(_o);}}},_fM=new T(function(){return [0,"(function(s,f){Haste[s] = f;})"];}),_fN=new T(function(){return B(_h(E(_fM)[1]));}),_fO=function(_fP,_fQ){return function(_fR,_){var _fS=B(A(new T(function(){return B(A(_fN,[E(E(_fQ)[1])]));}),[B(A(_fP,[_fR])),_])),_fT=_fS;return _3;};},_fU=new T(function(){return [0,"restoreState"];}),_fV=function(_fW){var _fX=String(_fW),_fY=_fX;return new F(function(){return fromJSStr(_fY);});},_fZ=new T(function(){return E(0);}),_g0=new T(function(){return B(_h("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_g1=function(_g2,_){return new F(function(){return A(_g0,[E(_g2),_]);});},_g3=function(_g4,_){return new F(function(){return _g1(_g4,_);});},_g5=function(_g6){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _g3(function(_g7){return new F(function(){return _d(function(_){var _=0,_g8=B(A(_g6,[B(_fV(_g7)),_])),_g9=_g8;return E(_fZ);});});},_);});});});},_ga=new T(function(){return B(_fO(_g5,_fU));}),_gb=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_gc=new T(function(){return B(err(_gb));}),_gd=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_ge=new T(function(){return B(err(_gd));}),_gf=function(_gg,_gh){while(1){var _gi=E(_gg);if(!_gi[0]){return E(_ge);}else{var _gj=E(_gh);if(!_gj){return E(_gi[1]);}else{_gg=_gi[2];_gh=_gj-1|0;continue;}}}},_gk=function(_gl,_gm){while(1){var _gn=E(_gl);if(!_gn[0]){return E(_gm);}else{_gl=_gn[2];var _go=_gm+1|0;_gm=_go;continue;}}},_gp=new T(function(){return B(_h("(function(m){    return (function() {        return (function(){return E(B(A(m,[0])));});      });})"));}),_gq=function(_gr,_){return new F(function(){return A(_gp,[E(_gr),_]);});},_gs=function(_g4,_){return new F(function(){return _gq(_g4,_);});},_gt=function(_){var _gu=B(_gv(_)),_gw=_gu;return _fZ;},_gx=function(_){var _=0;return new F(function(){return _gs(_gt,_);});},_gy=new T(function(){return [0,"arr2lst"];}),_gz=function(_gA,_gB){return new F(function(){return _d(function(_){var _=0;return new F(function(){return A(_h,[E(_gy)[1],E(_gA),E(_gB),_]);});});});},_gC=function(_gD,_gE){var _gF=E(_gE);return _gF[0]==0?[0]:[1,new T(function(){return B(A(_gD,[_gF[1]]));}),new T(function(){return B(_gC(_gD,_gF[2]));})];},_gG=function(_){var _gH=B(A(_h,["(function(){ var out = []; for (var n in appl.clickers) { var c = appl.clickers[n], p = c.box.pens_held, top = (c.top_or_bottom === \'top\'); if (c.box && (p[1] && top) || (p[2] && !top)) out.push(n); }; return out; })",_])),_gI=_gH;return new T(function(){return B(_gC(_fV,B(_gz(_gI,0))));});},_gJ=new T(function(){return B(_h("(function(m) { appl.clickers[m].init_poise().execute() })"));}),_gv=function(_){var _gK=B(_gG(_)),_gL=_gK,_gM=E(_gL);if(!_gM[0]){return _3;}else{var _gN=B(A(_h,["(Math.random)",_])),_gO=_gN,_gP=B(_gk(_gM,0))*_gO,_gQ=_gP&4294967295,_gR=function(_gS){if(_gS>=0){var _gT=B(A(_gJ,[E(toJSStr(B(_gf(_gM,_gS)))),_])),_gU=_gT,_gV=B(A(_h,["(function(n, action) { setTimeout(action, n); })",E(1000),B(_d(_gx)),_])),_gW=_gV;return _3;}else{return E(_gc);}};if(_gP>=_gQ){return new F(function(){return _gR(_gQ);});}else{return new F(function(){return _gR(_gQ-1|0);});}}},_gX=function(_gY){return new F(function(){return _1M("src/Haste/Foreign.hs:117:12-81|case");});},_gZ=new T(function(){return B(_gX(_));}),_h0=function(_h1){var _h2=jsRound(_h1),_h3=_h2;return [0,_h3];},_h4=new T(function(){return B(unCStr("CowState {"));}),_h5=new T(function(){return B(unCStr("is60 = "));}),_h6=new T(function(){return B(unCStr("lastBox = "));}),_h7=new T(function(){return B(unCStr("lastPen = "));}),_h8=new T(function(){return B(unCStr("pen2 = "));}),_h9=new T(function(){return B(unCStr(", "));}),_ha=new T(function(){return B(unCStr("pen1 = "));}),_hb=new T(function(){return B(unCStr("Box "));}),_hc=[0,34],_hd=new T(function(){return B(unCStr("ACK"));}),_he=new T(function(){return B(unCStr("BEL"));}),_hf=new T(function(){return B(unCStr("BS"));}),_hg=new T(function(){return B(unCStr("SP"));}),_hh=[1,_hg,_0],_hi=new T(function(){return B(unCStr("US"));}),_hj=[1,_hi,_hh],_hk=new T(function(){return B(unCStr("RS"));}),_hl=[1,_hk,_hj],_hm=new T(function(){return B(unCStr("GS"));}),_hn=[1,_hm,_hl],_ho=new T(function(){return B(unCStr("FS"));}),_hp=[1,_ho,_hn],_hq=new T(function(){return B(unCStr("ESC"));}),_hr=[1,_hq,_hp],_hs=new T(function(){return B(unCStr("SUB"));}),_ht=[1,_hs,_hr],_hu=new T(function(){return B(unCStr("EM"));}),_hv=[1,_hu,_ht],_hw=new T(function(){return B(unCStr("CAN"));}),_hx=[1,_hw,_hv],_hy=new T(function(){return B(unCStr("ETB"));}),_hz=[1,_hy,_hx],_hA=new T(function(){return B(unCStr("SYN"));}),_hB=[1,_hA,_hz],_hC=new T(function(){return B(unCStr("NAK"));}),_hD=[1,_hC,_hB],_hE=new T(function(){return B(unCStr("DC4"));}),_hF=[1,_hE,_hD],_hG=new T(function(){return B(unCStr("DC3"));}),_hH=[1,_hG,_hF],_hI=new T(function(){return B(unCStr("DC2"));}),_hJ=[1,_hI,_hH],_hK=new T(function(){return B(unCStr("DC1"));}),_hL=[1,_hK,_hJ],_hM=new T(function(){return B(unCStr("DLE"));}),_hN=[1,_hM,_hL],_hO=new T(function(){return B(unCStr("SI"));}),_hP=[1,_hO,_hN],_hQ=new T(function(){return B(unCStr("SO"));}),_hR=[1,_hQ,_hP],_hS=new T(function(){return B(unCStr("CR"));}),_hT=[1,_hS,_hR],_hU=new T(function(){return B(unCStr("FF"));}),_hV=[1,_hU,_hT],_hW=new T(function(){return B(unCStr("VT"));}),_hX=[1,_hW,_hV],_hY=new T(function(){return B(unCStr("LF"));}),_hZ=[1,_hY,_hX],_i0=new T(function(){return B(unCStr("HT"));}),_i1=[1,_i0,_hZ],_i2=[1,_hf,_i1],_i3=[1,_he,_i2],_i4=[1,_hd,_i3],_i5=new T(function(){return B(unCStr("ENQ"));}),_i6=[1,_i5,_i4],_i7=new T(function(){return B(unCStr("EOT"));}),_i8=[1,_i7,_i6],_i9=new T(function(){return B(unCStr("ETX"));}),_ia=[1,_i9,_i8],_ib=new T(function(){return B(unCStr("STX"));}),_ic=[1,_ib,_ia],_id=new T(function(){return B(unCStr("SOH"));}),_ie=[1,_id,_ic],_if=new T(function(){return B(unCStr("NUL"));}),_ig=[1,_if,_ie],_ih=[0,92],_ii=new T(function(){return B(unCStr("\\DEL"));}),_ij=new T(function(){return B(unCStr("\\a"));}),_ik=new T(function(){return B(unCStr("\\\\"));}),_il=new T(function(){return B(unCStr("\\SO"));}),_im=new T(function(){return B(unCStr("\\r"));}),_in=new T(function(){return B(unCStr("\\f"));}),_io=new T(function(){return B(unCStr("\\v"));}),_ip=new T(function(){return B(unCStr("\\n"));}),_iq=new T(function(){return B(unCStr("\\t"));}),_ir=new T(function(){return B(unCStr("\\b"));}),_is=function(_it,_iu){if(_it<=127){var _iv=E(_it);switch(_iv){case 92:return new F(function(){return _t(_ik,_iu);});break;case 127:return new F(function(){return _t(_ii,_iu);});break;default:if(_iv<32){var _iw=E(_iv);switch(_iw){case 7:return new F(function(){return _t(_ij,_iu);});break;case 8:return new F(function(){return _t(_ir,_iu);});break;case 9:return new F(function(){return _t(_iq,_iu);});break;case 10:return new F(function(){return _t(_ip,_iu);});break;case 11:return new F(function(){return _t(_io,_iu);});break;case 12:return new F(function(){return _t(_in,_iu);});break;case 13:return new F(function(){return _t(_im,_iu);});break;case 14:return new F(function(){return _t(_il,new T(function(){var _ix=E(_iu);if(!_ix[0]){var _iy=[0];}else{var _iy=E(E(_ix[1])[1])==72?B(unAppCStr("\\&",_ix)):E(_ix);}return _iy;}));});break;default:return new F(function(){return _t([1,_ih,new T(function(){var _iz=_iw;return _iz>=0?B(_gf(_ig,_iz)):E(_gc);})],_iu);});}}else{return [1,[0,_iv],_iu];}}}else{return [1,_ih,new T(function(){var _iA=jsShowI(_it),_iB=_iA;return B(_t(fromJSStr(_iB),new T(function(){var _iC=E(_iu);if(!_iC[0]){var _iD=[0];}else{var _iE=E(_iC[1])[1];if(_iE<48){var _iF=E(_iC);}else{var _iF=_iE>57?E(_iC):B(unAppCStr("\\&",_iC));}var _iG=_iF,_iH=_iG,_iD=_iH;}return _iD;})));})];}},_iI=new T(function(){return B(unCStr("\\\""));}),_iJ=function(_iK,_iL){var _iM=E(_iK);if(!_iM[0]){return E(_iL);}else{var _iN=_iM[2],_iO=E(E(_iM[1])[1]);if(_iO==34){return new F(function(){return _t(_iI,new T(function(){return B(_iJ(_iN,_iL));}));});}else{return new F(function(){return _is(_iO,new T(function(){return B(_iJ(_iN,_iL));}));});}}},_iP=function(_iQ,_iR,_iS){return _iQ<11?B(_t(_hb,[1,_hc,new T(function(){return B(_iJ(_iR,[1,_hc,_iS]));})])):[1,_5Y,new T(function(){return B(_t(_hb,[1,_hc,new T(function(){return B(_iJ(_iR,[1,_hc,[1,_5X,_iS]]));})]));})];},_iT=new T(function(){return B(unCStr("Pen "));}),_iU=function(_iV,_iW,_iX){return _iV<11?B(_t(_iT,new T(function(){return B(_5Z(11,E(_iW)[1],_iX));}))):[1,_5Y,new T(function(){return B(_t(_iT,new T(function(){return B(_5Z(11,E(_iW)[1],[1,_5X,_iX]));})));})];},_iY=new T(function(){return B(unCStr("True"));}),_iZ=new T(function(){return B(unCStr("False"));}),_j0=function(_j1,_j2,_j3,_j4,_j5,_j6,_j7){var _j8=function(_j9){return new F(function(){return _t(_h4,new T(function(){return B(_t(_ha,new T(function(){return B(_iP(0,_j2,new T(function(){return B(_t(_h9,new T(function(){return B(_t(_h8,new T(function(){return B(_iP(0,_j3,new T(function(){return B(_t(_h9,new T(function(){return B(_t(_h7,new T(function(){return B(_iU(0,_j4,new T(function(){return B(_t(_h9,new T(function(){return B(_t(_h6,new T(function(){return B(_iP(0,_j5,new T(function(){return B(_t(_h9,new T(function(){return B(_t(_h5,new T(function(){return !E(_j6)?B(_t(_iZ,[1,_e5,_j9])):B(_t(_iY,[1,_e5,_j9]));})));})));})));})));})));})));})));})));})));})));})));})));})));}));});};return _j1<11?B(_j8(_j7)):[1,_5Y,new T(function(){return B(_j8([1,_5X,_j7]));})];},_ja=function(_){var _jb=B(A(_h,["(function() { return [appl.wac.pens.pen1.box.id, appl.wac.pens.pen2.box.id, appl.wac.last_pen_moved_ndx, appl.wac.last_move_box_id, appl.wac.rule60on]; })",_])),_jc=_jb,_jd=B(_gz(_jc,0));if(!_jd[0]){return E(_gZ);}else{var _je=E(_jd[2]);if(!_je[0]){return E(_gZ);}else{var _jf=E(_je[2]);if(!_jf[0]){return E(_gZ);}else{var _jg=E(_jf[2]);if(!_jg[0]){return E(_gZ);}else{var _jh=E(_jg[2]);return _jh[0]==0?E(_gZ):E(_jh[2])[0]==0?new T(function(){return B(_j0(0,new T(function(){return B(_fV(_jd[1]));}),new T(function(){return B(_fV(_je[1]));}),new T(function(){return B(_h0(_jf[1]));}),new T(function(){return B(_fV(_jg[1]));}),new T(function(){return _jh[1]>0;}),_0));}):E(_gZ);}}}}},_ji=new T(function(){return [0,"printState"];}),_jj=function(_jk){return E(toJSStr(E(_jk)));},_jl=function(_jm){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _gs(function(_){var _jn=B(A(_jm,[_])),_jo=_jn;return new T(function(){return B(_jj(_jo));});},_);});});});},_jp=new T(function(){return B(_fO(_jl,_ji));}),_jq=new T(function(){return B(unCStr("Loaded; starting random walk."));}),_jr=function(_js,_){var _jt=B(A(_gJ,[E(toJSStr(E(_js))),_])),_ju=_jt;return _3;},_jv=new T(function(){return [0,"executeMove"];}),_jw=function(_jx){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _g3(function(_jy){return new F(function(){return _d(function(_){var _=0,_jz=B(A(_jx,[B(_fV(_jy)),_])),_jA=_jz;return E(_fZ);});});},_);});});});},_jB=new T(function(){return B(_fO(_jw,_jv));}),_jC=new T(function(){return [0,"possibleMoves"];}),_jD=new T(function(){return B(_h("lst2arr"));}),_jE=function(_jF){return new F(function(){return _d(function(_){var _=0;return new F(function(){return _gs(function(_){var _jG=B(A(_jF,[_])),_jH=_jG;return new T(function(){return B(_d(function(_){var _=0;return new F(function(){return A(_jD,[B(_gC(_jj,_jH)),_]);});}));});},_);});});});},_jI=new T(function(){return B(_fO(_jE,_jC));}),_jJ=function(_){var _=0,_jK=jsMkStdout(),_jL=_jK;return [0,_jL];},_jM=new T(function(){return B(_d(_jJ));}),_jN=function(_){var _jO=B(A(_jp,[_ja,_])),_jP=_jO,_jQ=B(A(_ga,[_fG,_])),_jR=_jQ,_jS=B(A(_jI,[_gG,_])),_jT=_jS,_jU=B(A(_jB,[_jr,_])),_jV=_jU,_jW=B(_8(_jM,_jq,_)),_jX=_jW;return new F(function(){return _gv(_);});},_jY=function(_){return new F(function(){return _jN(_);});};
var hasteMain = function() {B(A(_jY, [0]));};hasteMain()