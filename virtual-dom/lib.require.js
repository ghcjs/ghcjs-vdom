/*
  to generate lib.js, install virtual-dom and process file:

     $ npm install
     $ grunt
   the ./diff module is vtree/diff with a few changes to
   allow diff to run in an asynchronous thread in the presence of
   memoized nodes.
 */

/*
   Note on memory management:

   To ensure accurate heap tracing for finalization and profiling purposes,
   GHCJS needs to know reachable all Haskell values. ghcjs-vdom stores some
   Haskell values inside JavaScript references and uses extensible retention
   to collect these values. It's crucial that all data structures that may
   contain Haskell values are stored directly in a JSVal and not inside other
   JS data structures.

   The recognized types are:

     - HSPatch:
         The patch object contains the new (target) virtual-dom tree and
         the original tree is reachable trough the parent. Since all handlers,
         components and thunks are reachable through these trees, the patch itself
         does not need to be traversed.

     - HSMount:
         All mounted HSMount points are scanned as roots. The current virtual tree
         (mount.vtree) is traversed for this.

     - HSComponent:
         Traversed when reachable through Haskell heap or virtual-dom tree. Contains
         current tree (component.vtree) and rendering action (component.hsRender)

     - HSThunk:
         Traversed when reachable through Haskell heap or virtual-dom tree. Contains
         Haskell suspension (thunk.hst) or rendered tree (thunk.vnode)

     - virtual node ( isVirtualNode(x) )
         Some node in a virtual-dom tree. Haskell event handlers are stored in 'ev-*'
         properties, which virtual-dom adds to the node's hooks (vnode.hooks). If
         a virtual node contains thunks, widgets or any of its descendants have hooks,
         the children of the node have to be traversed.

   forceThunks and forcePatch fill an array of thunks, which is not directly
   scannable; however, these functions are only used as part of a `diff` operation.
   The initial diff creates an HSPatch object, through which the original and target
   virtual-dom tree are completely reachable.
 */

var isVirtualNode = require('virtual-dom/vnode/is-vnode');
var isThunk       = require('virtual-dom/vnode/is-thunk');
var isWidget      = require("virtual-dom/vnode/is-widget");
var h             = require('virtual-dom/h');
var svg           = require('virtual-dom/virtual-hyperscript/svg');
var isArray       = require('x-is-array');
var VPatch        = require("virtual-dom/vnode/vpatch");
var VText         = require('virtual-dom/vnode/vtext');
var vdomPatch     = require('virtual-dom/vdom/patch');
var DomDelegator  = require('dom-delegator');

var diff = require('./diff');

var VRenderableN = 0;

/** @constructor */
function HSPatch(patch, old, vnode, parent) {
  this.patch   = patch;
  this.old     = old;
  this.vnode   = vnode;
  this.parent  = parent;
}

/** @constructor */
function HSThunk(t, ids, key) {
  this.hst        = t;   // haskell thunk
  this.ids        = ids; // array of haskell unique ids
  this.key        = key;
  this.vnode      = null;
  this._ghcjsMark = 0;
}

HSThunk.prototype.type = 'Thunk';

/*
  render returns the deferred rendering object
  null if the thunk has already been rendered, in which case the value is in this.vnode
 */
HSThunk.prototype.render = function(previous) {
  if(previous && !this.vnode && eqIds(this.ids, previous.ids)) {
    if(previous.hst) {
      this.hst = previous.hst;
    } else {
      this.hst   = null;
      this.vnode = previous.vnode;
    }
  }
  return this.vnode ? null : this;
}

/** @constructor */
function HSComponent(r, mnt, unmnt, key) {
  this._key      = ++VRenderableN;
  this.hsRender  = r;   // IO action that produces a vdom tree
  this.hsMount   = mnt;
  this.hsUnmount = unmnt;
  this.key       = key || this._key;
  this.vnode     = this.initialVnode = new VText("");
  this.pending   = [];
  this.mounts    = {};
  this.version   = 0;
  this.latest    = 0;
}

HSComponent.prototype.type = 'Widget';

HSComponent.prototype.init = function() {
  var n = document.createTextNode('');
  if(this.vnode !== this.initialVnode) {
    var thunks = [];
    var p = diff(this.initialVnode, this.vnode, thunks);
    if(thunks.length !== 0) {
      throw new Error("HSComponent vnode contains unevaluated thunks");
    }
    n = vdomPatch(n, p);
  }
  var m = new HSComponentMount(n);
  n._key = m._key;
  n._widget = this;
  this.mounts[m._key] = m;
  if(this.hsMount) {
    h$vdomMountComponentCallback(this.hsMount, m._key, this);
  }
  return n;
}

HSComponent.prototype.destroy = function(domNode) {
  delete this.mounts[domNode._key];
  if(this.hsUnmount) {
    h$vdomUnmountComponentCallback(this.hsUnmount, domNode._key, domNode);
  }
}

HSComponent.prototype.diff = function(v, thunks) {
  var vn = this.vnode;
  if(this.pending.length > 0) {
    vn = this.pending[this.pending.length-1].vnode;
  }
  return new HSPatch( diff(vn, v, thunks)
		    , vn
		    , v
		    , this);
}

HSComponent.prototype.addPatch = function(p) {
  var cur = this.pending.length > 0 ? this.pending[this.pending.length-1]
                                    : this.vnode;
  if(p.old === cur) this.pending.push(p);
}

HSComponent.prototype.patch = function(p) {
  if(this.pending.length > 0) {
    var pnd = this.pending;
    this.pending = [];
    for(var i = 0; i < pnd.length; i++) this.patch(pnd[i]);
  }
  if(!p) return;
  if(p.parent !== this || p.old !== this.vnode) {
    return false;
  }
  for(var k in this.mounts) {
    var m = this.mounts[k];
    m.node = vdomPatch(m.node, p.patch);
  }
  this.vnode = p.vnode;
  return true;
}

// only use this for manually updated components (i.e. no diff/patch)
HSComponent.prototype.updateMount = function(mnt, node) {
  var m = this.mounts[mnt];
  node._key = mnt;
  node._widget = this;
  m.node.parentNode.replaceChild(node, m.node);
  m.node = node;
}

HSComponent.prototype.update = function(leftVNode, node) {
  if(node._widget) {
    if(node._widget == this) return node;
    node._widget.destroy(node);
  }
  return this.init();
}

var HSComponentMountN = 0;
function HSComponentMount(domNode) {
  this._key = ++HSComponentMountN;
  this.node = domNode;
}

/** @constructor */
function HSMount(domNode) {
  this._key       = ++VRenderableN;
  // this.version    = 0;
  this.vnode      = new VText(""); // currently rendered vdom tree
  this.pending    = [];            // pending patches, not yet applied
  this.node       = document.createTextNode("");
  this.parentNode = domNode;
}

HSMount.prototype.diff = function(v, thunks) {
  var vn = this.vnode;
  if(this.pending.length > 0) {
    vn = this.pending[this.pending.length-1].vnode;
  }
  return new HSPatch( diff(vn, v, thunks)
		    , vn
		    , v
		    , this);
}

HSMount.prototype.addPatch = function(p) {
  var cur = this.pending.length > 0 ? this.pending[this.pending.length-1]
                                    : this.vnode;
  if(p.old === cur) this.pending.push(p);
}

// HSMount.patch(null) to flush pending list
HSMount.prototype.patch = function(p) {
  if(this.pending.length > 0) {
    var pnd = this.pending;
    this.pending = [];
    for(var i = 0; i < pnd.length; i++) this.patch(pnd[i]);
  }
  if(!p) return;
  if(p.parent !== this || p.old !== this.vnode) {
    return false;
  }
  this.node  = vdomPatch(this.node, p.patch);
  this.vnode = p.vnode;
  return true;
}

/* mount a vdom tree, making it visible to extensible retention */
function mount(domNode) {
  while(domNode.firstChild) domNode.removeChild(domNode.firstChild);
  var m = new HSMount(domNode);
  domNode.appendChild(m.node);
  vdomMounts.add(m);
  return m;
}

/* unmount a tree, removing all child nodes. */
function unmount(vmount) {
  var n = vmount.parentNode;
  while(n.firstChild) n.removeChild(n.firstChild);
  vdomMounts.remove(vmount);
}

/*
   Compare lists of object identifiers associated with a thunk node. If the lists are equal,
   the subtree does not have to be recomputed.
*/
function eqIds(ids1, ids2) {
  if(!ids1 || !ids2 || ids1.length != ids2.length) return false;
  for(var i=ids1.length-1;i>=0;i--) {
    var id1 = ids1[i], id2 = ids2[i];
    if(typeof id1 === 'number') {
      if(typeof id2 !== 'number') return false;
      if(id1 !== id2 && !((id1!=id1) && (id2!=id2))) return false;
    } else {
      if(id1 !== id2) return false;
    }
  }
  return true;
}

function forcePatch(p) {
  var thunks = [], i, j, pi;
  for(i in p) {
    var pi = p[i];
    if(isArray(pi)) {
      for(j=pi.length-1;j>=0;j--) {
	forceTree(pi[j].patch, thunks);
      }
    }
    else if(pi.patch) forceTree(pi.patch, thunks);
    else forceTree(pi, thunks);
  }
  return thunks;
}

function forceTree(n, t) {
  if(isThunk(n)) {
    if(n.vnode) forceTree(n.vnode, t);
    else t.push(n);
  } else if(isVirtualNode(n) && n.hasThunks) {
    for(var i=n.children.length-1;i>=0;i--) {
      forceTree(n.children[i], t);
    }
  }
}

/*
   scan all mounted virtual-dom trees
 */
function scanMounts(currentMark) {
  var i = vdomMounts.iter(), m, res = [];
  while((m = i.next()) !== null) {
    scanTreeRec(m.vnode, res, currentMark);
    if(m.pending.length > 0) {
      scanTreeRec(m.pending[m.pending.length-1].vnode, res, currentMark);
    }
  }
  return res;
}

/*
   scan a tree (extensible retention callback).

   returns:
     - an array of haskell items if any
     - true if no haskell items have been found
     - false if the object is not a ghcjs-vdom tree
         (fallthrough to other extensible retention scanners)
 */
var scanTreeRes = [];
function scanTree(o, currentMark) {
  if(isVirtualNode(o) || isThunk(o) || isWidget(o) ||
     o instanceof HSPatch || o instanceof HSComponent || o instanceof HSMount) {
    var r = scanTreeRes;
    scanTreeRec(o, r, currentMark);
    if(r.length > 0) {
      scanTreeRes = [];
      return r;
    } else {
      return true;
    }
  } else { // not a ghcjs-vdom object, fall through
    return false;
  }
}

function scanTreeRec(o, r, currentMark) {
  if(o instanceof HSPatch) {
    scanTreeRec(o.vnode, r, currentMark);
    scanTreeRec(o.parent);
  } else if(o instanceof HSThunk) {
    if(o._ghcjsMark !== currentMark) {
      o._ghcjsMark = currentMark;
      if(o.hst) r.push(o.hst);
      if(o.vnode) scanTreeRec(o.vnode, r, currentMark);
    }
  } else if(o instanceof HSComponent) {
    if(o._ghcjsMark !== currentMark) {
      o._ghcjsMark = currentMark;
      if(o.hsRender) r.push(o.hsRender);
      if(o.hsMount) r.push(o.hsMount);
      if(o.hsUnmount) r.push(o.hsUnmount);
      if(o.vnode) scanTreeRec(o.vnode, r, currentMark);
      if(o.pending.length > 0) {
	scanTreeRec(o.pending[o.pending.length-1].vnode, r, currentMark);
      }
    }
  } else if(isVirtualNode(o)) {
    if(o._ghcjsMark !== currentMark) {
      o._ghcjsMark = currentMark;
      // collect event handlers
      var hooks = o.hooks;
      for(var p in hooks) {
	if(p.indexOf('ev-') === 0) {
	  var handler = hooks[p];
	  if(handler.value && handler.value.hsAction) {
	    r.push(handler.value.hsAction);
	  }
	}
      }
      // recurse if any of the children may have thunks, components or handlers
      if(o.hasWidgets || o.hasThunks || o.descendantHooks) {
	for(var i=o.children.length-1;i>=0;i--) {
          scanTreeRec(o.children[i], r, currentMark);
	}
      }
    }
  }
}

function setThunkPatch(n, p) {
  if(hasPatches(p)) n.p[n.i] = new VPatch(VPatch.THUNK, null, p);
}

function hasPatches(patch) {
  for (var index in patch) {
    if (index !== "a") {
      return true;
    }
  }
  return false;
}

function initDelegator(evTypes) {
  var d = DomDelegator();
  var l = evTypes.length;
  for(var i = 0; i < l; i++) {
    d.listenTo(evTypes[i]);
  }
}

function v(tag, props, children) {
  return h(tag, props, children);
}

function s(tag, props, children) {
  return svg(tag, props, children);
}

function t(text) {
  return new VText(text);
}

function th(t, ids, key) {
  return new HSThunk(t, ids, key);
}

function c(r, m, u, key) {
  return new HSComponent(r, m, u, key);
}


function makeHandler(action, async) {
  var f = function(ev) {
    return h$vdomEventCallback(async, action, ev);
  }
  f.hsAction = action;
  return f;
}

var vdomMounts = new h$Set();

module.exports = { setThunkPatch: setThunkPatch
                 , forceTree:     forceTree
                 , forcePatch:    forcePatch
                 , diff:          diff
                 , mount:         mount
                 , unmount:       unmount
                 , initDelegator: initDelegator
                 , v:             v
                 , th:            th
                 , t:             t
                 , c:             c
                 , s:             s
                 , makeHandler:   makeHandler
                 };

// the global variable we're using in the bindings
h$vdom = module.exports;

h$registerExtensibleRetention(scanTree);
h$registerExtensibleRetentionRoot(scanMounts);


