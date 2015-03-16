var isVNode = require("virtual-dom/vnode/is-vnode")
var isVText = require("virtual-dom/vnode/is-vtext")
var isWidget = require("virtual-dom/vnode/is-widget")
var isThunk = require("virtual-dom/vnode/is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    return { a: isThunk(a) ? renderThunk(a, null) : null
           , b: isThunk(b) ? renderThunk(b, a) : null
           }
}

function renderThunk(thunk, previous) {
    if(thunk.vnode) return null;
    thunk.render(previous);
    return thunk.vnode ? null : thunk;
}
