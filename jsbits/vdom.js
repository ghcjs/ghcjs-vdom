#include <ghcjs/rts.h>

/* 
 * global name for the things we need from the virtual-dom library
 */
var h$vdom;

function h$vdomEventCallback(async, action, ev) {
  var a = MK_AP1(action, MK_JSREF(ev));
  if(async) {
    h$run(a);
  } else {
    h$runSync(a, true);
  }
}

function h$vdomMountComponentCallback(action, mnt, comp) {
  h$run(MK_AP2(action, MK_JSREF(mnt), MK_JSREF(comp)));
}

function h$vdomUnmountComponentCallback(action, mnt, node) {
  h$run(MK_AP2(action, MK_JSREF(mnt), MK_JSREF(node)));
}
