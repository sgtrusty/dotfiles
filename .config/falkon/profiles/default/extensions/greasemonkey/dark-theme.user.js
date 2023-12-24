// ==UserScript==
// @name        Dimmer (localStorage)
// @description Toggle dark mode using hotkey: Ctrl + Shift + D
// @author      Schimon Jehudah, Adv.
// @namespace   org.openuserjs.sjehuda.Dimmer
// @homepageURL https://openuserjs.org/scripts/sjehuda/Dimmer
// @supportURL  https://openuserjs.org/scripts/sjehuda/Dimmer/issues
// @updateURL   https://openuserjs.org/meta/sjehuda/Dimmer.meta.js
// @copyright   2023, Schimon Jehudah (http://schimon.i2p)
// @license     MIT; https://opensource.org/licenses/MIT
// @icon        data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMDAgMTAwIj48dGV4dCB5PSIuOWVtIiBmb250LXNpemU9IjkwIj7wn5SFPC90ZXh0Pjwvc3ZnPgo=
// @include     *
// @exclude     devtools://*
// @version     23.04.09
// @require     https://cdn.jsdelivr.net/npm/darkreader/darkreader.min.js
// @noframes
// ==/UserScript==

const namespace = 'org-openuserjs-sjehuda-dimmer';

if (document.doctype == null) { return; }

if (getPreference('dimmer') == 1 || getPreference('dimmer') === null) {
    setPreference('dimmer', 1);
    enable();
}

// set hotkey
document.onkeyup = function(e) {
  if (e.ctrlKey && e.shiftKey && e.which == 68) { // Ctrl + Shift + D
    toggle();
  }
};

// toggle mode
function toggle() {
  if (getPreference('dimmer') == 1) { // TODO Add random to avoid abuse by 3rd party
    setPreference('dimmer', 0);
    disable();
  } else {
    setPreference('dimmer', 1);
    enable();
  }
}

function disable() {
  DarkReader.disable({
    brightness: 100,
    contrast: 90,
    sepia: 10
  });
}

function enable() {
  DarkReader.setFetchMethod(window.fetch); // https://eligrey.com/
  DarkReader.enable({
    brightness: 100,
    contrast: 90,
    sepia: 10
  });
}

function getPreference(key) {
  return window.localStorage.getItem(key);
}

function setPreference(key, value) {
  return window.localStorage.setItem(key, value);
}
