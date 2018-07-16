// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function cache(f) {
  var value = [/* () */0];
  var arg = [/* () */0];
  return (function (a) {
      if (a === arg) {
        return value[0];
      } else {
        value[0] = Curry._1(f, a);
        arg[0] = a[0];
        return value[0];
      }
    });
}

exports.cache = cache;
/* No side effect */