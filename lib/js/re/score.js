// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var WhiteText = require("./whiteText.js");
var ReasonReact = require("reason-react/lib/js/src/ReasonReact.js");
var Style$BsReactNative = require("bs-react-native/lib/js/src/style.js");

var component = ReasonReact.statelessComponent("Score");

function make(score, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function () {
              return ReasonReact.element(/* None */0, /* None */0, WhiteText.make(Style$BsReactNative.style(/* :: */[
                                  Style$BsReactNative.position(/* Absolute */0),
                                  /* :: */[
                                    Style$BsReactNative.top(/* Pt */Block.__(0, [20])),
                                    /* :: */[
                                      Style$BsReactNative.left(/* Pt */Block.__(0, [20])),
                                      /* [] */0
                                    ]
                                  ]
                                ]), /* array */[String(score)]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

exports.component = component;
exports.make = make;
/* component Not a pure module */