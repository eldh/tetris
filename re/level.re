open BsReactNative;

let component = ReasonReact.statelessComponent("Level");

let make = (~level, _children) => {
  ...component,
  render: _self => {
    <WhiteText
      styles=Style.(style([position(Absolute), top(Pt(20.)), right(Pt(20.))]))
    >(ReasonReact.string(string_of_int(level)))</WhiteText>;
  }
};