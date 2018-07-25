open BsReactNative;

let component = ReasonReact.statelessComponent("Score");

let make = (~score, _children) => {
  ...component,
  render: _self => {
    <WhiteText
      styles=Style.(style([position(Absolute), top(Pt(20.)), left(Pt(20.))]))
    >(ReasonReact.string( string_of_int(score)))</WhiteText>;
  }
};