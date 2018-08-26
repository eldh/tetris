open BsReactNative;

let component = ReasonReact.statelessComponent("WhiteText");

let make = (~styles, children) => {
  ...component,
  render: _self =>
    <Text style=Style.(combine(style([color(String("#ffffff"))]), styles))>
      ...children
    </Text>,
};
