open BsReactNative;

let component = ReasonReact.statelessComponent("Pixel");

let make = (~pos, ~color as bgColor, _children) => {
  ...component,
  render: _self => {
    let (x, y) = pos;
    <View
      style=Style.(
              style([
                position(Absolute),
                bottom(Pct(float_of_int(5 * y))),
                left(Pct(float_of_int(10 * x))),
                width(Pct(10.)),
                height(Pct(5.)),
                backgroundColor(String(bgColor)),
                borderColor(String("rgb(40, 40, 40)")),
                borderWidth(1.),
                borderStyle(Solid)
              ])
            )
    />;
  }
};