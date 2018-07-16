open BsReactNative;

exception Stupid(string);

type coord = (float, float);

type dir =
  | X
  | Y;

type state = {
  start: option(coord),
  lastMoveCoord: option(coord),
  pos: option(coord)
};

type action =
  | Move(coord)
  | Start(coord)
  | End(coord);

let windowWidth = float_of_int(Dimensions.get(`window)##width);

let pieceWidth = windowWidth /. 10.;

let coord = e => (RNEvent.TouchEvent.pageX(e), RNEvent.TouchEvent.pageY(e));

let component = ReasonReact.reducerComponent("FieldTouchHandler");

let make = (~moveX, ~rotate, ~moveY, children) => {
  ...component,
  initialState: () => {start: None, pos: None, lastMoveCoord: None},
  reducer: (action, state) =>
    switch action {
    | Move(thisCoord) =>
      let (xNow, yNow) = thisCoord;
      let start =
        switch state.start {
        | None => raise(Stupid("Start should really be set here"))
        | Some(t) => t
        };
      let (xStart, yStart) = start;
      let dir =
        abs_float(xStart -. xNow) -. abs_float(yStart -. yNow) > 0. ? X : Y;
      let (xLastMove, yLastMove) =
        switch state.lastMoveCoord {
        | None => start
        | Some(v) => v
        };
      let updateCoord = () =>
        ReasonReact.Update({...state, lastMoveCoord: Some(thisCoord)});
      switch dir {
      | X =>
        xLastMove -. xNow > pieceWidth ?
          {
            moveX(Direction.Left);
            updateCoord();
          } :
          xLastMove -. xNow < -. pieceWidth ?
            {
              moveX(Direction.Right);
              updateCoord();
            } :
            ReasonReact.NoUpdate
      | Y =>
        yLastMove -. yNow > pieceWidth && state.lastMoveCoord == None ?
          {
            rotate();
            updateCoord();
          } :
          yLastMove -. yNow < -. pieceWidth ?
            {
              moveY();
              updateCoord();
            } :
            ReasonReact.NoUpdate
      };
    | Start(c) => ReasonReact.Update({...state, start: Some(c)})
    | End(_) =>
      ReasonReact.Update({...state, lastMoveCoord: None, start: None})
    },
  render: self =>
    <View
      responderHandlers={
        onMoveShouldSetResponder:
          Some(
            (_) => {
              Js.log("onMoveShouldSetResponder");
              true;
            }
          ),
        onStartShouldSetResponder: Some((_) => true),
        onMoveShouldSetResponderCapture: Some((_) => true),
        onResponderGrant: Some(e => self.send(Start(coord(e)))),
        onResponderMove: Some(e => self.send(Move(coord(e)))),
        onResponderReject: Some((_) => Js.log("reject")),
        onResponderRelease: Some(e => self.send(End(coord(e)))),
        onResponderTerminate: Some((_) => Js.log("terminate")),
        onResponderTerminationRequest:
          Some((_) => Js.log("terminationrequest")),
        onStartShouldSetResponderCapture: Some((_) => true)
      }
      style=Style.(style([position(Relative), display(Flex), flex(1.)]))>
      ...children
    </View>
};