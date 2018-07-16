open BsReactNative;

type positionedPiece = {
  piece: Piece.t,
  y: int,
  x: int,
  rotation: int
};

type coloredCoord = {
  y: int,
  x: int,
  piece: Piece.t
};

type state = {
  /* pan: Animated.ValueXY.t, */
  gameOver: bool,
  activePiece: positionedPiece,
  nextPiece: Piece.t,
  /* panListener: string, */
  filled: list(coloredCoord)
  /* panResponder: PanResponder.t */
};

type action =
  | Tick
  | MoveY
  | NewPiece
  | Rotate
  | MoveX(Direction.t);

let windowWidth = Dimensions.get(`window)##width;

let coordinatesForPiece = ({piece, rotation, y, x}) => {
  let rotatedPosition = List.nth(Piece.getPositionsForPeice(piece), rotation);
  rotatedPosition |> List.map(((x2, y2)) => (x2 + x, y2 + y));
};

let hasHitGround = positions =>
  positions |> List.fold_left((max, (_x, y)) => y > max ? y : max, -999) > 19;

let convertToFilled = piece =>
  coordinatesForPiece(piece)
  |> List.map(((x, y)) => {x, y, piece: piece.piece});

let justFilledCoords = List.map(coord => (coord.x, coord.y));

let isDead = (piece, filled) =>
  List.append(coordinatesForPiece(piece), justFilledCoords(filled))
  |> List.exists(((_, y)) => y < (-2));

let collidesWithExisting = (filled, piece) =>
  justFilledCoords(filled)
  |> List.exists(coord =>
       coordinatesForPiece(piece)
       |> List.exists(fromPiece => coord == fromPiece)
     );

let positionedPiece = piece => {piece, y: 4, x: 4, rotation: 0};

/* let positionedPiece = piece => {piece, y: (-2), x: 4, rotation: 0}; */
let canMoveY = ({activePiece, filled}) => {
  let potentialPiece = {...activePiece, y: activePiece.y + 1};
  ! (
    hasHitGround(coordinatesForPiece(potentialPiece))
    || collidesWithExisting(filled, potentialPiece)
  );
};

let setPosition = pan => Js.log(pan);

let component = ReasonReact.reducerComponent("Field");

let make = _children => {
  ...component,
  initialState: () => {
    /* let pan = Animated.ValueXY.create(~x=0., ~y=0.); */
    /* pan, */
    gameOver: false,
    activePiece: positionedPiece(Piece.createPiece()),
    nextPiece: Piece.createPiece(),
    filled: []
    /* panListener:
       Animated.ValueXY.addListener(pan, raw => childCoordinates := raw), */
    /* panResponder:
       PanResponder.(
         create(
           ~onStartShouldSetPanResponder=callback((_e, _g) => true),
           /* ~onPanResponderMove=`update([`XY(pan)]), */
           ~onPanResponderMove=
             `callback(callback((_, _) => setPosition(pan))),
           ()
         )
       ) */
  },
  /* didMount: self => {
       let intervalId = Js.Global.setInterval(() => self.send(Tick), 100);
       self.onUnmount(() => Js.Global.clearInterval(intervalId));
     }, */
  reducer: (action, state) =>
    switch action {
    | Tick =>
      state.gameOver ?
        ReasonReact.NoUpdate :
        ReasonReact.SideEffects(
          (self => self.send(canMoveY(self.state) ? MoveY : NewPiece))
        )
    | NewPiece =>
      ReasonReact.Update(
        isDead(state.activePiece, state.filled) ?
          {...state, gameOver: true} :
          {
            ...state,
            nextPiece: Piece.createPiece(),
            activePiece: positionedPiece(state.nextPiece),
            filled:
              List.append(convertToFilled(state.activePiece), state.filled)
          }
      )
    | Rotate =>
      ReasonReact.Update({
        ...state,
        activePiece: {
          ...state.activePiece,
          rotation:
            state.activePiece.rotation === 3 ?
              0 : state.activePiece.rotation + 1
        }
      })
    | MoveY =>
      ReasonReact.Update({
        ...state,
        activePiece: {
          ...state.activePiece,
          y: state.activePiece.y + 1
        }
      })
    | MoveX(dir) =>
      ReasonReact.Update({
        ...state,
        activePiece: {
          ...state.activePiece,
          x:
            state.activePiece.x
            + (
              switch dir {
              | Direction.Right => 1
              | Direction.Left => (-1)
              }
            )
        }
      })
    },
  render: ({send, state: {activePiece, gameOver, filled}}) => {
    let boardAspectRatio = 20. /. 10.;
    <SafeAreaView
      style=Style.(
              style([
                position(Relative),
                display(Flex),
                backgroundColor(String("rgb(40, 40, 40)")),
                flex(1.),
                aspectRatio(1. /. boardAspectRatio)
              ])
            )>
      (
        gameOver ?
          <View
            style=Style.(style([position(Relative), display(Flex), flex(1.)]))>
            <Text> (ReasonReact.string("Game over")) </Text>
          </View> :
          <FieldTouchHandler
            moveY=(() => send(MoveY))
            rotate=(() => send(Rotate))
            moveX=(dir => send(MoveX(dir)))>
            <Piece
              key=(
                string_of_int(activePiece.x) ++ string_of_int(activePiece.y)
              )
              piece=activePiece.piece
              rotation=activePiece.rotation
              pos=(activePiece.x, activePiece.y)
            />
            (
              filled
              |> List.map(coord =>
                   <Pixel
                     key=(string_of_int(coord.x) ++ string_of_int(coord.y))
                     pos=(coord.x, coord.y)
                     color="rgb(90,90,90)"
                   />
                 )
              |> Array.of_list
              |> ReasonReact.array
            )
          </FieldTouchHandler>
      )
    </SafeAreaView>;
  }
};