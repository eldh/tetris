open BsReactNative;

exception ImpossibleLines;

type positionedPiece = {
  piece: Piece.t,
  y: int,
  x: int,
  rotation: int
};

type filledPixel = {
  x: int,
  piece: Piece.t
};

type pixelWithCoord = {
  x: int,
  y: int,
  piece: Piece.t
};

type state = {
  gameOver: bool,
  activePiece: positionedPiece,
  nextPiece: Piece.t,
  filled: list(list(filledPixel)),
  score: int,
  level: int,
};

type action =
  | Tick
  | MoveY
  | NewPiece
  | Rotate
  | MoveX(Direction.t);

let boardHeight = 20;

let boardWidth = 10;

let windowWidth = Dimensions.get(`window)##width;

let getMoveScore = (level, lines) => {
  let multiplier =
    switch lines {
    | 0 => 0
    | 1 => 40
    | 2 => 100
    | 3 => 300
    | 4 => 1200
    | _ => raise(ImpossibleLines)
    };
  multiplier * (level + 1);
};

let coordinatesForPiece = ({piece, rotation, y, x}) => {
  let rotatedPosition = List.nth(Piece.getPositionsForPeice(piece), rotation);
  rotatedPosition |> List.map(((x2, y2)) => (x2 + x, y2 + y));
};

let hasHitGround = positions =>
  positions |> List.fold_left((min, (_x, y)) => y < min ? y : min, 100) < 0;

let hasHitSide = positions => {
  let hasHitLeftSide =
    positions |> List.fold_left((min, (x, _y)) => x < min ? x : min, 999) < 0;
  let hasHitRightSide =
    positions
    |> List.fold_left((max, (x, _y)) => x > max ? x : max, -999) >= 10;
  hasHitLeftSide || hasHitRightSide;
};

let pieceToPixels = piece =>
  coordinatesForPiece(piece)
  |> List.map(((x, y)) => {y, x, piece: piece.piece});

let filledCoords = (filled: list(list(filledPixel))) =>
  List.mapi(
    (i, row: list(filledPixel)) => List.map(({x}: filledPixel) => (x, i), row),
    filled
  )
  |> List.flatten;

let isDead = (piece, filled) =>
  filled
  |> filledCoords
  |> List.append(coordinatesForPiece(piece))
  |> List.exists(((_, y)) => y > 20);

let collidesWithExisting = (filled: list((int, int)), coords: list((int, int))) =>
  filled
  |> List.exists(coord =>
       coords |> List.exists(fromPiece => coord == fromPiece)
     );

let positionedPiece = piece => {piece, y: 22, x: 4, rotation: 0};

/* let positionedPiece = piece => {piece, y: (2), x: 4, rotation: 0}; */
let validMove = (filled, potentialPiece) => {
  let coords = coordinatesForPiece(potentialPiece);
  ! (
    hasHitGround(coords)
    || hasHitSide(coords)
    || collidesWithExisting(filledCoords(filled), coords)
  );
};

let canMoveY = ({activePiece, filled}) =>
  validMove(filled, {...activePiece, y: activePiece.y - 1});

let canMoveX = (direction, {activePiece, filled}) =>
  validMove(
    filled,
    {
      ...activePiece,
      x: activePiece.x + (direction == Direction.Left ? (-1) : 1)
    }
  );

let canRotate = ({activePiece, filled}) =>
  validMove(
    filled,
    {
      ...activePiece,
      rotation: activePiece.rotation === 3 ? 0 : activePiece.rotation + 1
    }
  );

let newForRow = (index, coords) => List.filter(({y}) => y === index, coords);

let pixelToFilled = ({x, piece}) => {x, piece};

let addActiveToFilled = ({activePiece, filled}) => {
  let newCoords = pieceToPixels(activePiece);
  filled
  |> List.mapi((i, row) =>
       List.append(newForRow(i, newCoords) |> List.map(pixelToFilled), row)
     )
  |> List.filter(row => List.length(row) !== boardWidth);
};

let component = ReasonReact.reducerComponent("Field");

let make = _children => {
  ...component,
  initialState: () => {
    score: 0,
    level: 0,
    gameOver: false,
    activePiece: positionedPiece(Piece.createPiece()),
    nextPiece: Piece.createPiece(),
    filled: Array.to_list(Array.make(20, []))
  },
  didMount: self => {
    let intervalId = Js.Global.setInterval(() => self.send(Tick), 500);
    self.onUnmount(() => Js.Global.clearInterval(intervalId));
  },
  reducer: (action, state) =>
    switch action {
    | Tick =>
      state.gameOver ?
        ReasonReact.NoUpdate :
        ReasonReact.SideEffects(
          (self => self.send(canMoveY(self.state) ? MoveY : NewPiece))
        )
    | NewPiece =>
      let filled = addActiveToFilled(state);
      ReasonReact.Update(
        isDead(state.activePiece, state.filled) ?
          {...state, gameOver: true} :
          {
            ...state,
            nextPiece: Piece.createPiece(),
            activePiece: positionedPiece(state.nextPiece),
            filled,
            score: state.score + (1 |> getMoveScore(state.level))
          }
      )
    | Rotate =>
      canRotate(state) ?
        ReasonReact.Update({
          ...state,
          activePiece: {
            ...state.activePiece,
            rotation:
              state.activePiece.rotation === 3 ?
                0 : state.activePiece.rotation + 1
          }
        }) :
        ReasonReact.NoUpdate
    | MoveY =>
      canMoveY(state) ?
        ReasonReact.Update({
          ...state,
          activePiece: {
            ...state.activePiece,
            y: state.activePiece.y - 1
          }
        }) :
        ReasonReact.NoUpdate
    | MoveX(dir) =>
      canMoveX(dir, state) ?
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
        }) :
        ReasonReact.NoUpdate
    },
  render: ({send, state: {score, level, activePiece, gameOver, filled}}) => {
    let boardAspectRatio = float_of_int(boardHeight / boardWidth);
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
                "piece"
                ++ string_of_int(activePiece.x)
                ++ string_of_int(activePiece.y)
              )
              piece=activePiece.piece
              rotation=activePiece.rotation
              pos=(activePiece.x, activePiece.y)
            />
            (
              filled
              |> List.mapi((i, row) =>
                   row
                   |> List.map((pixel: filledPixel) =>
                        <Pixel
                          key=(
                            "pixel"
                            ++ string_of_int(pixel.x)
                            ++ string_of_int(i)
                          )
                          pos=(pixel.x, i)
                          color="rgb(90,90,90)"
                        />
                      )
                 )
              |> List.flatten
              |> Array.of_list
              |> ReasonReact.array
            )
          </FieldTouchHandler>
      )
      <Score score={score} />
      <Level level={level} />
    </SafeAreaView>;
  }
};