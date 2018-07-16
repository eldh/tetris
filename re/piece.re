open BsReactNative;

type t =
  | RightS
  | LeftS
  | RightL
  | LeftL
  | T
  | Long
  | Square;

let rec listPiecesRec = arr =>
  switch arr {
  | [] => listPiecesRec([RightS])
  | [RightS, ..._] => listPiecesRec([LeftS, ...arr])
  | [LeftS, ..._] => listPiecesRec([RightL, ...arr])
  | [RightL, ..._] => listPiecesRec([LeftL, ...arr])
  | [LeftL, ..._] => listPiecesRec([T, ...arr])
  | [T, ..._] => listPiecesRec([Long, ...arr])
  | [Long, ..._] => listPiecesRec([Square, ...arr])
  | [Square, ..._] => arr
  };

let listPieces = listPiecesRec([]);

let getStringForPeice = piece =>
  switch piece {
  | RightS => "RightS"
  | LeftS => "LeftS"
  | LeftL => "LeftL"
  | Square => "Square"
  | Long => "Long"
  | RightL => "RightL"
  | T => "T"
  };

Random.init(int_of_float(Js.Date.now()));

let numberOfPieces = List.length(listPieces);

let createPiece = () => List.nth(listPieces, Random.int(numberOfPieces));

let getPositionsForPeice = piece =>
  switch piece {
  | RightS => [
      /*
            0 X
          X X

          X
          0 X
            X

            X X
          X 0

          X
          X 0
            X

       */
      [(0, 0), ((-1), (-1)), (0, (-1)), (1, 0)],
      [(0, 0), (0, (-1)), (1, 0), (1, 1)],
      [(0, 0), ((-1), 0), ((-1), (-1)), (1, 0)],
      [(0, 0), ((-1), 1), ((-1), 0), (0, (-1))]
    ]
  | LeftS => [
      /*
          X 0
            X X

            X
          X 0
          X

          X X
            0 X

            X
          0 X
          X

       */
      [((-1), 0), (0, 0), (0, (-1)), (1, (-1))],
      [(0, 1), (0, 0), ((-1), 0), ((-1), (-1))],
      [((-1), 1), (0, 1), (0, 0), (1, 0)],
      [(1, 1), (1, 0), (0, 0), (0, (-1))]
    ]
  | LeftL => [
      /*
       *     X           X X  X
       *     0    X 0 X  0    X 0 X
       *   X X        X  X
       */
      [(0, 1), (0, 0), (0, (-1)), ((-1), (-1))],
      [((-1), 0), (0, 0), (1, 0), (1, (-1))],
      [(0, 1), (1, 1), (0, 0), (0, (-1))],
      [((-1), 1), ((-1), 0), (0, 0), (1, 0)]
    ]
  | Square => [
      /*
       *   0 X
       *   X X

       *   X 0
       *   X X

       *   X X
       *   X 0

       *   X X
       *   0 X
       */
      [(0, 0), (0, 1), (1, 0), (1, 1)],
      [(0, 0), (0, 1), (1, 0), (1, 1)],
      [(0, 0), (0, 1), (1, 0), (1, 1)],
      [(0, 0), (0, 1), (1, 0), (1, 1)]
    ]
  | Long => [
      /*
       *   X           X
       *   0  X 0 X X  X  X X 0 X
       *   X           0
       *   X           X
       */
      [(0, (-1)), (0, 0), (0, 1), (0, 2)],
      [((-1), 0), (0, 0), (1, 0), (2, 0)],
      [(0, (-2)), (0, (-1)), (0, 0), (0, 1)],
      [((-2), 0), ((-1), 0), (0, 0), (1, 0)]
    ]
  | RightL => [
      /*
       *   X        X  X X
       *   0    X 0 X    0  X 0 X
       *   X X           X  X
       */
      [(0, 1), (0, 0), (0, (-1)), (1, (-1))],
      [((-1), 0), (0, 0), (1, 0), (1, 1)],
      [((-1), (-1)), (0, 0), (0, 1), (0, (-1))],
      [((-1), (-1)), (0, 0), ((-1), 0), (1, 0)]
    ]
  | T => [
      /*
          X
        X 0 X

        X
        0 X
        X

        X 0 X
          X

          X
        X 0
          X

       */
      [((-1), 0), (0, 0), (0, 1), (1, 0)],
      [(0, 1), (0, 0), (1, 0), (0, (-1))],
      [((-1), 0), (0, 0), (1, 0), (0, (-1))],
      [((-1), 0), (0, 0), (0, 1), (0, (-1))]
    ]
  };

let renderPart = ((piecePosX, piecePosY), i, (posX, posY)) =>
  <Pixel
    key=(string_of_int(i))
    pos=(piecePosX + posX, piecePosY + posY)
    color="rgb(200, 200, 0)"
  />;

let component = ReasonReact.statelessComponent("Piece");

let make = (~piece, ~rotation, ~pos, _children) => {
  ...component,
  render: _self => {
    let comps =
      List.nth(getPositionsForPeice(piece), rotation)
      |> List.mapi(renderPart(pos));
    <View
      style=Style.(
              style([width(Pct(100.)), height(Pct(100.)), position(Absolute)])
            )>
      (ReasonReact.array(Array.of_list(comps)))
    </View>;
  }
};