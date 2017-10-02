type state = | Playing | Gameover;
type t = {
  board: Board.t,
  state: state
};
let create: Coordinate.dimension => t;
let tick: t => t;
let rotate: t => t;
let move: t => Board.direction => t;
