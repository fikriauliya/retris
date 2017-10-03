type t = {
  tetrominos: list Tetromino.t,
  dimension: Coordinate.dimension
};

type direction =
  | Down
  | Right
  | Left;

type movement =
  | Moved t
  | Collide
  | Still
  | NoActiveTetromino
  | Full;

type collision =
  | Intersection (list Coordinate.position)
  | HitBottom
  | HitLeftRight
  | NoCollision;

let create: Coordinate.dimension => t;

let matrix: t => Matrix.t;

let put: t => Tetromino.t => movement;

let active_tetromino: t => option Tetromino.t;

let stop_active_tetromino: t => t;

let move_tetromino: t => direction => movement;

let rotate_tetromino: t => movement;

let remove_lines: t => t;
