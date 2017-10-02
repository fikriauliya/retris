type tetromino_on_board = {
  tetromino: Tetromino.t,
  top_left_position: Coordinate.position
};
type t = {
  tetrominos_on_board: list tetromino_on_board,
  dimension: Coordinate.dimension
};
type direction = | Down | Right | Left;
type movement = | Moved t | Collide | Still | NoActiveTetromino | Full;
type collision = | Intersection (list Coordinate.position) | HitBottom | HitLeftRight | NoCollision;
type id_and_block = (int, Block.t);

let create: Coordinate.dimension => t;
let in_moveable_space: t => Coordinate.position => bool;
let in_board: t => Coordinate.position => bool;
let get_id_and_blocks: t => list id_and_block;
let matrix: t => Matrix.t;
let print: t => unit;
let does_collide: t => list tetromino_on_board => tetromino_on_board => collision;
let put: t => Tetromino.t => Coordinate.position => movement;
let active_tetromino: t => (option tetromino_on_board);
let stop_active_tetromino: t => t;
let move_tetromino: t => direction => movement;
let rotate_tetromino: t => movement;
let remove_lines: t => t;
