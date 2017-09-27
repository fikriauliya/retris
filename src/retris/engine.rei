type width = int;
type height = int;
type dimension = (width, height);

type x = int;
type y = int;
type position = (x, y);
type direction = | Down | Right | Left;

module Matrix: {
  type t = array (array int);
  let transpose: t => t;
};

module Block: {
  type t = position;
};

module Tetromino: {
  type tetromino_type =
    | Fixed 
    | Moveable;
  type shape = 
    | I | O | L | T | S;

  type t = {
    id: int,
    blocks: list Block.t,
    klazz: tetromino_type,
    shape: shape,
  };
};

module Board: {
  type tetromino_on_board = {
    tetromino: Tetromino.t,
    top_left_position: position
  };
  type t = {
    tetrominos_on_board: list tetromino_on_board,
    dimension: dimension
  };
  type id_and_block = (int, Block.t);

  let get_id_and_blocks: t => (list id_and_block);
  let print: t => unit;
  let matrix: t => array (array int);
};

module Game: {
  type state = | Playing | Gameover;
  type t = {
    board: Board.t,
    state: state
  };
  let create: dimension => t;
  let tick: t => t;
  let rotate: t => t;
  let move: t => direction => t;
};
let start: unit => unit;
let tick: unit => unit;
