type tetromino_type =
  | Fixed 
  | Moveable;
type shape = 
  | I | O | L | T | S | J | Z;

type t = {
  id: int,
  blocks: list Block.t,
  klazz: tetromino_type,
  shape: shape,
};

let size: t => int;
let print: t => unit;
let create: shape => t;
let rotate: t => t;
let freeze: t => t;
let delete_block: t => Block.t => t;
let move_down_blocks_above: t => int  => t;
