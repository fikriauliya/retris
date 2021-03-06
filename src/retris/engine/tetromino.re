type tetromino_type =
  | Fixed
  | Moveable;

type shape =
  | I
  | O
  | L
  | T
  | S
  | J
  | Z;

type t = {
  id: int,
  blocks: list Block.t,
  tetromino_type,
  top_left_position: Coordinate.position,
  shape
};

let size t => (t.blocks |> List.fold_left (fun accum (x, y) => max (max x accum) y) 0) + 1;

let print t => {
  let s = size t;
  let m = Array.make_matrix s s 0;
  t.blocks |> List.iter (fun (x, y) => m.(x).(y) = 1);
  Matrix.print m
};

let id = ref 0;

let create (shape: shape) => {
  id := !id + 1;
  switch shape {
  | I =>
    /* 0000 */
    /* 0000 */
    /* 0000 */
    /* 1111 */
    {id: !id, blocks: [(0, 3), (1, 3), (2, 3), (3, 3)], tetromino_type: Moveable, shape, top_left_position: (0, 0)}
  | O =>
    /* 11 */
    /* 11 */
    {id: !id, blocks: [(0, 0), (0, 1), (1, 0), (1, 1)], tetromino_type: Moveable, shape, top_left_position: (0, 0)}
  | L =>
    /* 100 */
    /* 100 */
    /* 110 */
    {id: !id, blocks: [(0, 0), (0, 1), (0, 2), (1, 2)], tetromino_type: Moveable, shape, top_left_position: (0, 0)}
  | T =>
    /* 000 */
    /* 010 */
    /* 111 */
    {id: !id, blocks: [(0, 2), (1, 2), (2, 2), (1, 1)], tetromino_type: Moveable, shape, top_left_position: (0, 0)}
  | S =>
    /* 000 */
    /* 011 */
    /* 110 */
    {id: !id, blocks: [(0, 2), (1, 2), (1, 1), (2, 1)], tetromino_type: Moveable, shape, top_left_position: (0, 0)}
  | J =>
    /* 010 */
    /* 010 */
    /* 110 */
    {id: !id, blocks: [(1, 0), (1, 1), (0, 2), (1, 2)], tetromino_type: Moveable, shape, top_left_position: (0, 0)}
  | Z =>
    /* 000 */
    /* 110 */
    /* 011 */
    {id: !id, blocks: [(2, 2), (1, 2), (1, 1), (0, 1)], tetromino_type: Moveable, shape, top_left_position: (0, 0)}
  }
};

let rotate t => {
  let scale_factor =
    switch t.shape {
    | I
    | O => 2.0
    | J
    | L
    | T
    | S
    | Z => 1.0
    };
  let origin =
    switch t.shape {
    | I => (3, 3)
    | O => (1, 1)
    | J
    | L
    | T
    | S
    | Z => (1, 1)
    };
  {
    ...t,
    blocks:
      t.blocks
      |> List.map (Block.scale factor::scale_factor)
      |> List.map (fun b => Block.rotate b origin)
      |> List.map (Block.scale factor::(1.0 /. scale_factor))
  }
};

let freeze t => {...t, tetromino_type: Fixed};

let delete_block t block => {
  ...t,
  blocks: t.blocks |> List.filter (fun b => not (Block.equal b block))
};

let move_down_blocks_above t block_y => {
  ...t,
  blocks: t.blocks |> List.map (fun (x, y) => y < block_y ? (x, y + 1) : (x, y))
};

let absolute_blocks t => {
   let (dis_x, dis_y) = t.top_left_position;
   t.blocks |> List.map (fun (x, y) => (x + dis_x, y + dis_y))
}
