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

type id_and_block = (int, Block.t);

let create dimension => {tetrominos: [], dimension};

let in_moveable_space t (x, y) => {
  let (width, height) = t.dimension;
  x >= 0 && x < width && y < height
};

let in_board t (x, y) => y >= 0 && in_moveable_space t (x, y);

let get_id_and_blocks t :list id_and_block =>
  t.tetrominos
  |> List.map (
       fun tetromino =>
         tetromino |> Tetromino.absolute_blocks |> List.map (fun b => (tetromino.id, b))
     )
  |> List.concat;

let matrix t => {
  let (width, height) = t.dimension;
  let m = Array.make_matrix width height 0;
  get_id_and_blocks t
  |> List.iter (
       fun (id, (x, y)) =>
         if (in_board t (x, y)) {
           m.(x).(y) = id
         }
     );
  m
};

let print t => Matrix.print (matrix t);

let fixed_tetrominos t =>
  t.tetrominos
  |> List.filter (
       fun (tetromino: Tetromino.t) =>
         switch tetromino.tetromino_type {
         | Moveable => false
         | Fixed => true
         }
     );

exception DuplicateActiveteTetromino;

let active_tetromino t =>
  t.tetrominos
  |> List.filter (
       fun (tetromino: Tetromino.t) =>
         switch tetromino.tetromino_type {
         | Moveable => true
         | Fixed => false
         }
     )
  |> (
    fun ls =>
      switch ls {
      | [] => None
      | [h] => Some h
      | [_, ..._] => raise DuplicateActiveteTetromino
      }
  );

let does_collide t tetromino => {
  let extract_blocks idb => {
    let (_, block) = idb;
    block
  };
  let blocks = get_id_and_blocks {...t, tetrominos: [tetromino]} |> List.map extract_blocks;
  let fixed_blocks =
    get_id_and_blocks {...t, tetrominos: fixed_tetrominos t} |> List.map extract_blocks;
  let intersections =
    blocks |> List.filter (fun b => fixed_blocks |> List.exists (fun c => Block.equal b c));
  let out_of_moveable_space_blocks =
    blocks |> List.filter (fun b => b |> in_moveable_space t |> not);
  if (List.length intersections > 0) {
    Intersection intersections
  } else if (
    List.length out_of_moveable_space_blocks > 0
  ) {
    let (_, height) = t.dimension;
    let does_hit_bottom = out_of_moveable_space_blocks |> List.exists (fun (_, y) => y == height);
    if does_hit_bottom {HitBottom} else {HitLeftRight}
  } else {
    NoCollision
  }
};

let put t tetromino =>
  switch (does_collide t tetromino) {
  | Intersection _ =>
    let (_, tl_y) = tetromino.top_left_position;
    if (tl_y < 0) {
      Full
    } else {
      Collide
    }
  | HitLeftRight => Still
  | HitBottom => Collide
  | NoCollision =>
    let remainders = fixed_tetrominos t;
    let b = {...t, tetrominos: [tetromino, ...remainders]};
    Moved b
  };

let stop_active_tetromino (t: t) :t => {
  ...t,
  tetrominos: t.tetrominos |> List.map Tetromino.freeze
};

let move_tetromino t direction => {
  let active = active_tetromino t;
  switch active {
  | Some a =>
    let (x, y) = a.top_left_position;
    let (dx, dy) =
      switch direction {
      | Down => (0, 1)
      | Right => (1, 0)
      | Left => ((-1), 0)
      };
    let (x', y') = (x + dx, y + dy);
    put t {...a, top_left_position: (x', y')}
  | None => NoActiveTetromino
  }
};

let rotate_tetromino t => {
  let active = active_tetromino t;
  switch active {
  | Some a => put t (Tetromino.rotate a)
  | None => NoActiveTetromino
  }
};

let remove_lines (t: t) => {
  let (width, _) = t.dimension;
  let full_rows =
    t
    |> matrix
    |> Matrix.transpose
    |> Array.mapi (
         fun i row => {
           let blocks_count_in_row =
             row |> Array.fold_left (fun accum x => x != 0 ? accum + 1 : accum) 0;
           (i, blocks_count_in_row)
         }
       )
    |> Array.to_list
    |> List.filter (fun (_, count) => count == width)
    |> List.map (fun (i, _) => i);
  /* full_rows |> List.iter (fun l => Js.log ("Full Row: " ^ string_of_int l)); */
  /* if (List.length full_rows > 0) { */
    /* Js.log "remove_lines"; */
    /* print t */
  /* }; */
  let tetrominos' =
    t.tetrominos
    |> List.map (
         fun (tetromino: Tetromino.t) => {
           let (_, offset_y) = tetromino.top_left_position;
           full_rows
           |> List.fold_left
                (
                  fun (tet: Tetromino.t) full_row => {
                    let to_be_deleted_blocks =
                      tet.blocks |> List.filter (fun (_, y) => y + offset_y == full_row);
                    let tetromino' =
                      to_be_deleted_blocks
                      |> List.fold_left (fun accum b => b |> Tetromino.delete_block accum) tet;
                    Tetromino.move_down_blocks_above tetromino' (full_row - offset_y);
                  }
                )
                tetromino
         }
       );
  {...t, tetrominos: tetrominos'};
  /* if (List.length full_rows > 0) { */
  /*   print res */
  /* }; */
};
