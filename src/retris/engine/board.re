type tetromino_on_board = {
  tetromino: Tetromino.t,
  top_left_position: Coordinate.position
};

type t = {
  tetrominos_on_board: list tetromino_on_board,
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

let create dimension => {tetrominos_on_board: [], dimension};

let in_moveable_space t (x, y) => {
  let (width, height) = t.dimension;
  x >= 0 && x < width && y < height
};

let in_board t (x, y) => y >= 0 && in_moveable_space t (x, y);

let get_id_and_blocks t :list id_and_block =>
  t.tetrominos_on_board
  |> List.map (
       fun tob => {
         let tetromino = tob.tetromino;
         let (dis_x, dis_y) = tob.top_left_position;
         tetromino.blocks |> List.map (fun (x, y) => (tetromino.id, (x + dis_x, y + dis_y)))
       }
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

let does_collide t tob1 tob2 => {
  let extract_blocks idb => {
    let (_, (x, y)) = idb;
    (x, y)
  };
  let blocks1 = get_id_and_blocks {...t, tetrominos_on_board: [tob2]} |> List.map extract_blocks;
  let blocks2 = get_id_and_blocks {...t, tetrominos_on_board: tob1} |> List.map extract_blocks;
  let intersections =
    blocks1 |> List.filter (fun b => blocks2 |> List.exists (fun c => Block.equal b c));
  let out_of_moveable_space = blocks1 |> List.filter (fun b => b |> in_moveable_space t |> not);
  if (List.length intersections > 0) {
    /* Js.log "Intersect"; */
    Intersection intersections
  } else if (
    List.length out_of_moveable_space > 0
  ) {
    let (_, height) = t.dimension;
    let does_hit_bottom = out_of_moveable_space |> List.exists (fun (_, y) => y == height);
    if does_hit_bottom {
      /* Js.log "HitBottom"; */
      HitBottom
    } else {
      /* Js.log "HitLeftRight"; */
      HitLeftRight
    }
  } else {
    /* Js.log "NoCollision"; */
    NoCollision
  }
};

let put t tetromino top_left_position => {
  /* let (displacement_x, displacement_y) = diplacement; */
  let new_tetromino_on_board = {tetromino, top_left_position};
  let remainders =
    t.tetrominos_on_board
    |> List.filter (
         fun t =>
           switch t.tetromino.tetromino_type {
           | Moveable => false
           | Fixed => true
           }
       );
  switch (does_collide t remainders new_tetromino_on_board) {
  | Intersection _ =>
    /* Js.log "Collide!"; */
    let (_, tl_y) = top_left_position;
    if (tl_y < 0) {
      Full
    } else {
      Collide
    }
  | HitLeftRight => Still
  | HitBottom => Collide
  | NoCollision =>
    let b = {...t, tetrominos_on_board: [new_tetromino_on_board, ...remainders]};
    Moved b
  }
};

exception DuplicateActiveteTetromino;

let active_tetromino t =>
  t.tetrominos_on_board
  |> List.filter (
       fun tetromino_on_board =>
         switch tetromino_on_board.tetromino.tetromino_type {
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

let stop_active_tetromino (t: t) :t => {
  ...t,
  tetrominos_on_board:
    t.tetrominos_on_board
    |> List.map (
         fun tetromino_on_board => {
           ...tetromino_on_board,
           tetromino: Tetromino.freeze tetromino_on_board.tetromino
         }
       )
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
    put t a.tetromino (x', y')
  | None => NoActiveTetromino
  }
};

let rotate_tetromino t => {
  let active = active_tetromino t;
  switch active {
  | Some a =>
    let (x, y) = a.top_left_position;
    put t (Tetromino.rotate a.tetromino) (x, y)
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
           let block_in_row = row |> Array.fold_left (fun accum x => x != 0 ? accum + 1 : accum) 0;
           (i, block_in_row)
         }
       )
    |> Array.to_list
    |> List.filter (fun (_, count) => count == width)
    |> List.map (fun (i, _) => i);
  full_rows |> List.iter (fun l => Js.log ("Full Row: " ^ string_of_int l));
  if (List.length full_rows > 0) {
    Js.log "remove_lines";
    print t
  };
  let tobs' =
    t.tetrominos_on_board
    |> List.map (
         fun tob => {
           let tetromino = tob.tetromino;
           let (_, dis_y) = tob.top_left_position;
           let tetromino' =
             full_rows
             |> List.fold_left
                  (
                    fun (tet: Tetromino.t) full_row => {
                      /* Js.log ("Row:" ^ (string_of_int full_row)); */
                      /* Js.log ("dis_y" ^ (string_of_int dis_y)); */
                      /* Tetromino.print tet; */
                      let to_be_deleted_blocks =
                        tet.blocks |> List.filter (fun (_, y) => y + dis_y == full_row);
                      let tetromino' =
                        to_be_deleted_blocks
                        |> List.fold_left (fun accum b => b |> Tetromino.delete_block accum) tet;
                      /* Js.log "Deleted"; */
                      /* Tetromino.print tetromino'; */
                      let tetromino' =
                        Tetromino.move_down_blocks_above tetromino' (full_row - dis_y);
                      /* Js.log "Moved down"; */
                      /* Tetromino.print tetromino'; */
                      tetromino'
                    }
                  )
                  tetromino;
           {...tob, tetromino: tetromino'}
         }
       );
  let res = {...t, tetrominos_on_board: tobs'};
  if (List.length full_rows > 0) {
    print res
  };
  res
};
