type width = int;
type height = int;
type dimension = (width, height);

type x = int;
type y = int;
type position = (x, y);

type blocks = array (array bool);
type direction = | Down | Right | Left;

module Result = {
  type t 'a 'b = 
    | Ok 'a
    | Error 'b
};

module Matrix = {
  let multiply x y => {
    let x_width = Array.length x;
    let x_height = Array.length x.(0);
    let y_width = Array.length y;
    let z = Array.make_matrix y_width x_height 0;
    for i in 0 to (x_height-1) {
      for j in 0 to (y_width-1) {
        for k in 0 to (x_width-1) {
          z.(j).(i) = z.(j).(i) + x.(k).(i) * y.(j).(k);
        }
      }
    };
    z 
  };

  let negate x => x |> Array.map(fun c => c |> Array.map (fun v => -v));
  let add x y => x |> Array.mapi (fun ix c => c |> Array.mapi (fun iy v => y.(ix).(iy) + v));
  let substract x y => add x (negate y);

  let print m => {
    let width = Array.length m; let height = Array.length m.(0);
    Js.log ((string_of_int height) ^ " x " ^ (string_of_int width));

    for i in 0 to (height - 1) {
      let accum = ref "";
      for j in 0 to (width - 1) {
        accum := !accum ^ "\t" ^ (string_of_int m.(j).(i));
      };
      Js.log !accum;
    };
  };

  let rotation_matrix = {
    let m = Array.make_matrix 2 2 0;
    m.(0).(0) = 0; m.(1).(0) = -1; m.(0).(1) = 1; m.(1).(1) = 0;
    m
  };
};

module Block = {
  type t = position;

  let to_matrix t => {
    let m = Array.make_matrix 1 2 0;
    let (x, y) = t; m.(0) = [|x, y|];
    m;
  };

  let from_matrix m => {
    let x = m.(0).(0); let y = m.(0).(1); (x, y)
  };

  let rotate t origin => {
    let m_t = to_matrix t; let m_origin = to_matrix origin;
    let m' = Matrix.add m_origin (Matrix.multiply Matrix.rotation_matrix (Matrix.substract m_t m_origin));
    from_matrix m'
  };

  let scale t ::factor => {
    t |> List.map(fun | (x, y) => 
      (int_of_float (factor *. float(x)), int_of_float (factor *. float(y)))
    );
  };

  let print (x, y) => Js.log ("(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")");

  let equal (x1, y1) (x2, y2) => (x1 == x2) && (y1 == y2);
};

module Tetromino = {
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

  let size t => 
    t.blocks 
    |> List.fold_left 
      (fun accum p => {
        let (x, y) = p;
        (max (max x accum) y)
      }) 0
    |> (fun s => s + 1);

  let print t => {
    let s = size t;
    let m = Array.make_matrix s s 0;
    t.blocks |> List.iter (fun p => {
      let (x, y) = p;
      m.(x).(y) = 1;
    });
    Matrix.print m;
  };

  let id = ref (0);

  let create (shape: shape) => {
    id := !id + 1;
    switch shape {
      | I => {
        /* 0000 */
        /* 0000 */
        /* 0000 */
        /* 1111 */
        { id: !id, blocks: [(0, 3), (1, 3), (2, 3), (3, 3)], klazz: Moveable, shape };
      }
      | O => {
        /* 11 */
        /* 11 */
        { id: !id, blocks: [(0, 0), (0, 1), (1, 0), (1, 1)], klazz: Moveable, shape };
      }
      | L => {
        /* 100 */
        /* 100 */
        /* 110 */
        { id: !id, blocks: [(0, 0), (0, 1), (0, 2), (1, 2)], klazz: Moveable, shape };
      }
      | T => {
        /* 000 */
        /* 010 */
        /* 111 */
        { id: !id, blocks: [(0, 2), (1, 2), (2, 2), (1, 1)], klazz: Moveable, shape };
      }
      | S => {
        /* 000 */
        /* 011 */
        /* 110 */
        { id: !id, blocks: [(0, 2), (1, 2), (1, 1), (2, 1)], klazz: Moveable, shape };
      }
    };
  };

  let rotate t => {
    let scale_factor = switch (t.shape) {
      | I | O  => 2.0
      | L | T | S => 1.0
      };
    let origin = switch (t.shape) {
      | I => (3, 3)
      | O => (1, 1)
      | L | T | S => (1, 1)
      };
    {
      ...t,
      blocks: t.blocks
        |> Block.scale factor::scale_factor 
        |> fun blocks => blocks |> List.map (fun b => Block.rotate b origin)
        |> Block.scale factor::(1.0/.scale_factor)
    };
  };

  let freeze t => { ...t, klazz: Fixed};
};

module Board = {
  type tetromino_on_board = {
    tetromino: Tetromino.t,
    top_left_position: position
  };
  type t = {
    tetrominos_on_board: list tetromino_on_board,
    dimension: dimension
  };
  type movement = | Moved t | Collide | Still | NoActiveTetromino | Full;
  type collision = | Intersection (list position) | HitBottom | HitLeftRight | NoCollision;

  let create dimension => { tetrominos_on_board: [], dimension };

  let in_moveable_space t ((x, y):position) => {
    let (width, height) = t.dimension;
    (x >= 0 && x < width && y < height);
  };

  let in_board t ((x, y):position) => (y >= 0 && (in_moveable_space t (x, y)));

  let get_id_and_blocks t => {
    t.tetrominos_on_board
    |> List.map (fun tob => {
      let tetromino = tob.tetromino;
      let (dis_x, dis_y) = tob.top_left_position;
      tetromino.blocks 
      |> List.map (fun block => {
        let (x, y) = block;
        (tetromino.id, (x + dis_x, y + dis_y));
      })
    })
    |> List.concat
  };

  let matrix t => {
    let (width, height) = t.dimension;
    let m = Array.make_matrix width height 0;
    t |> get_id_and_blocks |> List.iter (fun idb => {
      let (id, (x, y)) = idb;
      if (in_board t (x, y)) {
        m.(x).(y) = id;
      }
    });
    m
  };

  let print t => {
    Matrix.print (matrix t);
  };

  let does_collide t tetrominos_on_board new_tetromino_on_board => {
    let extract_blocks idb => {
      let (_, (x, y)) = idb;
      (x, y)
    };

    let blocks1 = get_id_and_blocks {
      ...t,
      tetrominos_on_board: [new_tetromino_on_board]
    } |> List.map(extract_blocks);
    /* Js.log "blocks1"; */
    /* blocks1 |> (List.iter Block.print); */

    let blocks2 = get_id_and_blocks {
      ...t,
      tetrominos_on_board: tetrominos_on_board
    } |> List.map(extract_blocks);
    /* Js.log "blocks2"; */
    /* blocks2 |> (List.iter Block.print); */

    let intersections = blocks1 |> List.filter(fun b => 
      blocks2 |> List.exists(fun c => Block.equal b c));

    let out_of_moveable_space = blocks1 
      |> List.filter (fun b => b |> (in_moveable_space t) |> (not));
      
    if ((List.length intersections) > 0) {
      Js.log "Intersect";
      Intersection intersections;
    } else if ((List.length out_of_moveable_space) > 0) {
      let (_, height) = t.dimension;
      /* Js.log "out_of_moveable_space"; */
      /* out_of_moveable_space |> (List.iter Block.print); */
      let does_hit_bottom = out_of_moveable_space 
        |> List.exists (fun (_, y) => y == height);
      if (does_hit_bottom) {
        Js.log "HitBottom";
        HitBottom;
      } else {
        Js.log "HitLeftRight";
        HitLeftRight;
      }
    } else {
      Js.log "NoCollision";
      NoCollision;
    }
  };

  let put t tetromino top_left_position => {
    /* let (displacement_x, displacement_y) = diplacement; */
    let new_tetromino_on_board = { tetromino, top_left_position };
    let remainders = t.tetrominos_on_board |> List.filter (fun t => {
      switch (t.tetromino.klazz) {
        | Moveable => false
        | Fixed => true
      };
    });
    switch (does_collide t remainders new_tetromino_on_board) {
      | Intersection _ => {
        Js.log "Collide!";
        let (_, tl_y) = top_left_position;
        if (tl_y < 0) {
          Full;
        } else {
          Collide;
        }
      }
      | HitLeftRight => Still
      | HitBottom => Collide
      | NoCollision => {
        let b = {
          ...t,
          tetrominos_on_board: [new_tetromino_on_board, ...remainders]
        };
        Moved b;
      }
    }
  };

  exception DuplicateActiveteTetromino;

  let active_tetromino t => {
    t.tetrominos_on_board 
      |> List.filter (fun tetromino_on_board => {
        switch (tetromino_on_board.tetromino.klazz) {
          | Moveable => true
          | Fixed => false
          }
      })
      |> (fun ls => switch (ls) {
        | [] => None
        | [h] => Some h
        | [_, ..._] => raise DuplicateActiveteTetromino
      });
  };

  let stop_active_tetromino (t:t):t => {
    ...t,
    tetrominos_on_board: t.tetrominos_on_board |> List.map (fun tetromino_on_board => 
      { 
        ...tetromino_on_board,
        tetromino: (Tetromino.freeze tetromino_on_board.tetromino)
      }
    )
  };

  let move_tetromino t direction => {
    let active = active_tetromino t;
    switch (active) {
      | Some a =>  {
        let (x, y) = a.top_left_position;
        let (dx, dy) = switch (direction) {
          | Down => (0, 1)
          | Right => (1, 0)
          | Left => (-1, 0)
          };
        let (x', y') = (x + dx, y + dy);
        put t a.tetromino (x', y');
      }
      | None => NoActiveTetromino;
    }
  };
};

module Game = {
  type state = | Playing | Gameover;
  type t = {
    board: Board.t,
    state: state
  };

  let create dimension => {
    Random.init 0;
    {
      board: (Board.create dimension),
      state: Playing
    };
  };

  let rec tick (t: t) :t => {
    let m = switch (Board.active_tetromino t.board) {
      | None => {
        let all_shapes = [|Tetromino.I, O, L, T, S|];
        let random_shape = all_shapes.(Random.int (Array.length all_shapes));
        let (width, _) = t.board.dimension;
        let random_tetromino = Tetromino.create random_shape;
        let rt_size = Tetromino.size random_tetromino;
        Board.put t.board random_tetromino (width / 2, (-rt_size));
      }
      | Some _ => {
        Board.move_tetromino t.board Down;
      }
    };
    switch (m) {
      | Moved board => { ...t, board }
      | Still | NoActiveTetromino  => t
      | Collide => {
        let new_t = {
          ...t,
          board: (Board.stop_active_tetromino t.board)
        };
        tick new_t
      }
      | Full => {
        Js.log "Gameover!";
        {...t, state: Gameover}
      }
    }
  };
};

let dimension = (10, 10);

let () = {
  let game = ref (Game.create dimension);
  for i in 0 to 35 {
    Js.log ("Move #" ^ (string_of_int i));
    game := Game.tick !game;
    Board.print (!game).board;
  }
}
