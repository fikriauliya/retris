type width = int;
type height = int;
type dimension = (width, height);

type x = int;
type y = int;
type position = (x, y);

type blocks = array (array bool);

module Matrix = {
  let multiply x y => {
    /* https://rosettacode.org/wiki/Matrix_multiplication#OCaml */
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

  let print t => {
    let size = t.blocks |> List.fold_left 
      (fun accum p => {
        let (x, y) = p;
        (max (max x accum) y)
      })
      0 |> (fun s => s + 1);

    let m = Array.make_matrix size size 0;
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
  let create dimension => { tetrominos_on_board: [], dimension };

  let in_bound t ((x, y):position) => {
    let (width, height) = t.dimension;
    if (x < 0 || y < 0 || x >= width || y >= height) {
      false;
    } else {
      true;
    }
  };

  let print t => {
    let (width, height) = t.dimension;
    let m = Array.make_matrix width height 0;
    t.tetrominos_on_board
      |> List.iter (fun tob => {
        let tetromino = tob.tetromino;
        let (dis_x, dis_y) = tob.top_left_position;
        tetromino.blocks |> List.iter (fun block => {
          let (x, y) = block;
          let (x', y') = (x + dis_x, y + dis_y);
          if (in_bound t (x', y')) {
            m.(x').(y') = tetromino.id;
          }
        })
    });
    Matrix.print m;
  };

  let put t tetromino top_left_position => {
    /* let (displacement_x, displacement_y) = diplacement; */
    let new_tetromino_on_board = { tetromino, top_left_position };
    let remainders = t.tetrominos_on_board;
    {
      ...t,
      tetrominos_on_board: [new_tetromino_on_board, ...remainders]
    };
  };
};

let dimension = (10, 15);

let () = {
  let board = Board.create dimension;
  Board.print board;
  let (x, y) = Block.from_matrix (Block.to_matrix (2,3)); Js.log (string_of_int x); Js.log (string_of_int y);

  let tetrominos = [(Tetromino.create I),
    (Tetromino.create O),
    (Tetromino.create L),
    (Tetromino.create T),
    (Tetromino.create S)];

  tetrominos |> List.iter (fun (tetromino: Tetromino.t) => {
    switch (tetromino.klazz) {
      | Fixed | Moveable => {
        Tetromino.print tetromino;
        Tetromino.print (tetromino |> Tetromino.rotate);
        Tetromino.print (tetromino |> Tetromino.rotate |> Tetromino.rotate);
        Tetromino.print (tetromino |> Tetromino.rotate |> Tetromino.rotate |> Tetromino.rotate);
        Tetromino.print (tetromino |> Tetromino.rotate |> Tetromino.rotate |> Tetromino.rotate |> Tetromino.rotate);
        Js.log "";
      };
    };
  });
  tetrominos |> List.iter (fun t => {
    let board' = Board.put board t (0, 0);
    Board.print board';
    Js.log "";
  });
}
