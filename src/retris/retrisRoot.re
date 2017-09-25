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
    let x0 = Array.length x;
    let y0 = Array.length y;
    let y1 = if (y0 == 0) {
      0;
    } else {
      Array.length y.(0);
    };
    let z = Array.make_matrix x0 y1 0;
    for i in 0 to (x0-1) {
      for j in 0 to (y1-1) {
        for k in 0 to (y0-1) {
          z.(i).(j) = z.(i).(j) + x.(i).(k) * y.(k).(j);
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
    Matrix.add m_origin (Matrix.multiply Matrix.rotation_matrix (Matrix.substract m_t m_origin))
  };
};

module Renderer = {
  let print blocks (width, height) => {
    for i in 0 to (height - 1) {
      let accum = ref "";
      for j in 0 to (width - 1) {
        let bit = if blocks.(j).(i) {
          "1"
        } else {
          "0"
        };
        accum := !accum ^ bit;
      };
      Js.log !accum;
    };
  };
};

module Tetromino = {
  include Renderer;
  type tetromino_type =
    | Fixed 
    | Moveable;
  type shape = 
    | I | O | L | T | S;

  type t = {
    blocks: blocks,
    dimension: dimension,
    klazz: tetromino_type,
    shape: shape,
  };

  let create (shape: shape) => {
    switch shape {
      | I => {
        /* 0000 */
        /* 0000 */
        /* 0000 */
        /* 1111 */
        let m = Array.make_matrix 4 4 false;
        m.(0).(3) = true; m.(1).(3) = true; m.(2).(3) = true; m.(3).(3) = true;
        { blocks: m, dimension: (4, 4), klazz: Moveable, shape };
      }
      | O => {
        /* 11 */
        /* 11 */
        let m = Array.make_matrix 2 2 true;
        { blocks: m, dimension: (2, 2), klazz: Moveable, shape };
      }
      | L => {
        /* 100 */
        /* 100 */
        /* 110 */
        let m = Array.make_matrix 3 3 false;
        m.(0).(0) = true; m.(0).(1) = true; m.(0).(2) = true; m.(1).(2) = true; 
        { blocks: m, dimension: (3, 3), klazz: Moveable, shape };
      }
      | T => {
        /* 000 */
        /* 010 */
        /* 111 */
        let m = Array.make_matrix 3 3 false;
        m.(0).(2) = true; m.(1).(2) = true; m.(2).(2) = true; m.(1).(1) = true; 
        { blocks: m, dimension: (3, 3), klazz: Moveable, shape };
      }
      | S => {
        /* 000 */
        /* 011 */
        /* 110 */
        let m = Array.make_matrix 3 3 false;
        m.(0).(2) = true; m.(1).(2) = true; m.(1).(1) = true; m.(2).(1) = true; 
        { blocks: m, dimension: (3, 3), klazz: Moveable, shape };
      }
    }
  };
};

module Board = {
  include Renderer;
  type t = {
    blocks: blocks,
    dimension: dimension,
    tetrominos: list Tetromino.t
  };

  let create dimension => {
    let (width, height) = dimension;
    {
      blocks: Array.make_matrix width height false,
      tetrominos: [],
      dimension 
    }
  };

  let in_bound t ((x, y):position) => {
    let (width, height) = t.dimension;
    if (x < 0 || y < 0) {
      false;
    } else if (x >= width || y >= height) {
      false;
    } else {
      true;
    }
  };

  let put t (tetromino: Tetromino.t) ((x, y):position) => {
    switch (tetromino.klazz) {
      | Fixed | Moveable => {
          let blocks' = t.blocks |> (Array.map Array.copy);
          tetromino.blocks |> Array.iteri (fun ix m => {
            m |> Array.iteri (fun iy tet => {
              let x' = ix + x;
              let y' = iy + y;
              if (in_bound t (x', y')) {
                blocks'.(x').(y') = blocks'.(x').(y') || tet;
              } else {
                ();
              }
            }
          )});
          {
            ...t,
            blocks: blocks',
            tetrominos: [tetromino, ...t.tetrominos]
          };
      }          
    };
  };
};

let dimension = (10, 15);

let () = {
  let board = Board.create dimension;
  Board.print board.blocks dimension;
  Js.log "";
  let tetrominos = [(Tetromino.create I),
    (Tetromino.create O),
    (Tetromino.create L),
    (Tetromino.create T),
    (Tetromino.create S)];

  tetrominos |> List.iter (fun (tetromino: Tetromino.t) => {
    switch (tetromino.klazz) {
      | Fixed | Moveable => {
        Tetromino.print tetromino.blocks tetromino.dimension;
        Js.log "";
      };
    };
  });

  tetrominos |> List.iter (fun t => {
    let board' = Board.put board t (0, -3);
    Board.print board'.blocks (board'.dimension);
    Js.log "";
  });

  Matrix.print Matrix.rotation_matrix; Js.log "";
  Matrix.print (Matrix.add Matrix.rotation_matrix Matrix.rotation_matrix); Js.log "";
  Matrix.print (Matrix.substract Matrix.rotation_matrix Matrix.rotation_matrix); Js.log "";
  Matrix.print (Matrix.multiply Matrix.rotation_matrix Matrix.rotation_matrix); Js.log "";

  Matrix.print (Block.to_matrix (2,3)); Js.log "";
  let (x, y) = Block.from_matrix (Block.to_matrix (2,3)); Js.log (string_of_int x); Js.log (string_of_int y);
}
