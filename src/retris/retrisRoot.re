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
    Matrix.add m_origin (Matrix.multiply Matrix.rotation_matrix (Matrix.substract m_t m_origin))
  };
};

module Renderer = {
  let print blocks => {
    let width = Array.length blocks; let height = Array.length blocks.(0);
    Js.log ((string_of_int height) ^ " x " ^ (string_of_int width));
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
  type tetromino_type =
    | Fixed 
    | Moveable;
  type shape = 
    | I | O | L | T | S;

  type t = {
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

  let create (shape: shape) => {
    switch shape {
      | I => {
        /* 0000 */
        /* 0000 */
        /* 0000 */
        /* 1111 */
        { blocks: [(0, 3), (1, 3), (2, 3), (3, 3)], klazz: Moveable, shape };
      }
      | O => {
        /* 11 */
        /* 11 */
        { blocks: [(0, 0), (0, 1), (1, 0), (1, 1)], klazz: Moveable, shape };
      }
      | L => {
        /* 100 */
        /* 100 */
        /* 110 */
        { blocks: [(0, 0), (0, 1), (0, 2), (1, 2)], klazz: Moveable, shape };
      }
      | T => {
        /* 000 */
        /* 010 */
        /* 111 */
        { blocks: [(0, 2), (1, 2), (2, 2), (1, 1)], klazz: Moveable, shape };
      }
      | S => {
        /* 000 */
        /* 011 */
        /* 110 */
        { blocks: [(0, 2), (1, 2), (1, 1), (2, 1)], klazz: Moveable, shape };
      }
    }
  };
};

module Board = {
  include Renderer;
  type t = {
    blocks: blocks,
    tetrominos: list Tetromino.t
  };

  let create dimension => {
    let (width, height) = dimension;
    {
      blocks: Array.make_matrix width height false,
      tetrominos: [],
    }
  };

  let in_bound t ((x, y):position) => {
    let (width, height) = (Array.length t.blocks, Array.length t.blocks.(0));
    if (x < 0 || y < 0) {
      false;
    } else if (x >= width || y >= height) {
      false;
    } else {
      true;
    }
  };
  /*  */
  /* let put t (tetromino: Tetromino.t) ((x, y):position) => { */
  /*   switch (tetromino.klazz) { */
  /*     | Fixed | Moveable => { */
  /*         let blocks' = t.blocks |> (Array.map Array.copy); */
  /*         tetromino.blocks |> Array.iteri (fun ix m => { */
  /*           m |> Array.iteri (fun iy tet => { */
  /*             let x' = ix + x; */
  /*             let y' = iy + y; */
  /*             if (in_bound t (x', y')) { */
  /*               blocks'.(x').(y') = blocks'.(x').(y') || tet; */
  /*             } else { */
  /*               (); */
  /*             } */
  /*           } */
  /*         )}); */
  /*         { */
  /*           blocks: blocks', */
  /*           tetrominos: [tetromino, ...t.tetrominos] */
  /*         }; */
  /*     }           */
  /*   }; */
  /* }; */
};

let dimension = (10, 15);

let () = {
  let board = Board.create dimension;
  Board.print board.blocks;
  Js.log "";
  let tetrominos = [(Tetromino.create I),
    (Tetromino.create O),
    (Tetromino.create L),
    (Tetromino.create T),
    (Tetromino.create S)];

  tetrominos |> List.iter (fun (tetromino: Tetromino.t) => {
    switch (tetromino.klazz) {
      | Fixed | Moveable => {
        Tetromino.print tetromino;
        Js.log "";
      };
    };
  });

  /* tetrominos |> List.iter (fun t => { */
  /*   let board' = Board.put board t (0, -3); */
  /*   Board.print board'.blocks; */
  /*   Js.log ""; */
  /* }); */

  Matrix.print Matrix.rotation_matrix; Js.log "";
  Matrix.print (Matrix.add Matrix.rotation_matrix Matrix.rotation_matrix); Js.log "";
  Matrix.print (Matrix.substract Matrix.rotation_matrix Matrix.rotation_matrix); Js.log "";
  Matrix.print (Matrix.multiply Matrix.rotation_matrix Matrix.rotation_matrix); Js.log "";

  Matrix.print (Block.to_matrix (2,3)); Js.log "";
  let (x, y) = Block.from_matrix (Block.to_matrix (2,3)); Js.log (string_of_int x); Js.log (string_of_int y);
}
