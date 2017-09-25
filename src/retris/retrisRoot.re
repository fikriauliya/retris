type width = int;
type height = int;
type dimension = (width, height);

type x = int;
type y = int;
type position = (x, y);

type blocks = array (array bool);

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
  type t = {
    blocks: blocks,
    dimension: dimension,
    klazz: tetromino_type
  };

  let i_shape () => {
    /* 0000 */
    /* 0000 */
    /* 0000 */
    /* 1111 */
    let m = Array.make_matrix 4 4 false;
    m.(0).(3) = true; m.(1).(3) = true; m.(2).(3) = true; m.(3).(3) = true;
    {
      blocks: m,
      dimension: (4, 4),
      klazz: Moveable
    };
  };
  let o_shape () => {
    /* 11 */
    /* 11 */
    let m = Array.make_matrix 2 2 true;
    {
      blocks: m,
      dimension: (2, 2),
      klazz: Moveable
    };
  };
  let l_shape () => {
    /* 100 */
    /* 100 */
    /* 110 */
    let m = Array.make_matrix 3 3 false;
    m.(0).(0) = true; m.(0).(1) = true; m.(0).(2) = true; m.(1).(2) = true; 
    {
      blocks: m,
      dimension: (3, 3),
      klazz: Moveable
    };
  };
  let t_shape () => {
    /* 000 */
    /* 010 */
    /* 111 */
    let m = Array.make_matrix 3 3 false;
    m.(0).(2) = true; m.(1).(2) = true; m.(2).(2) = true; m.(1).(1) = true; 
    {
      blocks: m,
      dimension: (3, 3),
      klazz: Moveable
    };
  };
  let s_shape () => {
    /* 000 */
    /* 011 */
    /* 110 */
    let m = Array.make_matrix 3 3 false;
    m.(0).(2) = true; m.(1).(2) = true; m.(1).(1) = true; m.(2).(1) = true; 
    {
      blocks: m,
      dimension: (3, 3),
      klazz: Moveable
    };
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
  let tetrominos = [Tetromino.i_shape (),
    Tetromino.o_shape (),
    Tetromino.l_shape (),
    Tetromino.t_shape (),
    Tetromino.s_shape ()];

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
}
