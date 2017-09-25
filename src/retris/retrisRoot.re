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
  type blocks = array (array bool);
  type t = 
    | Fixed blocks
    | Moveable blocks;

  let i_shape () => {
    /* ffff */
    /* ffff */
    /* ffff */
    /* tttt */
    let m = Array.make_matrix 4 4 false;
    m.(0).(3) = true; m.(1).(3) = true; m.(2).(3) = true; m.(3).(3) = true; Moveable m
  };
  let o_shape () => {
    /* ffff */
    /* ffff */
    /* ttff */
    /* ttff */
    let m = Array.make_matrix 4 4 false;
    m.(0).(2) = true; m.(0).(3) = true; m.(1).(2) = true; m.(1).(3) = true; Moveable m
  };
  let l_shape () => {
    /* ffff */
    /* tfff */
    /* tfff */
    /* ttff */
    let m = Array.make_matrix 4 4 false;
    m.(0).(1) = true; m.(0).(2) = true; m.(0).(3) = true; m.(1).(3) = true; Moveable m;
  };
  let t_shape () => {
    /* ffff */
    /* ffff */
    /* ftff */
    /* tttf */
    let m = Array.make_matrix 4 4 false;
    m.(0).(3) = true; m.(1).(3) = true; m.(2).(3) = true; m.(1).(2) = true; Moveable m;
  };
  let s_shape () => {
    /* ffff */
    /* ffff */
    /* fttf */
    /* ttff */
    let m = Array.make_matrix 4 4 false;
    m.(0).(3) = true; m.(1).(3) = true; m.(1).(2) = true; m.(2).(2) = true; Moveable m;
  };
};

module Board = {
  include Renderer;
  type t = {
    blocks: blocks,
    tetrominos: list Tetromino.t,
    dimension: dimension
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
    switch (tetromino) {
      | Fixed tetromino_blocks | Moveable tetromino_blocks => {
          let blocks' = t.blocks |> (Array.map Array.copy);
          tetromino_blocks |> Array.iteri (fun ix m => {
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
  tetrominos |> List.iter (fun |
    Tetromino.Fixed blocks | Moveable blocks => {
      Tetromino.print blocks (4, 4);
      Js.log "";
    }
  );

  tetrominos |> List.iter (fun t => {
    let board' = Board.put board t (0, -3);
    Board.print board'.blocks (board'.dimension);
    Js.log "";
  });
}
