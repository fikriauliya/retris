type width = int;
type height = int;
type dimension = (width, height);

type x = int;
type y = int;
type position = (x, y);

module Renderer = {
  let print t (width, height) => {
    for i in 0 to (height - 1) {
      let accum = ref "";
      for j in 0 to (width - 1) {
        let bit = if t.(j).(i) {
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
  type t = array (array bool);
  let i_shape () => {
    /* ffff */
    /* ffff */
    /* ffff */
    /* tttt */
    let m = Array.make_matrix 4 4 false;
    m.(0).(3) = true; m.(1).(3) = true; m.(2).(3) = true; m.(3).(3) = true; m;
  };
  let o_shape () => {
    /* ffff */
    /* ffff */
    /* ttff */
    /* ttff */
    let m = Array.make_matrix 4 4 false;
    m.(0).(2) = true; m.(0).(3) = true; m.(1).(2) = true; m.(1).(3) = true; m;
  };
  let l_shape () => {
    /* ffff */
    /* tfff */
    /* tfff */
    /* ttff */
    let m = Array.make_matrix 4 4 false;
    m.(0).(1) = true; m.(0).(2) = true; m.(0).(3) = true; m.(1).(3) = true; m;
  };
  let t_shape () => {
    /* ffff */
    /* ffff */
    /* ftff */
    /* tttf */
    let m = Array.make_matrix 4 4 false;
    m.(0).(3) = true; m.(1).(3) = true; m.(2).(3) = true; m.(1).(2) = true; m;
  };
  let s_shape () => {
    /* ffff */
    /* ffff */
    /* fttf */
    /* ttff */
    let m = Array.make_matrix 4 4 false;
    m.(0).(3) = true; m.(1).(3) = true; m.(1).(2) = true; m.(2).(2) = true; m;
  };
};

module Board = {
  include Renderer;
  type tetromino = 
    | Fixed Tetromino.t
    | Moveable Tetromino.t;

  type t = {
    blocks: array (array bool),
    tetrominos: list tetromino,
  };

  let create (width, height) => {
    {
      blocks: Array.make_matrix width height false,
      tetrominos: []
    }
  };

  let in_bound ((x, y):position) ((width, height):dimension) => {
    if (x < 0 || y < 0) {
      false;
    } else if (x >= width || y >= height) {
      false;
    } else {
      true;
    }
  };

  let put {blocks} tetromino ((x, y):position) dimension => {
    switch (tetromino) {
      | Fixed tetromino => 
          tetromino |> Array.iteri (fun ix m => {
            m |> Array.iteri (fun iy tet => {
              let x' = ix + x;
              let y' = iy + y;
              if (in_bound (x', y') dimension) {
                blocks.(x').(y') = blocks.(x').(y') || tet;
              } else {
                ();
              }
            }
          )});
      | Moveable _ => ()
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
  tetrominos |> List.iter (fun t => {
    Tetromino.print t (4, 4);
    Js.log "";
  });

  tetrominos |> List.iter (fun t => {
    Board.put board (Fixed t) (0, -3) dimension;
    Board.print board.blocks dimension;
    Js.log "";
  });
}
