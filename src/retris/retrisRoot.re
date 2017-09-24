type config = {
  width: int,
  height: int
};

module Renderer = {
  let print t {width, height} => {
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
      Js.log accum;
    };
  };
};

module Board = {
  include Renderer;
  type t = array (array bool);

  let create {width, height} => {
    Array.make_matrix width height false;
  };
};

let config = {
  width: 10,
  height: 15
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

let () = {
  let board = Board.create config;
  Board.print board config;
  Js.log "";
  let tetrominos = [Tetromino.i_shape (), 
    Tetromino.o_shape (),
    Tetromino.l_shape (),
    Tetromino.t_shape (),
    Tetromino.s_shape ()];
  tetrominos |> List.iter (fun t => {
    Tetromino.print t {width: 4, height: 4};
    Js.log "";
  }) 
}
