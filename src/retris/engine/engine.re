type state = | Playing | Gameover;
type t = {
  board: Board.t,
  state: state
};

let create dimension => {
  Random.self_init ();
  {
    board: (Board.create dimension),
    state: Playing
  };
};

let rec update t (m:Board.movement) direction => {
  switch (m) {
    | Moved board => { ...t, board }
    | Still | NoActiveTetromino  => t
    | Collide => {
      switch (direction) {
        | Board.Down => {
          let new_t = {
            ...t,
            board: (Board.remove_lines (Board.stop_active_tetromino t.board))
          };
          tick new_t
        }
        | Left | Right => t
      }
    }
    | Full => {
      /* Js.log "Gameover!"; */
      {...t, state: Gameover}
    }
  }
} and tick (t: t) :t => {
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
  update t m Down;
};

let rotate t => update t (Board.rotate_tetromino t.board) Down;
let move t direction => update t (Board.move_tetromino t.board direction) direction;
