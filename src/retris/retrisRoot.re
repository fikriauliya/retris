type config = {
  width: int,
  height: int
};

module Board = {
  type t = array (array bool);

  let create {width, height} => {
    Array.make_matrix width height false;
  };
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

let config = {
  width: 10,
  height: 15
};

let () = {
  let board = Board.create config;
  Board.print board config;
}
