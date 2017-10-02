type t = Coordinate.position;

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
  let rotation_matrix = {
    let m = Array.make_matrix 2 2 0;
    m.(0).(0) = 0; m.(1).(0) = 1; m.(0).(1) = -1; m.(1).(1) = 0;
    m
  };
  let m' = Matrix.add m_origin (Matrix.multiply rotation_matrix (Matrix.substract m_t m_origin));
  from_matrix m'
};

let scale t ::factor => {
  let (x, y) = t;
  (int_of_float (factor *. float(x)), int_of_float (factor *. float(y)))
};

let print (x, y) => Js.log ("(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")");

let equal (x1, y1) (x2, y2) => (x1 == x2) && (y1 == y2);
