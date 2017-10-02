type t = Coordinate.position;
let to_matrix: t => Matrix.t;
let from_matrix: Matrix.t => t;
let rotate: t => t => t;
let scale: t => factor::float => t;
let print: t => unit;
let equal: t => t => bool;
