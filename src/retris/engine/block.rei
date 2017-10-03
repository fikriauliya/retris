type t = Coordinate.position;

let rotate: t => t => t;

let scale: t => factor::float => t;

let print: t => unit;

let equal: t => t => bool;
