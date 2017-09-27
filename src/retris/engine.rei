type width = int;
type height = int;
type dimension = (width, height);

type x = int;
type y = int;
type position = (x, y);
type direction = | Down | Right | Left;

module Game: {
  type state = | Playing | Gameover;
  type t;
  let create: dimension => t;
  let tick: t => t;
  let rotate: t => t;
  let move: t => direction => t;
};
let start: unit => unit;
let tick: unit => unit;
