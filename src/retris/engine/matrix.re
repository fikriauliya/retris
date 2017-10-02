type t = array (array int);
let multiply x y => {
  let x_width = Array.length x;
  let x_height = Array.length x.(0);
  let y_width = Array.length y;
  let z = Array.make_matrix y_width x_height 0;
  for i in 0 to (x_height-1) {
    for j in 0 to (y_width-1) {
      for k in 0 to (x_width-1) {
        z.(j).(i) = z.(j).(i) + x.(k).(i) * y.(j).(k);
      }
    }
  };
  z 
};

let negate x => x |> Array.map(fun c => c |> Array.map (fun v => -v));
let add x y => x |> Array.mapi (fun ix c => c |> Array.mapi (fun iy v => y.(ix).(iy) + v));
let substract x y => add x (negate y);

let print m => {
  let width = Array.length m; let height = Array.length m.(0);
  Js.log ((string_of_int height) ^ " x " ^ (string_of_int width));

  for i in 0 to (height - 1) {
    let accum = ref "";
    for j in 0 to (width - 1) {
      accum := !accum ^ "\t" ^ (string_of_int m.(j).(i));
    };
    Js.log !accum;
  };
};

let transpose m => {
  let (width, height) = ((Array.length m), Array.length m.(0));
  let m' = Array.make_matrix height width 0;
  m |> Array.iteri (fun i col =>
    col |> Array.iteri (fun j _ => m'.(j).(i) = m.(i).(j))
  );
  m';
}
