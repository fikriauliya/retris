let component = ReasonReact.statelessComponent "BoardComponent";

let make ::board _children => {
  ...component,
  render: fun _self => {
    let m = Engine.Board.matrix board;
    let m' = Engine.Matrix.transpose m;

    <table style=(
      ReactDOMRe.Style.make border::"2px solid black" ())>
      <tbody>
        (ReasonReact.arrayToElement (m' |> Array.mapi (fun y row => {
          <tr key=(string_of_int y)>
            (ReasonReact.arrayToElement (row |> Array.mapi (fun x e => {
              let filled_in = e > 0;
              let color = filled_in ? "blue" : "white";
              <td key=(string_of_int x) style=(
                ReactDOMRe.Style.make color::color ())> (ReasonReact.stringToElement ("x")) </td>
            })))
          </tr>
        })))
      </tbody>
    </table>
  }
};

