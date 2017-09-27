let component = ReasonReact.statelessComponent "BoardComponent";

let make ::board _children => {
  ...component,
  render: fun _self => {
    let m = Engine.Board.matrix board;
    let m' = Engine.Matrix.transpose m;

    <table>
      <tbody>
        (ReasonReact.arrayToElement (m' |> Array.mapi (fun y row => {
          <tr key=(string_of_int y)>
            (ReasonReact.arrayToElement (row |> Array.mapi (fun x e => {
              <td key=(string_of_int x)> (ReasonReact.stringToElement (string_of_int e)) </td>
            })))
          </tr>
        })))
      </tbody>
    </table>
  }
};

