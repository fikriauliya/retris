let component = ReasonReact.statelessComponent "BoardComponent";
let basic_colors = [|"#C0C0C0", "#808080", "#000000", "#FF0000", "#800000", "#FFFF00", "#808000", "#00FF00", "#008000",
"#00FFFF", "#008080", "#0000FF", "#000080", "#FF00FF", "#800080"|];

let make ::board _children => {
  ...component,
  render: fun _self => {
    let m = Board.matrix board;
    let m' = Matrix.transpose m;

    <table style=(
      ReactDOMRe.Style.make border::"2px solid black" ())>
      <tbody>
        (ReasonReact.arrayToElement (m' |> Array.mapi (fun y row => {
          <tr key=(string_of_int y)>
            (ReasonReact.arrayToElement (row |> Array.mapi (fun x e => {
              let filled_in = e > 0;
              let backgroundColor = filled_in ? basic_colors.(e mod (Array.length basic_colors)) : "white";
              <td key=(string_of_int x) style=(
                ReactDOMRe.Style.make 
                  backgroundColor::backgroundColor
                  width::"20px"
                  height::"20px"
                  ())
               >  </td>
            })))
          </tr>
        })))
      </tbody>
    </table>
  }
};

