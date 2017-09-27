let component = ReasonReact.statelessComponent "BoardComponent";

let make _children => {
  ...component,
  render: fun _self =>
    <div>
      (ReasonReact.stringToElement ("Im board")) 
    </div>
};

