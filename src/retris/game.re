type state = { 
  game: Engine.Game.t,
  timer: int
};
type action = | Click;

/* This is the basic component. */
let component = ReasonReact.reducerComponent "Game";

/* let handleClick _event _self => Js.log "clicked!"; */

let make  _children => {
  ...component,
  initialState: fun () => { 
    game: Engine.Game.create (10, 10),
    timer: 0
  },
  reducer: fun action state =>
    switch action {
      | Click => ReasonReact.Update { ...state, timer: state.timer + 1}
    },
    /* ReasonReact.NoUpdate, */
    /* switch action { */
    /*   | Click => ReasonReact.Update { ...state, game: state.counter + 1 } */
    /* }, */
  render: fun _self =>
    <div>
      /* <button onClick=(self.reduce (fun _event => Click))>  */
      /*   (ReasonReact.stringToElement (string_of_int self.state.counter))  */
      /* </button> */
      <Board />
    </div>
};

