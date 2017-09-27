type state = { 
  game: option Engine.Game.t,
  timer_id: ref (option Js.Global.intervalId)
};
type action = | Tick;

let component = ReasonReact.reducerComponent "Game";

let make  _children => {
  ...component,
  initialState: fun () => { 
    game: None,
    timer_id: ref None
  },
  reducer: fun action state =>
    switch (state.game) {
      | None => ReasonReact.Update { ...state, game: (Some (Engine.Game.create (10, 10))) }
      | Some g => {
        switch action {
        | Tick => ReasonReact.Update {...state, game: (Some (Engine.Game.tick g)) }
        }
      }
    },
  didMount: fun self => {
    self.state.timer_id := Some (Js.Global.setInterval (self.reduce (fun _ => Tick)) 300);
    ReasonReact.NoUpdate
  },
  render: fun {state} =>
    switch (state.game) {
      | None => <div/>
      | Some g => {
        <div>
          <Board board=g.board/>
        </div>
      }
    }
};

