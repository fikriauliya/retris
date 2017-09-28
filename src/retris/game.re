type state = { 
  game: option Engine.Game.t,
  timer_id: ref (option Js.Global.intervalId)
};
type action = | Tick | ClickLeft | ClickRight | ClickRotate | Restart | PressKey string;

let component = ReasonReact.reducerComponent "Game";

let make  _children => {
  ...component,
  initialState: fun () => { 
    game: None,
    timer_id: ref None
  },
  reducer: fun action state => {
    let new_game = switch (state.game) {
      | None => Engine.Game.create (10, 10)
      | Some g => {
        switch action {
        | Tick => Engine.Game.tick g
        | ClickLeft => Engine.Game.move g Left
        | ClickRight => Engine.Game.move g Right
        | ClickRotate => Engine.Game.rotate g
        | PressKey key => {
          switch (key) {
            | "h" | "H" => Engine.Game.move g Left
            | "l" | "L" => Engine.Game.move g Right
            | "j" | "J" => Engine.Game.tick g
            | "k" | "K" => Engine.Game.rotate g
          };
          }
        | Restart => Engine.Game.create (10, 10)
        }
      }
    };
    ReasonReact.Update {...state, game: (Some new_game) }
  },
  didMount: fun self => {
    self.state.timer_id := Some (Js.Global.setInterval (self.reduce (fun _ => Tick)) 500);
    ReasonReact.NoUpdate
  },
  render: fun self =>
    switch (self.state.game) {
      | None => <div/>
      | Some g => {
        <div>
          <Board board=g.board/>
          <button onClick=(self.reduce (fun _event => ClickLeft))> (ReasonReact.stringToElement "<") </button>
          <button onClick=(self.reduce (fun _event => ClickRotate))> (ReasonReact.stringToElement "o") </button>
          <button onClick=(self.reduce (fun _event => ClickRight))> (ReasonReact.stringToElement ">") </button>
          <button onClick=(self.reduce (fun _event => Restart))> (ReasonReact.stringToElement "#?!?") </button>
          <input onKeyPress=(self.reduce (fun event => (PressKey (ReactEventRe.Keyboard.key event)))) />
        </div>
      }
    }
};

