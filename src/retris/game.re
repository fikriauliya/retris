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
      | None => Some (Engine.Game.create (10, 15))
      | Some g => {
        switch action {
        | Tick => Some (Engine.Game.tick g)
        | ClickLeft => Some (Engine.Game.move g Left)
        | ClickRight => Some (Engine.Game.move g Right)
        | ClickRotate => Some (Engine.Game.rotate g)
        | PressKey key => {
          switch (key) {
            | "h" | "H" => Some (Engine.Game.move g Left)
            | "l" | "L" => Some (Engine.Game.move g Right)
            | "j" | "J" => Some (Engine.Game.tick g)
            | "k" | "K" => Some (Engine.Game.rotate g)
            | _ => None
          };
          }
        | Restart => Some (Engine.Game.create (10, 10))
        }
      }
    };
    switch (new_game) {
      | None => ReasonReact.NoUpdate
      | Some ng => ReasonReact.Update {...state, game: (Some ng) }
    };
  },
  didMount: fun self => {
    self.state.timer_id := Some (Js.Global.setInterval (self.reduce (fun _ => Tick)) 300);
    ReasonReact.NoUpdate
  },
  render: fun self =>
    switch (self.state.game) {
      | None => <div/>
      | Some g => {
        <div>
          (switch (g.state) {
            | Gameover =>
              <div> (ReasonReact.stringToElement "Gameover") </div>
            | Playing => {
              <div>
                <Board board=g.board/>
                <button onClick=(self.reduce (fun _event => ClickLeft))> (ReasonReact.stringToElement "<") </button>
                <button onClick=(self.reduce (fun _event => ClickRotate))> (ReasonReact.stringToElement "o") </button>
                <button onClick=(self.reduce (fun _event => ClickRight))> (ReasonReact.stringToElement ">") </button>
                <input autoFocus=(Js.Boolean.to_js_boolean true) placeholder="hjkl" onKeyDown=(self.reduce (fun event => (PressKey (ReactEventRe.Keyboard.key event)))) />
              </div>
            }
          })
          <button onClick=(self.reduce (fun _event => Restart))> (ReasonReact.stringToElement "Restart") </button>
        </div>
      }
    }
};

