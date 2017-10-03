type state = {
  game: option Engine.t,
  timer_id: ref (option Js.Global.intervalId)
};

type action =
  | Tick
  | ClickLeft
  | ClickRight
  | ClickRotate
  | Restart
  | PressKey string;

let component = ReasonReact.reducerComponent "Game";

let make _children => {
  ...component,
  initialState: fun () => {game: None, timer_id: ref None},
  reducer: fun action state => {
    let new_game =
      switch state.game {
      | None => Some (Engine.create (10, 15))
      | Some g =>
        switch action {
        | Tick => Some (Engine.tick g)
        | ClickLeft => Some (Engine.move g Left)
        | ClickRight => Some (Engine.move g Right)
        | ClickRotate => Some (Engine.rotate g)
        | PressKey key =>
          switch key {
          | "h"
          | "H" => Some (Engine.move g Left)
          | "l"
          | "L" => Some (Engine.move g Right)
          | "j"
          | "J" => Some (Engine.tick g)
          | "k"
          | "K" => Some (Engine.rotate g)
          | _ => None
          }
        | Restart => Some (Engine.create (10, 15))
        }
      };
    switch new_game {
    | None => ReasonReact.NoUpdate
    | Some ng => ReasonReact.Update {...state, game: Some ng}
    }
  },
  didMount: fun self => {
    self.state.timer_id := Some (Js.Global.setInterval (self.reduce (fun _ => Tick)) 300);
    ReasonReact.NoUpdate
  },
  render: fun self =>
    switch self.state.game {
    | None => <div />
    | Some g =>
      <div>
        (
          switch g.state {
          | Gameover => <div> (ReasonReact.stringToElement "Gameover") </div>
          | Playing =>
            <div>
              <BoardComp board=g.board />
              <button onClick=(self.reduce (fun _event => ClickLeft))>
                (ReasonReact.stringToElement "<")
              </button>
              <button onClick=(self.reduce (fun _event => ClickRotate))>
                (ReasonReact.stringToElement "o")
              </button>
              <button onClick=(self.reduce (fun _event => ClickRight))>
                (ReasonReact.stringToElement ">")
              </button>
              <input
                autoFocus=(Js.Boolean.to_js_boolean true)
                placeholder="hjkl"
                onKeyDown=(self.reduce (fun event => PressKey (ReactEventRe.Keyboard.key event)))
              />
            </div>
          }
        )
        <button onClick=(self.reduce (fun _event => Restart))>
          (ReasonReact.stringToElement "Restart")
        </button>
      </div>
    }
};
