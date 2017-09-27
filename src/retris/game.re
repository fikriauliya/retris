type state = { 
  game: Engine.Game.t,
  timer: int
};
type action = | Click;

let component = ReasonReact.reducerComponent "Game";

let game = ref (Engine.Game.create (10, 10));
for _ in 0 to 20 {
  game := Engine.Game.tick !game;
};

let make  _children => {
  ...component,
  initialState: fun () => { 
    game: (!game),
    timer: 0
  },
  reducer: fun action state =>
    switch action {
      | Click => ReasonReact.Update { ...state, timer: state.timer + 1}
    },
  render: fun _self =>
    <div>
      <Board board=(!game).board/>
    </div>
};

