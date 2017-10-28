open Styles;

let se = ReasonReact.stringToElement;

type userId = int;

type user = {
  id: userId,
  name: string,
  points: int
};

let getValue = (event) => (
                            event
                            |> ReactEventRe.Form.target
                            |> ReactDOMRe.domElementToObj
                          )##value;

type state = option((string, int));

type action =
  | Update((string, int))
  | UpdateName(string)
  | UpdatePoints(int)
  | Save
  | ToggleEdit;

let edit = ((name, points), _event) => Update((name, points));

let component = ReasonReact.reducerComponent("User");

let make = (~points, ~onSave, ~onDelete, ~name, ~addMode=false, _children) => {
  ...component,
  initialState: () => addMode ? Some((name, points)) : None,
  reducer: (action, state) =>
    switch action {
    | Update((name, points)) => ReasonReact.Update(Some((name, points)))
    | UpdateName(name) =>
      switch state {
      | Some((_name, points)) => ReasonReact.Update(Some((name, points)))
      | None => ReasonReact.NoUpdate
      }
    | UpdatePoints(points) =>
      switch state {
      | Some((name, _points)) => ReasonReact.Update(Some((name, points)))
      | None => ReasonReact.NoUpdate
      }
    | Save =>
      switch state {
      | Some((name, points)) =>
        ReasonReact.UpdateWithSideEffects(
          None,
          (
            (self) => {
              onSave((name, points));
              self.reduce(() => ToggleEdit, ())
            }
          )
        )
      | None => ReasonReact.NoUpdate
      }
    | ToggleEdit =>
      switch state {
      | Some(_) => ReasonReact.Update(None)
      | None => ReasonReact.Update(Some((name, points)))
      }
    },
  render: ({state, reduce}) =>
    switch state {
    | Some((name, points)) =>
      <div style=rowStyle>
        <div style=cellStyle>
          <input
            value=name
            onChange=(reduce((event) => UpdateName(getValue(event))))
          />
        </div>
        <div style=cellStyle>
          <input
            value=(string_of_int(points))
            onChange=(
              reduce(
                (event) => {
                  let newValue = getValue(event);
                  if (newValue === "") {
                    UpdatePoints(0)
                  } else {
                    let newPoints =
                      try (int_of_string(newValue)) {
                      | Failure("int_of_string") => points
                      };
                    UpdatePoints(newPoints)
                  }
                }
              )
            )
          />
        </div>
        <div style=cellStyle>
          <button onClick=(reduce((_event) => Save))> (se("Save")) </button>
          (
            if (addMode) {
              <span> (se("")) </span>
            } else {
              <button onClick=(reduce((_event) => ToggleEdit))>
                (se("Cancel"))
              </button>
            }
          )
        </div>
      </div>
    | None =>
      <div style=rowStyle>
        <div style=cellStyle> (se(name)) </div>
        <div style=cellStyle> (se(string_of_int(points))) </div>
        <div style=cellStyle>
          <button onClick=(reduce(edit((name, points))))>
            (se("Edit"))
          </button>
          <button onClick=((_event) => onDelete())> (se("Remove")) </button>
        </div>
      </div>
    }
};
