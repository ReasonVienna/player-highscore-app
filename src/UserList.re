open Styles;

let se = ReasonReact.stringToElement;

let component = ReasonReact.statelessComponent "UserList";

let make ::onSave ::onDelete ::users _children => {
  ...component,
  render: fun _self =>
    <div style=tableStyle>
      <div style=rowStyle>
        <div style=cellStyle> (se "Name") </div>
        <div style=cellStyle> (se "Points") </div>
        <div style=cellStyle> (se "Actions") </div>
      </div>
      (
        ReasonReact.arrayToElement (
          Array.of_list (
            List.map
              (
                fun (user: User.user) =>
                  <User
                    key=(string_of_int user.id)
                    onSave=(onSave user.id)
                    onDelete=(onDelete user.id)
                    name=user.name
                    points=user.points
                  />
              )
              users
          )
        )
      )
    </div>
};
