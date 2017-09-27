open Styles;

let se = ReasonReact.stringToElement;

type sort =
  | Asc
  | Desc;

type action =
  | ShowUsersRoute
  | ShowUserRoute int
  | Sort sort
  | AddUser User.user
  | UpdateUsers (list User.user)
  | UpdateUser User.userId (string, int)
  | RemoveUser User.userId;

type routes =
  | UsersRoute
  | UserRoute int;

type state = {
  users: list User.user,
  sort,
  route: routes
};

let getNextId users =>
  List.length users === 0 ?
    0 :
    List.fold_left
      (fun maxId (user: User.user) => user.id > maxId ? user.id : maxId)
      0
      users + 1;

/* Event functions */
let update id (name, points) => UpdateUser id (name, points);

let delete id _event => RemoveUser id;

let sortUsers {users, sort} =>
  switch sort {
  | Asc =>
    List.sort (fun (a: User.user) (b: User.user) => a.points - b.points) users
  | Desc =>
    List.sort (fun (a: User.user) (b: User.user) => b.points - a.points) users
  };

let component = ReasonReact.reducerComponent "Main";

let initRouter reduce => {
  let router = DirectorRe.makeRouter {"/": "users", "/user/:user_id": "user"};
  let handlers = {
    "users": fun () => reduce (fun _ => ShowUsersRoute),
    "user": fun (user_id: string) =>
      reduce (fun _ => ShowUserRoute (int_of_string user_id))
  };
  DirectorRe.configure router {"html5history": false, "resource": handlers};
  DirectorRe.init router "/";
  router
};

let make _children => {
  ...component,
  initialState: fun () => {users: [], sort: Asc, route: UsersRoute},
  didMount: fun self => {
    /* initial fetch */
    UserDataRepo.fetch () |>
    Js.Promise.then_ (
      fun users => {
        self.reduce (fun () => UpdateUsers (Array.to_list users)) ();
        Js.Promise.resolve ()
      }
    ) |> ignore;
    let router = initRouter self.reduce;
    ReasonReact.NoUpdate
  },
  reducer: fun action state =>
    switch action {
    /* route specific actions */
    | ShowUsersRoute => ReasonReact.Update {...state, route: UsersRoute}
    | ShowUserRoute id => ReasonReact.Update {...state, route: UserRoute id}
    /* User specific actions */
    | AddUser user =>
      ReasonReact.Update {...state, users: List.append state.users [user]}
    | UpdateUsers users => ReasonReact.Update {...state, users}
    | UpdateUser id (name, points) =>
      let users =
        List.map
          (
            fun (user: User.user) =>
              user.id === id ? {...user, name, points} : user
          )
          state.users;
      ReasonReact.Update {...state, users}
    | RemoveUser (id: User.userId) =>
      UserDataRepo.delete id |>
      Js.Promise.then_ (fun user => Js.Promise.resolve user) |> (
        fun _user => {
          let users =
            List.filter (fun (user: User.user) => user.id !== id) state.users;
          ReasonReact.Update {...state, users}
        }
      )
    | Sort sort => ReasonReact.Update {...state, sort}
    },
  render: fun {state, reduce} => {
    let listed = sortUsers state;
    <div>
      <h1> (se "User High Score!") </h1>
      (
        switch state.sort {
        | Asc =>
          <div>
            (se "Sorted by Points Asc")
            <button onClick=(reduce (fun _evt => Sort Desc))>
              (se "Sort Points Desc")
            </button>
          </div>
        | Desc =>
          <div>
            (se "Sorted by Points Desc")
            <button onClick=(reduce (fun _evt => Sort Asc))>
              (se "Sort Points Asc")
            </button>
          </div>
        }
      )
      <hr />
      (
        switch state.route {
        | UsersRoute =>
          <UserList
            onSave=(fun id => reduce (update id))
            onDelete=(fun id => reduce (delete id))
            users=listed
          />
        | UserRoute id =>
          try {
            let {name, points}: User.user =
              List.find (fun (user: User.user) => user.id === id) state.users;
            <User
              name
              points
              onSave=(reduce (update id))
              onDelete=(reduce (delete id))
            />
          } {
          | Not_found => <div> (se "No User found.") </div>
          }
        }
      )
      <div style=rowStyle>
        <User
          name=""
          points=0
          onDelete=(fun _event => ())
          onSave=(
            fun (name, points) =>
              UserDataRepo.save (name, points) |>
              Js.Promise.then_ (
                fun user => {
                  reduce (fun () => AddUser user) ();
                  Js.Promise.resolve ()
                }
              ) |> ignore
          )
        />
      </div>
    </div>
  }
};
