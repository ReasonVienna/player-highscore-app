open Router;

open Styles;

let se = ReasonReact.stringToElement;

type sort =
  | Asc
  | Desc;

type action =
  /* route actions */
  | ShowUsersRoute
  | ShowUserRoute(int)
  | ShowAddUserRoute
  /* other actions */
  | Sort(sort)
  | AddUser(User.user)
  | UpdateUsers(list(User.user))
  | UpdateUser(User.userId, (string, int))
  | RemoveUser(User.userId);

type state = {
  users: list(User.user),
  sort,
  route: routes
};

let getNextId = (users) =>
  List.length(users) === 0 ?
    0 :
    List.fold_left(
      (maxId, user: User.user) => user.id > maxId ? user.id : maxId,
      0,
      users
    )
    + 1;

/* Event functions */
let update = (id, (name, points)) => UpdateUser(id, (name, points));

let delete = (id, _event) => RemoveUser(id);

let sortUsers = ({users, sort}) =>
  switch sort {
  | Asc =>
    List.sort((a: User.user, b: User.user) => a.points - b.points, users)
  | Desc =>
    List.sort((a: User.user, b: User.user) => b.points - a.points, users)
  };

let component = ReasonReact.reducerComponent("Main");

let router = ref(None);

let make = (_children) => {
  ...component,
  initialState: () => {users: [], sort: Asc, route: UsersRoute},
  didMount: (self) => {
    /* initial fetch */
    UserDataRepo.fetch()
    |> Js.Promise.then_(
         (users) => {
           self.reduce(() => UpdateUsers(Array.to_list(users)), ());
           Js.Promise.resolve()
         }
       )
    |> ignore;
    /* define router handlers */
    let handlers = {
      users: () => self.reduce((_) => ShowUsersRoute, ()),
      user: (user_id: string) =>
        self.reduce((_) => ShowUserRoute(int_of_string(user_id)), ()),
      addUser: () => self.reduce((_) => ShowAddUserRoute, ())
    };
    router := Some(initRouter(handlers));
    ReasonReact.NoUpdate
  },
  reducer: (action, state) =>
    switch action {
    /* route specific actions */
    | ShowUsersRoute => ReasonReact.Update({...state, route: UsersRoute})
    | ShowUserRoute(id) => ReasonReact.Update({...state, route: UserRoute(id)})
    | ShowAddUserRoute => ReasonReact.Update({...state, route: AddUserRoute})
    /* User specific actions */
    | AddUser(user) =>
      ReasonReact.Update({
        ...state,
        users: List.append(state.users, [user]),
        route: UsersRoute
      })
    | UpdateUsers(users) => ReasonReact.Update({...state, users})
    | UpdateUser(id, (name, points)) =>
      let users =
        List.map(
          (user: User.user) => user.id === id ? {...user, name, points} : user,
          state.users
        );
      ReasonReact.Update({...state, users})
    | RemoveUser((id: User.userId)) =>
      UserDataRepo.delete(id)
      |> Js.Promise.then_((user) => Js.Promise.resolve(user))
      |> (
        (_user) => {
          let users =
            List.filter((user: User.user) => user.id !== id, state.users);
          ReasonReact.Update({...state, users})
        }
      )
    | Sort(sort) => ReasonReact.Update({...state, sort})
    },
  render: ({state, reduce}) => {
    let listed = sortUsers(state);
    <div>
      (
        switch state.route {
        | UsersRoute =>
          <div>
            <h1> (se("User High Score!")) </h1>
            (
              switch state.sort {
              | Asc =>
                <div>
                  (se("Sorted by Points Asc"))
                  <button onClick=(reduce((_evt) => Sort(Desc)))>
                    (se("Sort Points Desc"))
                  </button>
                </div>
              | Desc =>
                <div>
                  (se("Sorted by Points Desc"))
                  <button onClick=(reduce((_evt) => Sort(Asc)))>
                    (se("Sort Points Asc"))
                  </button>
                </div>
              }
            )
            <hr />
            <UserList
              onSave=((id) => reduce(update(id)))
              onDelete=((id) => reduce(delete(id)))
              users=listed
            />
            <a href="#/addUser"> (se("Add User")) </a>
          </div>
        | UserRoute(id) =>
          try {
            let {name, points}: User.user =
              List.find((user: User.user) => user.id === id, state.users);
            <User
              name
              points
              onSave=(reduce(update(id)))
              onDelete=(reduce(delete(id)))
            />
          } {
          | Not_found => <div> (se("No User found.")) </div>
          }
        | AddUserRoute =>
          <div style=rowStyle>
            <h2> (se("Add User")) </h2>
            <User
              name=""
              points=0
              onDelete=((_event) => ())
              addMode=true
              onSave=(
                ((name, points)) =>
                  UserDataRepo.save((name, points))
                  |> Js.Promise.then_(
                       (user) => {
                         reduce(() => AddUser(user), ());
                         switch router^ {
                         | Some(router) => DirectorRe.setRoute(router, "/")
                         | None => ()
                         };
                         Js.Promise.resolve()
                       }
                     )
                  |> ignore
              )
            />
            <a href="#"> (se("Back to User List")) </a>
          </div>
        }
      )
    </div>
  }
};
