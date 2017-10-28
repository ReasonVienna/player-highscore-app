type routes =
  | UsersRoute
  | UserRoute(int)
  | AddUserRoute;

type handlers = {
  users: unit => unit,
  user: string => unit,
  addUser: unit => unit
};

let initRouter = (handlers) => {
  let router =
    DirectorRe.makeRouter({"/": "users", "/user/:user_id": "user", "/addUser": "addUser"});
  let handlers = {"users": handlers.users, "user": handlers.user, "addUser": handlers.addUser};
  DirectorRe.configure(router, {"html5history": false, "resource": handlers});
  DirectorRe.init(router, "/");
  router
};
