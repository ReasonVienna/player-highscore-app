/* let initRouter reduce => {
     let router = DirectorRe.makeRouter {"/": "users", "/user/:user_id": "user"};
     let handlers = {
       "users": fun () => reduce (fun _ => ShowUsersRoute),
       "user": fun (user_id: string) =>
         reduce (fun _ => ShowUserRoute (int_of_string user_id))
     };
     DirectorRe.configure router {"html5history": false, "resource": handlers};
     DirectorRe.init router "/";
     router
   }; */
