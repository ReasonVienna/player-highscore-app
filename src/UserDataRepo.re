let url = "http://localhost:3000/users";

type userData = list(User.user);

type data = {
  name: string,
  points: int
};

let encode = (data) =>
  Json.Encode.(
    object_([("name", string(data.name)), ("points", int(data.points))])
  );

let parseUser = (json) : User.user =>
  Json.Decode.{
    id: field("id", int, json),
    name: field("name", string, json),
    points: field("points", int, json)
  };

let parseResponse = (json) => Json.Decode.array(parseUser, json);

let save = ((name, points)) =>
  Js.Promise.(
    Bs_fetch.fetchWithInit(
      url,
      Bs_fetch.RequestInit.make(
        ~method_=Post,
        ~body=
          Bs_fetch.BodyInit.make(Js.Json.stringify(encode({name, points}))),
        ~headers=
          Bs_fetch.HeadersInit.makeWithArray([|
            ("content-Type", "application/json")
          |]),
        ()
      )
    )
    |> then_(Bs_fetch.Response.text)
    |> then_(
         (jsonText) =>
           Js.Promise.resolve(parseUser(Js.Json.parseExn(jsonText)))
       )
  );

let fetch = () =>
  Bs_fetch.fetch(url)
  |> Js.Promise.then_(Bs_fetch.Response.text)
  |> Js.Promise.then_(
       (jsonText) =>
         Js.Promise.resolve(parseResponse(Js.Json.parseExn(jsonText)))
     );

let delete = (id) =>
  Js.Promise.(
    Bs_fetch.fetchWithInit(
      url ++ ("/" ++ string_of_int(id)),
      Bs_fetch.RequestInit.make(~method_=Delete, ())
    )
    |> then_(Bs_fetch.Response.text)
    |> then_((_) => Js.Promise.resolve(true))
  );
