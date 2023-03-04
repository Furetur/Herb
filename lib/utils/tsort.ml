open Base

type mark = Unmarked | Visiting | Scheduled

let get_mark marks v =
  Option.value_or_thunk (Map.find marks v) ~default:(fun _ -> Unmarked)

let update_cycle v cycle =
  let ( = ) = Caml.( = ) in
  match cycle with
  | first :: rest -> (
      match List.last rest with
      | Some last when first = last -> cycle
      | _ -> v :: cycle)
  | _ -> v :: cycle

let rec dfs (marks, schedule) neigh v =
  match get_mark marks v with
  | Scheduled -> Ok (marks, schedule)
  | Visiting -> Error [ v ]
  | Unmarked -> (
      let marks = Map.set marks ~key:v ~data:Visiting in
      match dfs_many (marks, schedule) neigh (neigh v) with
      | Ok (marks, schedule) ->
          let marks = Map.set marks ~key:v ~data:Scheduled in
          let schedule = v :: schedule in
          Ok (marks, schedule)
      | Error cycle -> Error (update_cycle v cycle))

and dfs_many (marks, schedule) neigh vs =
  List.fold_result vs ~init:(marks, schedule) ~f:(fun s v -> dfs s neigh v)

let tsort (m : ('a, 'b) Comparator.Module.t) (vs : 'a list)
    (neigh : 'a -> 'a list) : ('a list, [> `Cycle of 'a list ]) Result.t =
  let marks = Map.empty m in
  let schedule = [] in
  match
    List.fold_result vs ~init:(marks, schedule) ~f:(fun (marks, schedule) v ->
        dfs (marks, schedule) neigh v)
  with
  | Ok (_, schedule) -> Ok (List.rev schedule)
  | Error cycle -> Error (`Cycle cycle)
