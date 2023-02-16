open Base
open Proj

type mark = Unmarked | Visiting | Scheduled

type tsort_state = {
  marks : (cu, mark, Cu_comparator.comparator_witness) Map.t;
  schedule : cu list;
  get_imports : cu -> cu list;
}

module P = Passes.Pass (struct
  type t = tsort_state
end)

open P

(* Accessors *)

let get_mark cu =
  let* { marks; _ } = access in
  return
    (Option.value_or_thunk (Map.find marks cu) ~default:(fun _ -> Unmarked))

let set_mark cu mark =
  let* s = access in
  let marks = Map.set s.marks ~key:cu ~data:mark in
  put { s with marks }

let add_to_schedule cu =
  let* s = access in
  put { s with schedule = cu :: s.schedule }

let get_imports cu =
  let* { get_imports; _ } = access in
  return (get_imports cu)

(* --- Sort --- *)

let update_cycle cu cycle =
  let ( = ) = Proj.cu_eq in
  match cycle with
  | first :: rest -> (
      match List.last rest with
      | Some last when first = last -> cycle
      | _ -> cu :: cycle)
  | _ -> cu :: cycle

let rec visit cu =
  let* mark = get_mark cu in
  match mark with
  | Scheduled -> ok ()
  | Visiting -> error [ cu ]
  | Unmarked -> (
      let* _ = set_mark cu Visiting in
      let* imports = get_imports cu in
      let* res = many_err imports ~f:visit in
      match res with
      | Ok () ->
          let* _ = set_mark cu Scheduled in
          let* _ = add_to_schedule cu in
          ok ()
      | Error cycle -> error (update_cycle cu cycle))

let tsort get_imports cu =
  let s =
    { get_imports; schedule = []; marks = Map.empty (module Cu_comparator) }
  in
  let s, _, res = run_pass (visit cu) ~init:s in
  match res with
  | Ok () -> Ok (List.rev s.schedule)
  | Error cycle -> Error cycle
