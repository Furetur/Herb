open Base
open Proj

type mark = Unmarked | Visiting | Scheduled

type tsort_state = {
  marks : (cu, mark, Cu_comparator.comparator_witness) Map.t;
  schedule : cu list;
  cycle : cu list;
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

let add_to_cycle cu =
  let ( = ) = Proj.equal_cu in
  let* s = access in
  let not_add = return () in
  let add = put { s with cycle = cu :: s.cycle } in
  match s.cycle with
  | first :: rest -> (
      match List.last rest with
      | Some last when first = last -> not_add
      | _ -> add)
  | _ -> add

(* --- Pass --- *)

let rec visit cu =
  let* mark = get_mark cu in
  match mark with
  | Scheduled -> ok ()
  | Visiting -> add_to_cycle cu *> error ()
  | Unmarked -> (
      let* _ = set_mark cu Visiting in
      let* imports = get_imports cu in
      let* res = many_err imports ~f:visit in
      match res with
      | Ok () ->
          let* _ = set_mark cu Scheduled in
          let* _ = add_to_schedule cu in
          ok ()
      | Error () -> add_to_cycle cu *> error ())

(* --- Result --- *)

type cu_schedule = cu list * cu

let into_cu_schedule list entry : cu_schedule =
  let ( = ) = Proj.equal_cu in
  let libs = List.filter list ~f:(fun cu -> Caml.Bool.not (cu = entry)) in
  (libs, entry)

let tsort get_imports cu =
  let s =
    {
      get_imports;
      schedule = [];
      cycle = [];
      marks = Map.empty (module Cu_comparator);
    }
  in
  let s, _, res = run_pass cu (visit cu) ~init:s in
  match res with
  | Ok () -> Ok (into_cu_schedule (List.rev s.schedule) cu)
  | Error () ->
      Logs.info (fun m -> m "Dependency cycle detected");
      let text =
        String.concat ~sep:"->\n  "
          (List.map s.cycle ~f:(fun cu -> Fpath.to_string (Proj.cu_path cu)))
      in
      Error
        (`Errs
          [ Errs.err ~title:"Dependency cycle detected" ~text cu Loc.start_loc ])
