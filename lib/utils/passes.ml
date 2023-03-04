open Base

module type STATE = sig
  type t
end

module Pass (S : STATE) : sig
  type 'r t

  (* Run *)

  val run_pass :
    Proj.cu ->
    ('a, Errs.err list) Result.t t ->
    init:S.t ->
    ('a, Errs.err list) Result.t

  (* User state combinators *)

  val access : S.t t
  val put : S.t -> unit t
  val return : 'r -> 'r t
  val ( let* ) : 'r t -> ('r -> 'rr t) -> 'rr t

  (* Compilation state combinators *)

  val get_cu : Proj.cu t
  val set_cu : Proj.cu -> unit t
  val add_custom_err : Errs.err -> unit t
  val add_err : title:string -> ?text:string -> Loc.loc -> unit t
  val has_errs : bool t

  (* Final combinators *)

  val return_final : 'r -> ('r, _) Result.t t
  val fail_final : Errs.err -> (_, Errs.err list) Result.t t

  (* High-level combinators *)

  val ignore : 'a t -> unit t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val many : 'a list -> f:('a -> 'b t) -> 'b list t

  val many_err :
    'a list -> f:('a -> (_, 'c) Result.t t) -> (unit, 'c) Result.t t

  (* Rename to many_unit??? *)
  val fold_state : 'a list -> f:('a -> unit t) -> unit t

  (* Simple helpers *)

  val ok : 'ok -> ('ok, _) Result.t t
  val error : 'err -> (_, 'err) Result.t t
end = struct
  type state = { userstate : S.t; cu : Proj.cu; errs : Errs.err list }
  type 'r t = state -> state * 'r

  (* Run *)

  let run_pass cu t ~init =
    let init = { userstate = init; cu; errs = [] } in
    let s, r = t init in
    match (r, s.errs) with
    | Ok r, [] -> Ok r
    | Error errs, _ -> Error errs
    | _, errs -> Error errs

  (* Get and set internal state *)
  let access_state s = (s, s)
  let put_state u _ = (u, ())

  (* User state combinators *)

  let return r s = (s, r)

  let ( let* ) t f s =
    let s, r = t s in
    let f_t = f r in
    f_t s

  let access =
    let* s = access_state in
    return s.userstate

  let put u =
    let* s = access_state in
    put_state { s with userstate = u }

  (* Compilation state combinators *)

  let get_cu =
    let* s = access_state in
    return s.cu

  let set_cu cu =
    let* s = access_state in
    put_state { s with cu }

  let add_custom_err err =
    let* s = access_state in
    put_state { s with errs = err :: s.errs }

  let add_err ~title ?(text = "") loc =
    let open Errs in
    let* cu = get_cu in
    add_custom_err { cu; loc; title; text }

  let has_errs =
    let* { errs; _ } = access_state in
    return (Caml.Bool.not (List.is_empty errs))

  (* Simple helpers *)

  let ok x = return (Ok x)
  let error x = return (Error x)

  (* Final combinators *)

  let return_final x = ok x

  let fail_final err =
    let* _ = add_custom_err err in
    let* s = access_state in
    error s.errs

  (* High-level combinators *)

  let ( *> ) a b =
    let* _ = a in
    b

  let ignore t = t *> return ()

  let ( <* ) a b =
    let* a = a in
    let* _ = b in
    return a

  let many xs ~f =
    let rec helper acc xs =
      let* ys = acc in
      match xs with
      | [] -> return (List.rev ys)
      | h :: tl ->
          let* y = f h in
          helper (return (y :: ys)) tl
    in
    helper (return []) xs

  let rec many_err xs ~f =
    match xs with
    | [] -> return (Ok ())
    | h :: tl -> (
        let* r = f h in
        match r with Ok _ -> many_err tl ~f | Error err -> return (Error err))

  let fold_state xs ~f =
    let rec helper acc = function
      | [] -> acc
      | h :: tl -> helper (acc *> f h) tl
    in
    helper (return ()) xs
end
