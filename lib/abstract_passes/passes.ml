module type STATE = sig
  type t
end

module Pass (S : STATE) : sig
  type 'r t

  (* Run *)

  val run_pass : 'a t -> init:S.t -> S.t * Errs.err list * 'a

  (* Combinators *)

  val access : S.t t
  val put : S.t -> unit t
  val return : 'r -> 'r t
  val bind : 'r t -> ('r -> 'rr t) -> 'rr t
  val ( let* ) : 'r t -> ('r -> 'rr t) -> 'rr t

  (* Error combinators *)

  val add_err : Errs.err -> unit t
  val has_errs : bool t

  (* Simple helpers *)

  val ok : 'ok -> ('ok, _) result t
  val error : 'err -> (_, 'err) result t

  (* High-level combinators *)

  val ignore : 'a t -> unit t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val many : 'a list -> f:('a -> 'b t) -> 'b list t
  val many_err : 'a list -> f:('a -> (_, 'c) result t) -> (unit, 'c) result t
  val fold_state : 'a list -> f:('a -> unit t) -> unit t
end = struct
  type state = { userstate : S.t; errs : Errs.err list }
  type 'r t = state -> state * 'r

  (* Run *)

  let run_pass t ~init =
    let init = { userstate = init; errs = [] } in
    let state, r = t init in
    (state.userstate, state.errs, r)

  (* Combinators *)

  let access s = (s, s.userstate)
  let put u s = ({ s with userstate = u }, ())
  let return r s = (s, r)

  let bind t f s =
    let s, r = t s in
    let f_t = f r in
    f_t s

  let ( let* ) = bind

  (* Error combinators *)

  let add_err err s =
    let s = { s with errs = err :: s.errs } in
    (s, ())

  let has_errs s =
    let { errs; _ } = s in
    match errs with [] -> (s, false) | _ -> (s, true)

  (* Simple helpers *)

  let ok x = return (Ok x)
  let error x = return (Error x)

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
