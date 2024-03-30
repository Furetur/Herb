open Base

module Result_dsl_syntax = struct
  let return = Result.return
  let fail = Result.fail
  let ( let* ) = Result.( >>= )
end

module type Pass_state = sig
  type t
end

module No_errors (S : Pass_state) = struct
  type 'a t = S.t -> S.t * 'a

  let run_pass (t : 'a t) ~(init : S.t) : 'a =
    let _, r = t init in
    r

  (* ----- Base ----- *)

  (* - Constructors - *)

  let return (x : 'r) : 'r t = fun s -> (s, x)

  (* - State -  *)

  let get : S.t t = fun s -> (s, s)
  let set (s : S.t) : unit t = fun _ -> (s, ())

  (* - Combinators - *)

  let ( let* ) (t : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s ->
    let s, a = t s in
    f a s

  (* ----- Helpers ----- *)

  let ( *> ) x y =
    let* _ = x in
    y

  let ( <* ) x y =
    let* x = x in
    let* _ = y in
    return x

  let many (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
    let rec aux xs acc =
      match xs with
      | [] -> return acc
      | a :: xs ->
          let* b = f a in
          aux xs (acc @ [ b ])
    in
    aux xs []
end

module Single_error (S : Pass_state) = struct
  type 'a pass = S.t -> (S.t * 'a, Errors.error) Result.t

  let run_pass (t : 'a pass) ~(init : S.t) : ('a, Errors.error) Result.t =
    let result = t init in
    Result.map result ~f:(fun (_, a) -> a)

  (* ----- Base ----- *)

  (* - Constructors - *)

  let return (x : 'a) : 'a pass = fun s -> Ok (s, x)
  let fail (err : Errors.error) : 'a pass = fun _ -> Error err

  (* - State - *)

  let get : S.t pass = fun s -> Ok (s, s)
  let set (new_state : S.t) : unit pass = fun _ -> Ok (new_state, ())

  (* - Combinators - *)

  let ( let* ) (pass : 'a pass) (f : 'a -> 'b pass) : 'b pass =
   fun s ->
    match pass s with
    | Ok (new_state, result) -> f result new_state
    | Error err -> Error err

  (* ----- Helpers ----- *)
  (* TODO: remove this code duplication *)

  let ( *> ) (x : _ pass) (y : 'a pass) : 'a pass =
    let* _ = x in
    y

  let many (xs : 'a list) ~(f : 'a -> 'b pass) : 'b list pass =
    let rec aux xs acc =
      match xs with
      | [] -> return acc
      | a :: xs ->
          let* b = f a in
          aux xs (acc @ [ b ])
    in
    aux xs []

  let many_unit (xs : 'a list) ~(f : 'a -> unit pass) : unit pass =
    many xs ~f *> return ()
end
