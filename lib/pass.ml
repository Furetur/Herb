open Base

type 'a result = ('a, Errs.err list) Result.t

module type STATE = sig
  type t
end

module Pass (S : STATE) = struct
  type 'a t = S.t -> S.t * ('a, Errs.err list) Result.t

  let run_pass (t : 'a t) ~(init : S.t) : 'a result =
    let _, r = t init in
    r

  (* ----- Base ----- *)

  (* - Constructors - *)

  let return (x : 'r) : 'r t = fun s -> (s, Ok x)
  let fail (err : Errs.err) : _ t = fun s -> (s, Error [ err ])
  let fails (errs : Errs.err list) : _ t = fun s -> (s, Error errs)

  (* - State -  *)

  let get : S.t t = fun s -> (s, Ok s)
  let set (s : S.t) : unit t = fun _ -> (s, Ok ())

  (* - Combinators - *)

  let ( let* ) (t : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s ->
    let s, a = t s in
    match a with Ok a -> f a s | Error errs -> (s, Error errs)

  let ( <*> ) (ft : ('a -> 'b) t) (xt : 'a t) : 'b t =
   fun s ->
    let combine x y =
      Result.combine x y ~ok:(fun x y -> (x, y)) ~err:List.append
    in
    let s, f = ft s in
    let s, x = xt s in
    match combine f x with
    | Ok (f, x) -> (s, Ok (f x))
    | Error errs -> (s, Error errs)

  let ( <$> ) (f : 'a -> 'b) (xt : 'a t) : 'b t = return f <*> xt

  (* ----- Helpers ----- *)

  let ( and* ) (at : 'a t) (bt : 'b t) : ('a * 'b) t =
    let pair x y = (x, y) in
    pair <$> at <*> bt

  let ( *> ) x y =
    let* _ = x in
    y

  let ( <* ) x y =
    let* x = x in
    let* _ = y in
    return x

  let return_result : 'a result -> 'a t = function
    | Ok x -> return x
    | Error errs -> fails errs

  let many (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
    let pushback (bs : 'b list t) (b : 'b t) : 'b list t =
      Util.pushback <$> bs <*> b
    in
    let rec aux (xs : 'a list) (acc : 'b list t) : 'b list t =
      match xs with
      | [] -> acc
      | x :: xs ->
          let b_t = f x in
          aux xs (pushback acc b_t)
    in
    aux xs (return [])

  let many_seq (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
    let rec aux xs acc =
      match xs with
      | [] -> return acc
      | a :: xs ->
          let* b = f a in
          aux xs (Util.pushback acc b)
    in
    aux xs []
end

module NoErrors (S : STATE) = struct
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
          aux xs (Util.pushback acc b)
    in
    aux xs []
end
