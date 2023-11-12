open Base

module type STATE = sig
  type t
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
          aux xs (acc @ [b])
    in
    aux xs []
end
