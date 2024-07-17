module type Dim = sig (* We want to be able to pass an integer parameter to module Cube. This is done by wrapping it in a module and turning Cube into a functor. *)
  val arg : int
end

module Cube(Dim : Dim) = struct (* Implements a Garside structure for the Dim.arg-dimensional cube. *)
  type t = bool array (* false is 0 and true is 1. *)

  let (is_trivial : t -> bool) =
    Array.for_all not (* Check if all coordinates are 0. *)
  let delta = (Array.make Dim.arg true : t)
  let delta_normal (e : t) (f : t) =
    let d_e = Array.map not e in (* e_1's right complement *)
    let gcd = Array.map2 (&&) d_e f in
    let e = Array.map2 (<>) e gcd (* <> is xor. *)
    and f = Array.map2 (<>) f gcd in
    ((e : t), (f : t))
  let from_bool_array a =
    match Array.length a = Dim.arg with
      | true ->
        (a : t)
      | false ->
        failwith "Wrong dimension!" 
  let to_string (v : t) =
    Array.fold_left (^) "" (Array.mapi (fun i b -> if b then ("e" ^ string_of_int i)  else "") v)
end