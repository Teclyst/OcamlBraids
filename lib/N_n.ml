module E_n = struct
  type t = bool array (* false is 0 and true is 1. *)
  type parameter = int
  let (is_trivial : t -> bool) =
    Array.for_all not (* Check if all coordinates are 0. *)
  let delta n =
    (Array.make n true : t)
  let delta_normal (e_1 : t) (e_2 :t) =
    let d_e_1 = Array.map not e_1 in
    let gcd = Array.map2 (&&) d_e_1 e_2 in
    let e_1 = Array.map2 (<>) e_1 gcd (* <> is xor. *)
    and e_2 = Array.map2 (<>) e_2 gcd in
    ((e_1 : t), (e_2 : t))
  let to_string (v : t) =
    Array.fold_left (^) "" (Array.mapi (fun i b -> if b then ("e" ^ string_of_int i)  else "") v)
end