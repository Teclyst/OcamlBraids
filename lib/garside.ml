module type GarsideStructure = sig
  type t (* Type representing canonical factors.*)
  type parameter (* As Ocaml does not support parameterizing modules with values, we have to work around to gather information for cases where a module may represent several garside structures *)
  (* For example, parameter would be an int for braid groups, representing width. *)
  val is_trivial : t -> bool
  val delta : parameter -> t (* The Garside element may depend on a parameter. *)
  val delta_normal : t -> t -> (t * t) (* Computes the delta normal decomposition of a product of two canonical factors. *)
  val to_string : t -> string
end

module GarsideMonoid(CanonicalFactors: GarsideStructure) = struct
  let to_string w =
    List.fold_left (fun acc a -> acc ^ "(" ^ CanonicalFactors.to_string a ^ ")") "" w
  let rec left_normal_form w =
    let rec one_pass w = (* Does one pass of setting successive factors in left normal form. *)
      match w with
        | e :: [] when CanonicalFactors.is_trivial e ->
          []
        | [] | _ :: [] -> (* If w is of length at most 1, then do nothing. *)
          w
        | e_1 :: e_2 :: t ->
          let (e_1, e_2) = CanonicalFactors.delta_normal e_1 e_2 in
          match (CanonicalFactors.is_trivial e_1, CanonicalFactors.is_trivial e_2) with
            | (true, true) ->
              one_pass t
            | (true, false) ->
              one_pass (e_2 :: t)
            | (false, true) ->
              e_1 :: one_pass t
            | (false, false) ->
              e_1 :: one_pass (e_2 :: t) in
    match w with
      | [] -> (* If w is of length 0, then do nothing. *)
        []
      | e :: t ->
        let t = left_normal_form t in
        one_pass (e :: t)
end