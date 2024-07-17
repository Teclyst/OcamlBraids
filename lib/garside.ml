(* A reasonably minimal description of a Garside structure. *)
module type GarsideStructure = sig

  (* Type representing canonical factors. *)
  type factor
  
  (* Equality test. *)
  val compare: factor -> factor -> bool

  (* Garside element Delta. *)
  val delta: factor

  (* Length (as a word in the atoms) of delta. *)
  val delta_length: int

  (* Neutral element *)
  val e: factor
  
  (* Atoms. *)
  val atoms: factor list

  (* Lateral GCD's *)
  val left_gcd: factor -> factor -> factor
  val right_gcd: factor -> factor -> factor

  (* Lateral complements. *)
  val left_complement: factor -> factor
  val right_complement: factor -> factor

  (* Computes the product of two factors, under the assumption that it is below Delta. *)
  val product: factor -> factor -> factor
  
  (* Conversion to and from string. *)
  val of_string: string -> factor 
  val to_string: factor -> string

  (* Random factor. *)
  val random: unit -> factor

end

(* A more complete expressive description of what a canonical factor is. *)
module type CanonicalFactor = sig

  (* Type representing canonical factors. *)
  type factor

  (* Equality test. *)
  val compare: factor -> factor -> bool
  val left_div: factor -> factor -> bool
  val right_div: factor -> factor -> bool

  val is_trivial: factor -> bool
  val is_delta: factor -> bool

  (* Garside element Delta. *)
  val delta: factor

  (* Atoms. *)
  val atoms: factor list

  (* Neutral element *)
  val e: factor
  
  (* Lateral GCDs. *)
  val left_gcd: factor -> factor -> factor
  val right_gcd: factor -> factor -> factor

  (* Lateral LCMs. *)
  val left_lcm: factor -> factor -> factor
  val right_lcm: factor -> factor -> factor

  (* Lateral complements. *)
  val left_complement: factor -> factor
  val right_complement: factor -> factor

  val delta_conjugate: factor -> factor

  (* Conjugates by a power a delta. *)
  (* delta_conjugate is equivalent to the partial application delta_conjugate_power 1. *)
  val delta_conjugate_power: int -> factor -> factor

  (* Computes the product of two factors, under the assumption that it is below Delta. *)
  val product: factor -> factor -> factor

  (* Computes the left weighted decomposition of a product of two factors. *)
  val left_weighted: factor -> factor -> (factor * factor)

  val right_weighted: factor -> factor -> (factor * factor)
  
  (* Conversion to and from string. *)
  val of_string: string -> factor 
  val to_string: factor -> string

  (* Random factor. *)
  val random: unit -> factor

end

module CanonicalFactor(GarsideStructure: GarsideStructure): CanonicalFactor = struct
  
  type factor =
    GarsideStructure.factor
  
  let compare =
    GarsideStructure.compare
  
  let e =
    GarsideStructure.e
  
  let delta =
    GarsideStructure.delta
  
  let atoms =
    GarsideStructure.atoms
  
  let is_trivial =
    compare e
  
  let is_delta =
    compare delta

  let left_gcd =
    GarsideStructure.left_gcd
  
  let right_gcd =
    GarsideStructure.right_gcd
  
  let left_complement =
    GarsideStructure.left_complement
  
  let right_complement =
    GarsideStructure.right_complement
  
  let left_lcm fact_1 fact_2 =
    left_complement (right_gcd (right_complement fact_1) (right_complement fact_2))
  
  let right_lcm fact_1 fact_2 =
    right_complement (left_gcd (left_complement fact_1) (left_complement fact_2))

  let left_div fact_1 fact_2 =
    compare (left_gcd fact_1 fact_2) fact_1
  
  let right_div fact_1 fact_2 =
    compare (right_gcd fact_1 fact_2) fact_1

  let delta_conjugate fact =
    left_complement (left_complement fact)
  
  let rec delta_conjugate_power k fact =
    match k with
      | 0 ->
        fact
      | _ ->
        delta_conjugate (delta_conjugate_power (k - 1) fact)
  
  let product =
    GarsideStructure.product
  
  let left_weighted fact_1 fact_2 =
    let t = left_gcd (right_complement fact_1) fact_2 in
    product fact_1 t, left_gcd (right_complement t) fact_2
  
  let right_weighted fact_1 fact_2 =
    let t = right_gcd fact_1 (left_complement fact_2) in
    right_gcd fact_1 (left_complement t), product t fact_2
  
  let of_string =
    GarsideStructure.of_string
  
  let to_string =
    GarsideStructure.to_string
  
  let random =
    GarsideStructure.random

end