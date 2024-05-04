open OcamlBraids.Garside
open OcamlBraids.N_n

let () = Random.self_init ()

module N_3 = GarsideMonoid(Cube(struct let arg = 3 end)) (* Implements a garside structure for N^3. *)

let element = [[|true; false; false|]; [|true; false; false|]; [|true; false; false|]; [|false; true; false|]; [|false; true; false|]; [|false; false; true|]]

let delta n = (* Prints the Garside element of N^n. *)
  let module Cube_n = Cube(struct let arg = n end) in (* Here we use first class modules (Cube's argument is determined at run time). *)
  print_endline (Cube_n.to_string Cube_n.delta)

let () = print_endline (N_3.to_string (N_3.left_normal_form element))

let () = delta (Random.int 10) (* delta's argument isn't known at compile time! *)