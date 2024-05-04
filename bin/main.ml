open OcamlBraids.Garside
open OcamlBraids.N_n

module N_3 = GarsideMonoid(Cube(struct let arg = 3 end)) (* Implements a garside structure for N^3. *)

let element = [[|true; false; false|]; [|true; false; false|]; [|true; false; false|]; [|false; true; false|]; [|false; true; false|]; [|false; false; true|]]

let delta n =
  let module Cube_n = Cube(struct let arg = n end) in (* Here we use first class modules. *)
  print_endline (Cube_n.to_string Cube_n.delta)

let () = print_endline (N_3.to_string (N_3.left_normal_form element))

let () = delta 7