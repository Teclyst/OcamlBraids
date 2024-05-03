open OcamlBraids.Garside
open OcamlBraids.N_n

module N_n = GarsideMonoid(E_n) (* Implements a garside structure for N^n *)

let element = [[|true; false; false|]; [|true; false; false|]; [|true; false; false|]; [|false; true; false|]; [|false; true; false|]; [|false; false; true|]]

let () = print_endline (N_n.to_string (N_n.left_normal_form element))