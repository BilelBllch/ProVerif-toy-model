(*----------------------------------------------------------------------------*)
(** Main file *)

open Syntax
open Read

(* The compiler expects to find a function `pif : unit -> unit`
   in file `main.ml` (this file), which will be the main function called
   when executing the code.

   The idea is for you to write your code in file `saturation.ml`,
   but feel free to do otherwise if you want, for instance
   to split your code into several files.

   Module Syntax contains the definitions of terms, substitutions, atoms,
   rules, and some useful functions.
   Module Read contains the code parsing the rules from the file,
   and handling user input.
   If for whatever reason you decide to modify them, please let me know.   
*)



(** Main function *)
let pif () =
  let filename = read_arg () in
  let (rules:rule list) = read_rules filename in

  let (solved_rules:rule list) = Saturation.hello_rules rules in
  
  while true do
    try
      let (q:atom) = read_query () in
      
      Saturation.hello_query q solved_rules;

    with
    | Next_query -> ()
  done;
  Format.printf "@."

