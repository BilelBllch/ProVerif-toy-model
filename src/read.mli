(*----------------------------------------------------------------------------*)
(** Module for reading files and user inputs, and printing errors *)

open Syntax

(** Prints an error message, optionally with a position in the lexbuf *)
val error : ?lexbuf:Lexing.lexbuf -> string -> unit

(** Prints an error message, optionally with a position in the lexbuf;
    and optionally an exception; and exits with error 1 *)
val error_exit : ?e:exn -> ?lexbuf:Lexing.lexbuf -> string -> 'a

(** Reads rules from a file at a given path *)
val read_rules : string -> rule list

(** Returns the file name given as a command-line argument *)
val read_arg : unit -> string

(** Exception raised when the query is not well-formed *)
exception Next_query

(** Reads a query from stdin. Raises Next_query if parsing the input fails,
    or if the atom given by the user is not ground. *)
val read_query : unit -> atom
