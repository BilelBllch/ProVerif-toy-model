(*----------------------------------------------------------------------------*)
(** Module for reading files and user inputs, and printing errors *)

open Syntax


(** Pretty-printer for lexing errors *)
let pp_lexerror chan (lexbuf:Lexing.lexbuf) : unit =
  let open Lexing in
  Format.fprintf chan "at character %d, line %d"
    (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
    lexbuf.lex_curr_p.pos_lnum


(** Prints an error message, optionally with a position in the lexbuf *)
let error ?(lexbuf:Lexing.lexbuf option) (s:string) : unit =
  Format.eprintf "@[Error: %s" s;
  match lexbuf with
  | Some lexbuf -> Format.eprintf " %a.@.@]" pp_lexerror lexbuf
  | None -> Format.eprintf ".@.@]"


(** Prints an error message, optionally with a position in the lexbuf;
    and optionally an exception; and exits with error 1 *)
let error_exit ?(e:exn option) ?(lexbuf:Lexing.lexbuf option) (s:string) : 'a =
  error ?lexbuf s;
  (match e with
   | None -> ();
   | Some e -> Format.eprintf "@[%s@.@]" (Printexc.to_string e));
  Printexc.print_backtrace stderr;
  exit 1


(** Reads rules from a file at a given path *)
let read_rules (filename:string) : rule list =
  Format.printf "@[Reading rules from %s...@." filename ;
  if not (Sys.file_exists filename) then
    error_exit ("file " ^ filename ^ " does not exist");
  if Sys.is_directory filename then
    error_exit (filename ^ " is a directory, not a file");
  let inchan =
    try
      open_in filename
    with
    | e -> error_exit ~e ("could not open file " ^ filename)
  in
  let lexbuf = Lexing.from_channel inchan in
  try
    let rules = Parser.rules Lexer.tokenize lexbuf in
    Format.printf "Loaded %d rules.@.@]" (List.length rules) ;
    rules
  with
  | Parser.Error ->
    error_exit ~lexbuf "syntax error"
  | Lexing_error ->
    error_exit ~lexbuf "lexing error"


(** Returns the file name given as a command-line argument *)
let read_arg () : string =
  let fail () =
    Format.eprintf "@[Usage: %s <path to rules file>@.@]" Sys.argv.(0);
    exit 1
  in
  if Array.length Sys.argv <> 2 then fail ();
  Sys.argv.(1)
  

(** Exception raised when the query is not well-formed *)
exception Next_query


(** Reads a query from stdin. Raises Next_query if parsing the input fails,
    or if the atom given by the user is not ground. *)
let read_query () : atom =
  let lexbuf = Lexing.from_channel stdin in
  Format.printf "@[@.Query (terminated with '.')? @]" ;
  Format.print_flush () ;
  let (a:atom) =
    try
      Parser.query Lexer.tokenize lexbuf
    with
    | Parser.Error ->
      error ~lexbuf "syntax error";
      Lexing.flush_input lexbuf;
      raise Next_query
    | Syntax.Lexing_error ->
      error ~lexbuf "lexing error";
      Lexing.flush_input lexbuf;
      raise Next_query
  in
  if not (Atom.is_ground a) then
    (error "query must be a ground atom";
     raise Next_query);
  a
