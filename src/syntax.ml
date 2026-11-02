(*----------------------------------------------------------------------------*)
(** Syntax definitions: variables, substitutions, terms, atoms, rules *)


(** Exception raised by the lexer *)
exception Lexing_error

(** Pretty-printers for list separators *)
let pp_comma chan () : unit = Format.pp_print_string chan ", "
let pp_and chan () : unit = Format.pp_print_string chan " /\\ "


(*----------------------------------------------------------------------------*)
(** Module for variable names *)
module Var = struct

  (** Type of variable names *)
  type t = string

  (** Creates a variable name from a string.
      The string must be in [A-Z][A-Za-z0-9_']* (the lexer conforms to this) *)
  let mk_var (s:string) : t =
    assert (String.for_all ((<>) '#') s);
    s

  (** Returns a fresh variable name, never used before,
      based on the one given *)
  let refresh : t -> t =
    let i = ref 0 in
    fun (v:t) ->
      let varname = List.hd (String.split_on_char '#' v) in
      let v' = varname ^ "#" ^ (string_of_int !i) in
      incr i;
      v'

  (** Comparison function (needed later to build maps) *)
  let compare = compare
end


(** Convenient alias for the variable type*)
type var = Var.t



(*----------------------------------------------------------------------------*)
(** Substitutions map variables to values of any type
    (in practice we use them with terms) *)
module Subst = struct

  (** Mapping variables *)
  module VM = Map.Make(Var)

  (** Type for substitutions *)
  type 'a t = 'a VM.t

  (** Empty substitution *)
  let empty = VM.empty

  (** Get the value associated to a variable name, if it exists.
      Raises Not_found if not. *)
  let get (theta:'a t) (v:var) : 'a =
    VM.find v theta

  (** Updates the binding for a variable in a substitution *)
  let update (theta:'a t) (v:var) (t:'a) : 'a t =
    VM.add v t theta

  (** Maps a function to each binding of a substitution:
      map f theta returns a substitution theta' such that for all x,
      theta'(x) = f(theta(x)) *)
  let map (f:'a -> 'b) (theta:'a t) : 'b t =
    VM.map f theta
      
  (** Pretty-printer for substitutions *)
  let pp ppt chan (theta:'a t) : unit =
    let pps = Format.pp_print_string chan in
    let binds = VM.bindings theta in
    pps "{";
    Format.pp_print_list ~pp_sep:pp_comma
      (fun chan (v, a) ->
         let pps = Format.pp_print_string chan in
         pps v;
         pps " --> ";
         ppt chan a;
      )
      chan
      binds;
    pps "}"
    
      
end



(*--------------------------------------------------------------*)
(** Terms are constructed inductively by applying functions to other terms,
    e.g. f(x, y, g(z)), where x, y, z are variables and f a function symbol *)
module Term = struct 

  (** Type for function symbols *)
  type fsym = string

  (** Type of a term *)
  type t =
      Var of var (* variable name *)
    | Fun of fsym * t list (* function name and arguments *)

  (** Creates a function symbol from a string *)
  let mk_fsym (s:string) : fsym =
    s

  (** Function symbols for zero and successor *) 
  let fzero : fsym = mk_fsym "z"
  let fsucc : fsym = mk_fsym "s"

  (** Creates a term representing an integer in unary.
      (could be convenient when writing examples, otherwise unimportant) *) 
  let term_of_int (n:int) : t =
  let rec aux (acc:t) (n:int) : t =
    if n = 0 then acc
    else aux (Fun (fsucc,[acc])) (n - 1)
  in
  aux (Fun (fzero, [])) n

  (** Checks whether a term is ground (i.e. contains no variables) *)
  let rec is_ground (t:t) : bool =
    match t with
    | Var _ -> false
    | Fun (_, args) -> List.for_all is_ground args

  (** Maps a function [f] to each variable in a term [t], 
    i.e. replaces each variable [Var x] in [t] with [f x]. *)
let rec map (f:var -> t) (t:t) = 
  match t with 
  | Var x -> f x
  | Fun (c, args) -> Fun (c, List.map (map f) args)


(** Returns the term bound to a variable in a term substitution,
    or the variable itself if no term is bound to it. *)
let get (theta: t Subst.t) (v:var) : t =
  try
    Subst.get theta v
  with
  | Not_found -> Var v  

(** Applies a substitution to a term *)
let apply (theta:t Subst.t) (t:t) : t =
  map (get theta) t

(** Pretty-printer for terms *)
let rec pp chan (t:t) : unit =
  let pps = Format.pp_print_string chan in
  match t with 
  | Var x -> pps x;
  | Fun (c, args) -> 
    pps c;
    if args <> [] then
      (pps "(";
       pp_terms chan args;
       pps ")")

and pp_terms chan (ts:t list) : unit = 
  Format.pp_print_list ~pp_sep:pp_comma pp chan ts

end 


(** Convenient alias for the term type *)
type term = Term.t


(** Convenient alias for the substitution type *)
type subst = term Subst.t


(*--------------------------------------------------------------*)
(** Atoms are unary predicates applied to terms, e.g. P(f(x,y)) *)
module Atom = struct 

  (** Type for predicate symbols *)
  type psym = string

  (** Type of an atom *)
  type t = {p: psym; arg: term}

  (** Creates a predicate symbol from a string.
      The string must be in [a-z][A-Za-z0-9_']* (the lexer conforms to this) *)
  let mk_psym (s:string) : psym =
    assert (String.for_all ((<>) '#') s);
    s

  (** Creates a variant of a predicate symbol,
      guaranteed to have never been seen before *)
  let variant_psym : psym -> psym =
    let i = ref 0 in
    fun (p:psym) ->
      let pname = List.hd (String.split_on_char '#' p) in
      let p' = pname ^ "#" ^ (string_of_int !i) in
      incr i;
      p'

  (** Checks whether an atom is ground (i.e. contains no variables) *)
  let is_ground (a:t) : bool =
    Term.is_ground a.arg
      
  (** Maps a function to all variables in an atom *)
  let map (f:var -> term) (a:t) : t =
    { a with arg = Term.map f a.arg }

  (** Applies a substitution to an atom *)
  let apply (theta:subst) (a:t) : t =
    map (Term.get theta) a

  (** Pretty-printer for atoms *)
  let pp chan (a:t) : unit =
    let pps = Format.pp_print_string chan in
    pps a.p;
    pps "(";
    Term.pp chan a.arg;
    pps ")"
end


(** Convenient alias for the atom type *)
type atom = Atom.t



(*--------------------------------------------------------------*)
(** A rule is a definite Horn clause, i.e. a formula of the form 
        A_1 /\ … /\ A_n => A
      where A, A_1, …, A_n are atoms.
      A_1, …, A_n are the hypotheses of the rule, A its conclusion. *)
module Rule = struct 

  (** Type of a rule *)
  type t = {hyps: atom list; concl: atom}

  (** Applies a function to all variables in a rule *) 
  let map (f:var -> term) (r:t) : t =
    { hyps = List.map (Atom.map f) r.hyps;
      concl = Atom.map f r.concl }

  (** Applies a substitution to a rule *)
  let apply (theta:subst) (r:t) : t =
    map (Term.get theta) r

  (** Pretty-printer for rules *)
  let pp chan (r:t) : unit =
    Format.pp_print_list ~pp_sep:pp_and Atom.pp chan r.hyps;
    Format.pp_print_string chan " => ";
    Atom.pp chan r.concl

end


(** Convenient alias for the rule type *)
type rule = Rule.t