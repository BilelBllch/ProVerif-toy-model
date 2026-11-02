(*----------------------------------------------------------------------------*)
(** Syntax definitions: variables, substitutions, terms, atoms, rules *)


(** Exception raised by the lexer *)
exception Lexing_error


(*----------------------------------------------------------------------------*)
(** Module for variable names *)
module Var : sig

  (** Type of variable names *)
  type t

  (** Creates a variable name from a string.
      The string must be in [A-Z][A-Za-z0-9_']* (the lexer conforms to this) *)
  val mk_var : string -> t

  (** Returns a fresh variable name, never used before,
      based on the one given *)
  val refresh : t -> t
end

(** Convenient alias for the variable type*)
type var = Var.t



(*----------------------------------------------------------------------------*)
(** Substitutions map variables to values of any type
    (in practice we use them with terms) *)
module Subst : sig

  (** Type for substitutions *)
  type 'a t

  (** Empty substitution *)
  val empty : 'a t

  (** Get the value associated to a variable name, if it exists.
      Raises Not_found if not. *)
  val get : 'a t -> var -> 'a

  (** Updates the binding for a variable in a substitution *)
  val update : 'a t -> var -> 'a -> 'a t

  (** Maps a function to each binding of a substitution:
      map f theta returns a substitution theta' such that for all x,
      theta'(x) = f(theta(x)) *)
  val map : ('a -> 'b) -> 'a t -> 'b t
      
  (** Pretty-printer for substitutions *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end



(*----------------------------------------------------------------------------*)
(** Terms are constructed inductively by applying functions to other terms,
    e.g. f(x, y, g(z)), where x, y, z are variables and f a function symbol *)
module Term : sig

  (** Type for function symbols *)
  type fsym

  (** Type of a term *)
  type t =
      Var of var
    | Fun of fsym * t list

  (** Creates a function symbol from a string *)
  val mk_fsym : string -> fsym

  (** Function symbols for zero and successor *) 
  val fzero : fsym
  val fsucc : fsym

  (** Creates a term representing an integer in unary.
      (could be convenient when writing examples, otherwise unimportant) *) 
  val term_of_int : int -> t

  (** Checks whether a term is ground (i.e. contains no variables) *)
  val is_ground : t -> bool

  (** Maps a function [f] to each variable in a term [t], 
      i.e. replaces each variable [Var x] in [t] with [f x]. *)
  val map : (var -> t) -> t -> t

  (** Applies a substitution to a term *)
  val apply : t Subst.t -> t -> t

  (** Pretty-printer for terms *)
  val pp : Format.formatter -> t -> unit
  val pp_terms : Format.formatter -> t list -> unit
end

(** Convenient alias for the term type *)
type term = Term.t

(** Convenient alias for the substitution type *)
type subst = term Subst.t



(*----------------------------------------------------------------------------*)
(** Atoms are unary predicates applied to terms, e.g. P(f(x,y)) *)
module Atom : sig

  (** Type for predicate symbols *)
  type psym

  (** Type of an atom *)
  type t = { p : psym; arg : term; }

  (** Creates a predicate symbol from a string.
      The string must be in [a-z][A-Za-z0-9_']* (the lexer conforms to this) *)
  val mk_psym : string -> psym

  (** Creates a variant of a predicate symbol,
      guaranteed to have never been seen before *)
  val variant_psym : psym -> psym

  (** Checks whether an atom is ground (i.e. contains no variables) *)
  val is_ground : t -> bool

  (** Maps a function to all variables in an atom *)
  val map : (var -> term) -> t -> t

  (** Applies a substitution to an atom *)
  val apply : subst -> t -> t

  (** Pretty-printer for atoms *)
  val pp : Format.formatter -> t -> unit
end

(** Convenient alias for the atom type *)
type atom = Atom.t



(*----------------------------------------------------------------------------*)
(** A rule is a definite Horn clause, i.e. a formula of the form 
        A_1 /\ … /\ A_n => A
      where A, A_1, …, A_n are atoms.
      A_1, …, A_n are the hypotheses of the rule, A its conclusion. *)
module Rule : sig

  (** Type of a rule *)
  type t = { hyps : atom list; concl : atom; }

  (** Applies a function to all variables in a rule *) 
  val map : (var -> term) -> t -> t

  (** Applies a substitution to a rule *)
  val apply : subst -> t -> t

  (** Pretty-printer for rules *)
  val pp : Format.formatter -> t -> unit
end

(** Convenient alias for the rule type *)
type rule = Rule.t