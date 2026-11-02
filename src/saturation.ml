open Syntax
open Term


let rec occurence (x: var) (t : term) : bool =
  match t with
  | Term.Var y -> (x = y)
  | Term.Fun (_, args) -> List.exists (occurence x) args


let rec unification_loop (eqs: (term * term) list) (theta: subst) : subst option =
  match eqs with
  | [] -> Some(theta)
  | (s, t)::rest -> 
    if s = t then
      unification_loop rest theta
    else
      match s, t with
      | Term.Fun (f, args_f), Term.Fun (g, args_g) ->
        if f <> g then
          (*raise (Invalid_argument "unification echouée")*)
          None
        else
          let pairs = List.combine args_f args_g in
          unification_loop (pairs @ rest) theta
      | Term.Var x, _ -> 
        if occurence x t then
          None
          (*raise (Invalid_argument "unification echouée")*)
        else
          let tmpsigma = Subst.update Subst.empty x t in
          let theta' = Subst.update (Subst.map (Term.apply tmpsigma) theta) x t in

          (*pour être honnete, un LLM m'a aidé a trouvé comment ecrire la ligne suivante.
           J'avais compris ce qui etait attendu mais la fonction anonyme ne me serait jamais venue en tête et sa syntaxe non plus*)
          let rest' = List.map (fun (u, v) -> (Term.apply tmpsigma u, Term.apply tmpsigma v)) rest in 
          unification_loop rest' theta'

      | _, Term.Var _ -> unification_loop ((t, s)::rest) theta

let unification (u: term) (v: term) : subst option = 
 unification_loop [(u, v)] Subst.empty

let is_atom_notvar (a: atom) : bool =
  match a.arg with
  | Var _ -> false
  | Fun (_, _) -> true

let rec sel (r: rule) : atom =
  match r.hyps with
  | [] -> r.concl
  | pi::rest_hyps -> if is_atom_notvar pi then pi
                     else sel {r with Rule.hyps = rest_hyps} (*même syntaxe qu'en Lean !!*)

let is_solved (r: rule) : bool =
  not(List.exists (is_atom_notvar) r.hyps)
  

(*
https://ocaml.org/manual/5.5/api/List.html

val concat : 'a list list -> 'a list
Concatenate a list of lists. The elements of the argument are all concatenated together (in the same order) to give the result. Not tail-recursive (length of the argument + length of the longest sub-list).


val map : ('a -> 'b) -> 'a list -> 'b list
map f [a1; ...; an] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] with the results returned by f.

*)
let rec list_vars_in_term (t: term) : var list =
  match t with
  | Term.Var x -> [x]
  | Term.Fun (_, []) -> []
  | Term.Fun (_, args) -> List.concat (List.map list_vars_in_term args)

let list_vars_in_atom (a: atom) : var list =
  list_vars_in_term a.arg

let rec list_vars_in_rule (r: rule) : var list =
  match r.hyps with 
  | [] -> list_vars_in_atom r.concl
  | h::rest -> (list_vars_in_atom h) @ (list_vars_in_rule {r with Rule.hyps = rest})

let rec create_refresh (vars: var list) : subst =
  match vars with
  | [] -> Subst.empty
  | v::rest ->  let subst_rest = create_refresh rest in
                let new_v = Term.Var(Var.refresh v) in
                Subst.update subst_rest v new_v

let refresh_rule (r: rule) : rule =
  let vars = list_vars_in_rule r in
  let refresh_subst = create_refresh vars in 
  Rule.apply refresh_subst r


let rec remove_selected_hyp (hyps: atom list) : atom list =
  match hyps with
  | [] -> []
  | hi::rest -> if is_atom_notvar hi then rest 
                else hi::(remove_selected_hyp rest)


let resolution (c1: rule) (c2: rule) : rule option = (*comme en lean, on a option*)
  let c1' = refresh_rule c1 in 
  let c2' = refresh_rule c2 in

  let c1_concl = c1'.concl in
  let c2_selected = sel c2' in 
  if c1_concl.p <> c2_selected.p then None
  else
    match unification c1_concl.arg c2_selected.arg with 
    | None -> None
    | Some sigma -> let h1 = c1'.hyps in
                    let h2 = remove_selected_hyp c2'.hyps in
                    let concl = c2'.concl in
                    let new_rule = {Rule.hyps = h1@h2; Rule.concl = concl} in
                    Some (Rule.apply sigma new_rule)

let rec remove_duplicate_hyps (hyps: atom list) : atom list =
  match hyps with 
  | [] -> []
  | h::rest -> if List.mem h rest then remove_duplicate_hyps rest else h::(remove_duplicate_hyps rest)

let keep_hyp (concl: atom) (h: atom) : bool = 
  match h.arg with
  | Term.Var x -> occurence x concl.arg
  | Term.Fun (_, _) -> true 


let is_tautology (r : rule) : bool = 
  List.mem r.concl r.hyps

(* let rec remove_hyps_with_free_vars (r: rule) : rule =  *)
  (* if is_solved r then  *)
    (* match r.hyps with *)
    (* | [] -> r *)
    (* | h::rest ->  match h.arg with  *)
                  (* | Term.Var x -> if occurence x r.concl.arg then {} *)

(* let simplify_rule (r: rule) : rule =
  let unduplicated_hyps = remove_duplicate_hyps r.hyps in
  let final_hyps =  if is_solved r then List.filter (keep_hyp r.concl) unduplicated_hyps
                    else unduplicated_hyps
                  in
  {r with Rule.hyps = final_hyps} *)

(*
  Un llm et des discussions avec mes camarades m'ont aidé a réécrire simplify_rule et les fonctins auxiliaires ci-dessous. 
  Les idées vienent néanmoins majoritairement de mes camarades et le LLM m'a surtout aidé a résoudre des problèmes de syntaxe et comme outil
  pour discuter et lever mes incompréhensions et pas comme moyen de génération du code
*)
let rec create_canonical_subst (seen: var list) (i: int) (vars: var list) : subst =
  match vars with
  | [] -> Subst.empty
  | v::rest ->
    if List.mem v seen then
      create_canonical_subst seen i rest
    else
      let subst_rest = create_canonical_subst (v::seen) (i + 1) rest in
      let v_canon = Term.Var (Var.mk_var ("V" ^ string_of_int i)) in
      Subst.update subst_rest v v_canon

let normalize_rule (r: rule) : rule =
  let vars = list_vars_in_rule r in
  let sigma = create_canonical_subst [] 0 vars in
  Rule.apply sigma r

let simplify_rule (r: rule) : rule =
  let unduplicated_hyps = remove_duplicate_hyps r.hyps in
  let final_hyps =
    if is_solved r then List.filter (keep_hyp r.concl) unduplicated_hyps
    else unduplicated_hyps
  in
  normalize_rule {r with Rule.hyps = final_hyps}



(*
let rec resolution_on_unsolvedlist (solved_rule: rule list) (unsolved_list : rule list) : rule list = 
  match solved_rule with
  | [] -> []
  | c::rest ->  match resolution solved_rule c with
                | None -> resolution_on_unsolvedlist solved_rule rest 
                | Some r -> r::(resolution_on_unsolvedlist solved_rule rest)

let rec resolution_on_solvedlist (unsolved_rule: rule) (solved_list : rule list) : rule list = 
  match unsolved_rule with
  | [] -> []
  | c::rest ->  match resolution c unsolved_rule with
                | None -> resolution_on_solvedlist unsolved_rule rest 
                | Some r -> r::(resolution_on_solvedlist unsolved_rule rest)
*)

let rec resolution_on_list (r : rule) (l: rule list) : rule list = 
  match l with
  | [] -> []
  | c::rest ->  let res = if is_solved r then resolution r c else resolution c r in
                match res with
                | None -> resolution_on_list r rest 
                | Some r' -> r'::(resolution_on_list r rest)
                
 
let rec saturation_loop (solveds : rule list) (unsolveds : rule list) (todo: rule list) : rule list = 
  match todo with
  | [] -> solveds
  | r::rest ->  Format.printf "Rule: %a@." Rule.pp r;
                let r' = simplify_rule r in 
                if is_tautology r' then saturation_loop solveds unsolveds rest 
                else 
                    if is_solved r' then
                      if List.mem r' solveds then saturation_loop solveds unsolveds rest
                      else 
                        let infered_rules = resolution_on_list r' unsolveds in
                        saturation_loop (r'::solveds) unsolveds (infered_rules @ rest)
                    else  
                      if List.mem r' unsolveds then saturation_loop solveds unsolveds rest
                      else 
                        let infered_rules = resolution_on_list r' solveds in
                        saturation_loop solveds (r'::unsolveds) (infered_rules @ rest)

let saturation (rules : rule list) : rule list = 
  saturation_loop [] [] rules   

let check_atomic (rules: rule list) (query: atom) : bool =
  let solved_rules = saturation_loop [] [] rules in
  let target_rule = { Rule.hyps = []; concl = query } in
  List.mem target_rule solved_rules


let check_query (solved_rules: rule list) (q: atom) : bool =
    let attstar = Atom.variant_psym q.p in 
    let target_rule = {Rule.hyps = [q]; Rule.concl = { Atom.p = attstar; Atom.arg = q.arg }} in
    let newsolved_rules = saturation_loop solved_rules [] [target_rule] in
    List.exists (fun (r : rule) -> r.concl.p = attstar) newsolved_rules
    (*
    List.mem target_rule newsolved_rules 
    List.exists ? new_solved_rules
    let r = {construit avec attstar}
    List.exists r newsolved_rules
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    *)


let hello_rules (rules:rule list) : rule list =
  saturation rules

let hello_query (q:atom) (solved_rules : rule list) : unit =
  (* Format.printf
    "Hello! I have received a query: %a.\n"
    Atom.pp q; *)
  let is_deducible = check_query solved_rules q in
  Format.printf "Query %a : %b\n" Atom.pp q is_deducible
