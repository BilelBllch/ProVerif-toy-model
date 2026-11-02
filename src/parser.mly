%{
open Syntax
%}

%token <string> VAR
%token <string> CST
%token <int> INT
%token LPAR COMMA RPAR
%token IMPLIES AND STOP
%token EOF

%start rules
%type <rule list> rules

%start query
%type <atom> query

%%

rules:
  | EOF   { [] }
  | rule rules { $1::$2 }

query:
  | atom STOP { $1 }
  | EOF { exit 0 }

rule:
  | atoms IMPLIES atom STOP { Rule.{hyps=$1; concl=$3} }
  | atom STOP               { Rule.{hyps=[]; concl=$1} }

atom:
  | CST LPAR term RPAR     { Atom.{p=mk_psym $1; arg=$3} }

atoms:
  |                         { [] }
  | atom                    { [$1] }
  | atom AND atoms          { $1 :: $3 }

terms:
  |                         { [] }
  | term                    { [$1] }
  | term COMMA terms        { $1 :: $3 }

term:
  | VAR                     { Term.Var (Var.mk_var $1) }
  | CST                     { Term.(Fun (mk_fsym $1, [])) }
  | CST LPAR terms RPAR     { Term.(Fun (mk_fsym $1, $3)) }
  | INT                     { Term.term_of_int $1 }

