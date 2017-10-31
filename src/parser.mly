%{ open Ast
   open Core
  let make_pos start e =
    Ast.{ pstart = start; pend = e }
  let pattern_or_wildcard = function
  | Some x -> x
  | None -> (PWildcard, dummy)
%}

%token EOF
%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token TRUE FALSE NULL SIGNAL STOP WILDCARD
%token VAL TYPE IMPORT INCLUDE
%token LAMBDA AS DEF
%token IF THEN ELSE
%token LEFT_BRACE RIGHT_BRACE
%token LEFT_BRACK RIGHT_BRACK
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_COMMENT RIGHT_COMMENT
%token COMMENT NUMBER_SIGN
%token COMMA DEREFERENCE
%token LESS MORE
%token BAR SEMICOLON
%token DOUBLE_COLON TOVERRIDE
%token <string> ASSIGN ADD SUB MULT DIV EQ NOT_EQ GT LT GTE LTE POW MOD AND OR

%nonassoc low

%nonassoc IDENT

%left BAR SEMICOLON LESS MORE
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NOT_EQ
%nonassoc GT LT GTE LTE
%left ADD SUB
%left MULT DIV MOD POW

%left LEFT_PAREN
%left LEFT_BRACK

%start <Ast.e option> prog
%%
prog:
  | v = expr; EOF { Some v }
  | EOF           { None   } ;


expr:
  | LEFT_PAREN exp=expr RIGHT_PAREN { exp }
  | LEFT_PAREN e=expr COMMA es=separated_nonempty_list(COMMA, expr) RIGHT_PAREN
    { (ETuple(e::es), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_list(COMMA, k=IDENT EQ e=expr { (k, e) })
    RIGHT_BRACE
    { (ERecord(pairs), make_pos $startpos $endpos) }
  | LEFT_BRACK es=separated_nonempty_list(COMMA, expr) RIGHT_BRACK
    { (EList(es), make_pos $startpos $endpos) }
  | LEFT_PAREN op=binop RIGHT_PAREN { (EIdent(op), make_pos $startpos $endpos) }
  | STOP { (EStop, make_pos $startpos $endpos)}
  | t1=expr op=binop t2=expr
    { let ident = (EIdent(op), make_pos $startpos(op) $endpos(op)) in
        (ECall(ident, [], [t1; t2]),
         make_pos $startpos $endpos) }
  | c=const { (EConst c, make_pos $startpos $endpos) }
  | name=IDENT { (EIdent(name), make_pos $startpos $endpos) }
  | target=expr ar=args 
   { (ECall(target, [], ar),
      make_pos $startpos $endpos) }
  | target=expr t_args=type_args ar=args 
   { (ECall(target, t_args, ar),
      make_pos $startpos $endpos) }
  | e1=expr BAR e2=expr
    { (EParallel(e1, e2),
       make_pos $startpos $endpos) }
  | e1=expr SEMICOLON e2=expr
    { (EOtherwise(e1, e2),
       make_pos $startpos $endpos) }
  | e1 = expr LESS p=pattern? LESS e2 = expr
    { (EPruning(e1, pattern_or_wildcard p, e2),
       make_pos $startpos $endpos) }
  | e1 = expr MORE p=pattern? MORE e2 = expr
    { (ESequential(e1, pattern_or_wildcard p, e2),
       make_pos $startpos $endpos) }
  | IF p=expr THEN t=expr ELSE e=expr
    { (ECond(p, t, e),
       make_pos $startpos $endpos) } %prec low
  | d=decl NUMBER_SIGN? e=expr { (EDecl(d, e), make_pos $startpos $endpos) } %prec low

pattern:
  | name=IDENT { (PVar(name), make_pos $startpos $endpos) }
  | WILDCARD { (PWildcard, make_pos $startpos $endpos) }
  | c=const { (PConst(c), make_pos $startpos $endpos) }
  | LEFT_PAREN p=pattern COMMA ps=separated_nonempty_list(COMMA, pattern) RIGHT_PAREN
    { (PTuple(p::ps), make_pos $startpos $endpos) }
   
decl:
  | VAL p=pattern EQ e=expr  { (DVal(p, e), make_pos $startpos $endpos) }
  | DEF name=IDENT t_params=type_params? args=arg_types ret=ty
    { (DSig(name, (Option.value t_params ~default:[]), args, ret),
       make_pos $startpos $endpos) }
type_params: LEFT_BRACK l=separated_nonempty_list(COMMA, IDENT) RIGHT_BRACK { l }
type_args: LEFT_BRACK l=separated_nonempty_list(COMMA, ty) RIGHT_BRACK { l }
arg_types: LEFT_PAREN l=separated_list(COMMA, ty) RIGHT_PAREN { l }
args: LEFT_PAREN l=separated_list(COMMA, expr) RIGHT_PAREN { l }
ty:
  | name=IDENT { (TyVar(name), make_pos $startpos $endpos) }
  | name=IDENT args=type_args { (TyApp(name, args), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_nonempty_list(COMMA, k=IDENT EQ t=ty { (k, t) })
    RIGHT_BRACE
    { (TyRecord(pairs), make_pos $startpos $endpos) }
const:
  | i=INT { Int i }
  | f=FLOAT { Float f }
  | s=STRING { String s }
  | NULL { Null }
  | FALSE { Bool false }
  | TRUE { Bool true }
  | SIGNAL { Signal }


%inline binop:
  | t=ADD | t=MULT | t=SUB | t=DIV | t=POW | t=MOD
  | t=LT | t=GT | t=GTE | t=LTE | t=AND | t=OR | t=ASSIGN
  | t=EQ | t=NOT_EQ
  { t }
