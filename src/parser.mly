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
%token COMMA DEREFERENCE DOT
%token LESS MORE
%token BAR SEMICOLON
%token DOUBLE_COLON TOVERRIDE
%token <string> ASSIGN ADD SUB MULT DIV EQ NOT_EQ GT LT GTE LTE POW MOD AND OR NOT COLON

%nonassoc low

%left AS
%nonassoc IDENT

%left DOUBLE_COLON TOVERRIDE
%left BAR SEMICOLON LESS MORE
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NOT_EQ
%nonassoc GT LT GTE LTE
%right COLON
%left ADD SUB
%left MULT DIV MOD POW

%left DOT
%left LEFT_PAREN
%left LEFT_BRACK

%nonassoc high

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
  | t=expr DOT f=IDENT { (EFieldAccess(t, f), make_pos $startpos $endpos)}
  | op=unop e=expr
    { let ident = (EIdent(op), make_pos $startpos(op) $endpos(op)) in
        (ECall(ident, [], [e]),
         make_pos $startpos $endpos) } %prec high
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
  | e=expr DOUBLE_COLON ty=ty { (EHasType(e, ty), make_pos $startpos $endpos) }
  | e=expr TOVERRIDE ty=ty { (EOverrideType(e, ty), make_pos $startpos $endpos) }
  | LAMBDA ty_ps=type_params? ps=params return=ty? EQ e=expr
    { (ELambda(Option.value ty_ps ~default:[], ps, return, e),
       make_pos $startpos $endpos) }

pattern:
  | name=IDENT { (PVar(name), make_pos $startpos $endpos) }
  | WILDCARD { (PWildcard, make_pos $startpos $endpos) }
  | c=const { (PConst(c), make_pos $startpos $endpos) }
  | LEFT_PAREN p=pattern COMMA ps=separated_list(COMMA, pattern) RIGHT_PAREN
    { (PTuple(p::ps), make_pos $startpos $endpos) }
  | LEFT_BRACK ps=separated_list(COMMA, pattern) RIGHT_BRACK
    { (PList(ps), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_nonempty_list(COMMA, k=IDENT EQ p=pattern { (k, p) })
    RIGHT_BRACE
    { (PRecord(pairs), make_pos $startpos $endpos) }
  | head=pattern COLON tail=pattern
    { (PCons(head, tail), make_pos $startpos $endpos) }
  | p=pattern AS n=IDENT
    { (PAs(p, n), make_pos $startpos $endpos) }
  | p=pattern DOUBLE_COLON ty=ty
    { (PTyped(p, ty), make_pos $startpos $endpos) }
  | LEFT_PAREN p=pattern RIGHT_PAREN
    { p }

decl:
  | VAL p=pattern EQ e=expr  { (DVal(p, e), make_pos $startpos $endpos) }
  | DEF name=IDENT t_ps=type_params? args=ty_or_patterns ret=ty
    { (DSig(name, ((Option.value t_ps ~default:[]), List.map ~f:ty_of_p_or_ty args, ret)),
       make_pos $startpos $endpos) }
  | DEF name=IDENT t_ps=type_params? ps=ty_or_patterns ret=ty? guard=expr? EQ e=expr
    { (DDef(name, (Option.value t_ps ~default:[]), List.map ~f:p_of_p_or_ty ps, ret, guard, e),
       make_pos $startpos $endpos) }

type_params: LEFT_BRACK l=separated_nonempty_list(COMMA, IDENT) RIGHT_BRACK { l }
params: LEFT_PAREN l=separated_list(COMMA, pattern) RIGHT_PAREN { l }
type_args: LEFT_BRACK l=separated_nonempty_list(COMMA, ty) RIGHT_BRACK { l }
arg_types: LEFT_PAREN l=separated_list(COMMA, ty) RIGHT_PAREN { l }
args: LEFT_PAREN l=separated_list(COMMA, expr) RIGHT_PAREN { l }
ty_or_patterns: LEFT_PAREN l=separated_list(COMMA, ty_or_pattern) RIGHT_PAREN { l }
ty:
  | name=IDENT { (TyVar(name), make_pos $startpos $endpos) }
  | name=IDENT args=type_args { (TyApp(name, args), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_nonempty_list(COMMA, k=IDENT EQ t=ty { (k, t) })
    RIGHT_BRACE
    { (TyRecord(pairs), make_pos $startpos $endpos) }
  | LEFT_PAREN
      l=separated_nonempty_list(COMMA, ty)
    RIGHT_PAREN
    { (TyTuple(l), make_pos $startpos $endpos) }
  | LAMBDA ty_ps=type_params? args=arg_types return=ty
    { (TyFun((Option.value ty_ps ~default:[], args, return)),
       make_pos $startpos $endpos) }

const:
  | i=INT { Int i }
  | f=FLOAT { Float f }
  | s=STRING { String s }
  | NULL { Null }
  | FALSE { Bool false }
  | TRUE { Bool true }
  | SIGNAL { Signal }

ty_or_pattern:
  | name=IDENT { (PTyIdent(name), make_pos $startpos $endpos) }
  | name=IDENT args=type_args { (PTyApp(name, args), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_nonempty_list(COMMA, k=IDENT EQ v=ty_or_pattern { (k, v) })
    RIGHT_BRACE
    { (PTyRecord(pairs), make_pos $startpos $endpos) }
  | LEFT_PAREN
      l=separated_nonempty_list(COMMA, ty_or_pattern)
    RIGHT_PAREN
    { (PTyTuple(l), make_pos $startpos $endpos) }
  | LAMBDA ty_ps=type_params? args=arg_types return=ty
    { (PTyFun((Option.value ty_ps ~default:[], args, return)),
       make_pos $startpos $endpos) }
  | WILDCARD { (PTyWildcard, make_pos $startpos $endpos) }
  | c=const { (PTyConst(c), make_pos $startpos $endpos) }
  | LEFT_BRACK ps=separated_list(COMMA, pattern) RIGHT_BRACK
    { (PTyList(ps), make_pos $startpos $endpos) }
  | head=ty_or_pattern COLON tail=ty_or_pattern
    { (PTyCons(head, tail), make_pos $startpos $endpos) }
  | p=ty_or_pattern AS n=IDENT
    { (PTyAs(p, n), make_pos $startpos $endpos) }
  | p=ty_or_pattern DOUBLE_COLON ty=ty
    { (PTyTyped(p, ty), make_pos $startpos $endpos) }



%inline binop:
  | t=ADD | t=MULT | t=SUB | t=DIV | t=POW | t=MOD
  | t=LT | t=GT | t=GTE | t=LTE | t=AND | t=OR | t=ASSIGN
  | t=EQ | t=NOT_EQ | t=COLON
  { t }


%inline unop:
  | t=SUB | t=NOT
  { t }
