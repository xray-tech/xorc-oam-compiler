%{
   open Ast
   let make_pos start e =
     Pos.{ start = of_lexing start;
               finish = of_lexing e }

   let pattern_or_wildcard = function
   | Some x -> x
   | None -> (PWildcard, Pos.dummy_range)

   let optional_list = function
   | Some l -> l
   | None -> []
%}


%token EOF
%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token TRUE FALSE NULL SIGNAL STOP WILDCARD
%token VAL TYPE IMPORT INCLUDE REFER
%token LAMBDA AS DEF
%token IF THEN ELSE
%token LEFT_BRACE RIGHT_BRACE
%token LEFT_BRACK RIGHT_BRACK
%token LEFT_PAREN RIGHT_PAREN
%token NUMBER_SIGN
%token COMMA DOT
%token LESS MORE
%token <string> FFC
%token BAR SEMICOLON
%token SIG DOUBLE_COLON TOVERRIDE
%token <string> ASSIGN ADD SUB MULT DIV EQ NOT_EQ GT LT GTE LTE POW MOD AND OR NOT COLON DEREFERENCE

%nonassoc low

%left TOVERRIDE
%left DOUBLE_COLON

%left AS
%nonassoc IDENT

%left SEMICOLON
%left LESS
%left BAR
%right MORE
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NOT_EQ
%nonassoc GT LT GTE LTE
%right COLON
%left ADD SUB
%left MULT DIV MOD
%right POW

%left DOT
%left LEFT_PAREN LEFT_BRACK

%nonassoc DEREFERENCE
%nonassoc high

%start <Ast.e option> prog
%start <Ast.decl list> orc_module
%%
prog:
  | v = expr; EOF { Some v }
  | EOF           { None   } ;

orc_module:
  | l = decl*; EOF { l };

expr:
  | LEFT_PAREN exp=expr RIGHT_PAREN { exp }
  | LEFT_PAREN e=expr COMMA es=separated_nonempty_list(COMMA, expr) RIGHT_PAREN
    { (ETuple(e::es), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_list(COMMA, k=IDENT EQ e=expr { (k, e) })
    RIGHT_BRACE
    { (ERecord(pairs), make_pos $startpos $endpos) }
  | LEFT_BRACK es=separated_list(COMMA, expr) RIGHT_BRACK
    { (EList(es), make_pos $startpos $endpos) }
  | LEFT_PAREN op=binop RIGHT_PAREN { (EIdent(op), make_pos $startpos $endpos) }
  | STOP { (EStop, make_pos $startpos $endpos)}
  | t=expr DOT f=IDENT { (EFieldAccess(t, f), make_pos $startpos $endpos)}
  | t=expr op=DEREFERENCE
    { let ident = (EIdent(op), make_pos $startpos(op) $endpos(op)) in
        (ECall(ident, [], [t]),
         make_pos $startpos $endpos) }
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
  | target=FFC ar=args
   { (EFFC(target, ar),
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
  | e=expr DOUBLE_COLON ty=ty { (EHasType(e, ty), make_pos $startpos $endpos) }
  | e=expr TOVERRIDE ty=ty { (EOverrideType(e, ty), make_pos $startpos $endpos) }
  | LAMBDA ty_ps=type_params? ps=params return=return_type? EQ e=expr
    { (ELambda(optional_list ty_ps, ps, return, e),
       make_pos $startpos $endpos) }
  | d=decl NUMBER_SIGN? e=expr { (EDecl(d, e), make_pos $startpos $endpos) } %prec low

return_type: DOUBLE_COLON ret=ty { ret }
decl:
  | VAL p=pattern EQ e=expr  { (DVal(p, e), make_pos $startpos $endpos) }
  | DEF name=ident_or_op t_ps=type_params? ps=params ret=return_type? guard=guard? EQ e=expr
    { (DDef(name, (optional_list t_ps), ps, ret, guard, e),
       make_pos $startpos $endpos) }
  | SIG name=IDENT t_ps=type_params? args=arg_types DOUBLE_COLON ret=ty
    { (DSig(name, ((optional_list t_ps), args, ret)),
       make_pos $startpos $endpos) }
  | REFER mod_=mod_name LEFT_PAREN idents=separated_list(COMMA, ident_or_op) RIGHT_PAREN
    { (DRefer(mod_, idents),
       make_pos $startpos $endpos) }
  | IMPORT t=IDENT n=IDENT EQ def=STRING
    { match import_decl t n def with
      | Some d -> (d,
                   make_pos $startpos $endpos)
      | None -> $syntaxerror }
  | INCLUDE p=STRING
    { (DInclude(p),
       make_pos $startpos $endpos) }
  | TYPE i=IDENT t_ps=type_params? EQ ty=ty
    { (DAlias(i, optional_list t_ps, ty),
       make_pos $startpos $endpos) }
  | TYPE i=IDENT t_ps=type_params? EQ cs=constructors
    { (DData(i, optional_list t_ps, cs),
       make_pos $startpos $endpos) }

guard: IF LEFT_PAREN e=expr RIGHT_PAREN { e }
ident_or_op:
  | x=IDENT { x }
  | LEFT_PAREN x=binop RIGHT_PAREN { x }
  | LEFT_PAREN x=NOT RIGHT_PAREN { x }
  | LEFT_PAREN x=DEREFERENCE RIGHT_PAREN { x }

constructors:
  | c=constructor { [c] }
  | BAR c=constructor { [c] }
  | cs=constructors BAR c=constructor { c::cs }

constructor: n=IDENT LEFT_PAREN slots=separated_list(COMMA, slot) RIGHT_PAREN { (n, List.length slots) }
slot:
  | WILDCARD { None }
  | ty=ty { Some(ty) }

params: LEFT_PAREN l=separated_list(COMMA, pattern) RIGHT_PAREN { l }
args: LEFT_PAREN l=separated_list(COMMA, expr) RIGHT_PAREN { l }
arg_types: LEFT_PAREN l=separated_list(COMMA, ty) RIGHT_PAREN { l }
type_args: LEFT_BRACK l=separated_nonempty_list(COMMA, ty) RIGHT_BRACK { l }
type_params: LEFT_BRACK l=separated_nonempty_list(COMMA, IDENT) RIGHT_BRACK { l }

mod_name: l=separated_nonempty_list(DOT, IDENT) { String.concat "." l }

pattern:
  | name=IDENT { (PVar(name), make_pos $startpos $endpos) }
  | WILDCARD { (PWildcard, make_pos $startpos $endpos) }
  | SUB i=INT { (PConst(Int(-i)), make_pos $startpos $endpos) }
  | SUB f=FLOAT { (PConst(Float(-. f)), make_pos $startpos $endpos)}
  | c=const { (PConst(c), make_pos $startpos $endpos) }
  | LEFT_PAREN p=pattern COMMA ps=separated_list(COMMA, pattern) RIGHT_PAREN
    { (PTuple(p::ps), make_pos $startpos $endpos) }
  | LEFT_BRACK ps=separated_list(COMMA, pattern) RIGHT_BRACK
    { (PList(ps), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_list(COMMA, k=IDENT EQ p=pattern { (k, p) })
    RIGHT_BRACE
    { (PRecord(pairs), make_pos $startpos $endpos) }
  | head=pattern COLON tail=pattern
    { (PCons(head, tail), make_pos $startpos $endpos) }
  | name=IDENT LEFT_PAREN ps=separated_list(COMMA, pattern) RIGHT_PAREN
    { (PCall(name, ps), make_pos $startpos $endpos) }
  | p=pattern AS n=IDENT
    { (PAs(p, n), make_pos $startpos $endpos) }
  | LEFT_PAREN p=pattern RIGHT_PAREN
    { p }
  | p=pattern DOUBLE_COLON ty=ty
    { (PTyped(p, ty), make_pos $startpos $endpos) }

ty:
  | name=IDENT { (TyVar(name), make_pos $startpos $endpos) }
  | name=IDENT args=type_args { (TyApp(name, args), make_pos $startpos $endpos) }
  | LEFT_BRACE
      pairs=separated_list(COMMA, k=IDENT DOUBLE_COLON t=ty { (k, t) })
    RIGHT_BRACE
    { (TyRecord(pairs), make_pos $startpos $endpos) }
  | LEFT_PAREN
      l=separated_nonempty_list(COMMA, ty)
    RIGHT_PAREN
    { (TyTuple(l), make_pos $startpos $endpos) }
  | LAMBDA ty_ps=type_params? args=arg_types DOUBLE_COLON return=ty
    { (TyFun((optional_list ty_ps, args, return)),
       make_pos $startpos $endpos) }

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
  | t=EQ | t=NOT_EQ | t=COLON
  { t }

%inline unop:
  | t=SUB | t=NOT
  { t }
