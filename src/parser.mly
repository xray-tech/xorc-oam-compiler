%{ open Ast
  let make_pos start e =
    Ast.{ pstart = start; pend = e }
  let pattern_or_wildcard = function
  | Some x -> x
  | None -> (PWildcard, dummy)

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
%token BAR SEMICOLON
%token <string> ASSIGN ADD SUB MULT DIV EQ NOT_EQ GT LT GTE LTE POW MOD AND OR NOT COLON DEREFERENCE

%nonassoc low

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
%left MULT DIV MOD POW

%left DOT
%left LEFT_PAREN

%nonassoc DEREFERENCE
%nonassoc high

%start <Ast.e option> prog
%start <Ast.decl list> ns
%%
prog:
  | v = expr; EOF { Some v }
  | EOF           { None   } ;

ns:
  | l = decl*; EOF { l }
  | EOF            { []   } ;


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
        (ECall(ident, [t]),
         make_pos $startpos $endpos) }
  | op=unop e=expr
    { let ident = (EIdent(op), make_pos $startpos(op) $endpos(op)) in
        (ECall(ident, [e]),
         make_pos $startpos $endpos) } %prec high
  | t1=expr op=binop t2=expr
    { let ident = (EIdent(op), make_pos $startpos(op) $endpos(op)) in
        (ECall(ident, [t1; t2]),
         make_pos $startpos $endpos) }
  | c=const { (EConst c, make_pos $startpos $endpos) }
  | name=IDENT { (EIdent(name), make_pos $startpos $endpos) }
  | target=expr ar=args
   { (ECall(target, ar),
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
  | LAMBDA ps=params EQ e=expr
    { (ELambda(ps, e),
       make_pos $startpos $endpos) }
  | d=decl NUMBER_SIGN? e=expr { (EDecl(d, e), make_pos $startpos $endpos) } %prec low

decl:
  | VAL p=pattern EQ e=expr  { (DVal(p, e), make_pos $startpos $endpos) }
  | DEF name=IDENT ps=params guard=guard? EQ e=expr
    { (DDef(name, ps, guard, e),
       make_pos $startpos $endpos) }
  | REFER ns=namespace LEFT_PAREN idents=separated_list(COMMA, IDENT) RIGHT_PAREN
    { (DRefer(ns, idents),
       make_pos $startpos $endpos) }
  | IMPORT t=IDENT n=IDENT EQ def=STRING
    { match import_decl t n def with
      | Some d -> (d,
                   make_pos $startpos $endpos)
      | None -> $syntaxerror }
  | INCLUDE p=STRING
    { (DInclude(p),
       make_pos $startpos $endpos) }
  | TYPE i=IDENT EQ cs=constructors
    { (DData(i, cs),
       make_pos $startpos $endpos) }

guard: IF LEFT_PAREN e=expr RIGHT_PAREN { e }

constructors:
  | c=constructor { [c] }
  | BAR c=constructor { [c] }
  | cs=constructors c=constructor { c::cs }

constructor: n=IDENT LEFT_PAREN slots=separated_list(COMMA, WILDCARD) RIGHT_PAREN { (n, List.length slots) }

params: LEFT_PAREN l=separated_list(COMMA, pattern) RIGHT_PAREN { l }
args: LEFT_PAREN l=separated_list(COMMA, expr) RIGHT_PAREN { l }

namespace: l=separated_nonempty_list(DOT, IDENT) { String.concat "." l }

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
      pairs=separated_nonempty_list(COMMA, k=IDENT EQ p=pattern { (k, p) })
    RIGHT_BRACE
    { (PRecord(pairs), make_pos $startpos $endpos) }
  | head=pattern COLON tail=pattern
    { (PCons(head, tail), make_pos $startpos $endpos) }
  | p=pattern AS n=IDENT
    { (PAs(p, n), make_pos $startpos $endpos) }
  | LEFT_PAREN p=pattern RIGHT_PAREN
    { p }


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
