%token <int> NB
%token <string> ID
%token PLUS MOINS PARG PARD EGAL EOF
%token COLON VAR
%token DEBUT FIN BPINCEAU HPINCEAU
%token AVANCE TOURNE
%token SI FAIRE ALORS SINON TANTQUE
%start <Syntax.program> s

%{ open Syntax %}
%%

s: p=program EOF                                     {p}

program: ds=declaration* is=instruction    {(ds,is)}

declaration: VAR i=ID COLON                {i}

instruction:
  | AVANCE e=expression {Avance(e)}
  | TOURNE e=expression {Tourne(e)}
  | BPINCEAU {BasPinceau}
  | HPINCEAU {HautPinceau}
  | i=ID EGAL e=expression {Affect(i,e)}
  | DEBUT b=blocinstruction FIN {Bloc(b)}
  | SI e=expression ALORS i_si=instruction SINON i_non=instruction {Cond(e,i_si,i_non)}
  | TANTQUE e=expression FAIRE i=instruction {Repet(e,i)}
blocinstruction:
  | {[]}
  | i=instruction COLON b=blocinstruction {i::b}
expression:
  | s=ID  e=expressionSuite {App(Ident s,e)}
  | n=NB  e=expressionSuite {App(Const n,e)}
  | PARG e=expression PARD es=expressionSuite {App(e,es)}
expressionSuite:
  | PLUS e=expression {(Plus,e)}
  | MOINS e=expression {(Moins,e)}
  | {(Identite,Const 0)}
