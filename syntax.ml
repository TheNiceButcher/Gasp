type op =
	Plus | Moins | Identite | Mult | Div
type wtf =
	op * expression
and expression =
	Const of int
	| Ident of string
	| App of expression * wtf
	| Neg of expression
type instruction =
	Avance of expression
	| Tourne of expression
	| BasPinceau
	| HautPinceau
	| ChangeEpaisseur of expression
	| ChangeCouleur of string
	| Affect of string * expression
	| Bloc of instruction list
	| Cond of expression * instruction * instruction option
	| Repet of expression * instruction
type declaration = string
type program = declaration list * instruction
type interpreter = Inst of instruction | Decl of declaration
let rec affiche_expression e =
	match e with
	| Const n -> print_int n
	| Ident s -> print_string s
	| Neg e -> print_string "-("; affiche_expression e; print_string ")"
	| App (e,w)-> affiche_expression e;
				match w with
	 			| (Plus, e1) -> print_string "+";affiche_expression e1
				| (Moins,e1) -> print_string "-";affiche_expression e1
				| (Identite,_) -> print_string ";"
				| (Mult,e1) -> print_string "*";affiche_expression e1
				| (Div,e1) -> print_string "/";affiche_expression e1

let rec affiche_instruction i =
	match i with
	| Avance e -> print_string "Avance "; affiche_expression e;
	| Tourne e -> print_string "Tourne "; affiche_expression e;
	| BasPinceau -> print_string "BasPinceau"
	| HautPinceau ->  print_string "HautPinceau"
	| ChangeEpaisseur e -> print_string "ChangeEpaisseur "; affiche_expression e;
	| ChangeCouleur c -> print_string ("ChangeCouleur " ^ c);
	| Affect (s,e) -> print_string (s ^ " = "); affiche_expression e;
	| Bloc(l) -> List.iter (affiche_instruction) l
	| Repet(e,i1) -> print_string "Tant que ";affiche_expression e; print_string "Faire";
				affiche_instruction i1;
	| Cond(e,i1,i2) -> print_string "si ";affiche_expression e; print_string "Alors";
			affiche_instruction i1;
					match i2 with
					| Some i -> print_string "Sinon"; affiche_instruction i;
					| None -> print_string "";;

let affiche_inter m =
	match m with
	| Inst i  -> affiche_instruction i
	| Decl d -> print_string d
