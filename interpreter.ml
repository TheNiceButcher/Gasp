type tortue = {
	position : int * int;
	angle: int;
	dessine: bool;
	env: (string * int) list;
}
open Syntax
open Graphics
exception Error of string
let deg_to_rad n =
	let a = n mod 360 in
		let a' = if(a < 0) then (float_of_int a) +. 360.
		 		else (float_of_int a)
		in
		a' /. 180. *. 3.1415927
let nvx_pos (x,y) a dist=
	let angle = deg_to_rad a in
	 let pos = 	(float_of_int x +. (float_of_int dist) *. (cos angle),
		 		float_of_int y +. (float_of_int dist) *. (sin angle))
	in (int_of_float (fst pos),int_of_float (snd pos))
(* Initialise les variables déclarées à 0 dans l *)
let init_var l = List.map (fun(s) -> (s,0)) l
(*Calcule la valeur de expr dans l'environnement env*)
let rec calcul env expr =
 	match expr with
	| Const n -> n
	| Ident s -> begin
					let e = List.assoc_opt s env in
					match e with
					| Some n -> n
					| None -> raise (Error ("identificateur " ^ s ^ " non declare"))
				end
	| App(e,w) ->
	 	let val_e = calcul env e in
		match w with
		| (Plus,e1) -> val_e + calcul env e1
		| (Moins,e1) -> val_e - calcul env e1
(*Execute l'instruction i*)
let rec exec_inst tortue i =
	match i with
	| Avance e ->
		begin
		let n = calcul tortue.env e in
		let npos = nvx_pos tortue.position tortue.angle n in
		if (tortue.dessine) then lineto (fst npos) (snd npos)
		else moveto (fst npos) (snd npos);
		{tortue with position=npos}
		end
	| Tourne e ->
		let n = calcul tortue.env e in
		{ tortue with angle = ((tortue.angle + n) mod 360) }
	| BasPinceau -> {tortue with dessine = true}
	| HautPinceau -> {tortue with dessine = false}
	| Affect(s,e) ->
		let n = calcul tortue.env e in
		{ tortue with env =
		(List.map (fun (id,v) -> if(s = id) then (id,n) else (id,v)) tortue.env) }
	| Bloc(l) -> exec_bloc tortue l
	| Cond(e,i1,i2) ->
		let n = calcul tortue.env e in
		if(n <> 0)
		then
			exec_inst tortue i1
		else
			exec_inst tortue i2
	| Repet(e,i1) ->
		let n = calcul tortue.env e in
		if (n <> 0)
		then
			let t = exec_inst tortue i1 in
			exec_inst t i;
		else
			tortue

and exec_bloc tortue b =
	match b with
	| [] -> tortue
	| i::ll -> let t = exec_inst tortue i in
			exec_bloc t ll;;
let exec p =
	open_graph " 800x800";
	let envi = init_var (fst p) in
	let tortue = {position = (0,0);angle = 90;dessine=false;env = envi} in
	let _ = exec_inst tortue (snd p) in
	Unix.sleep 10;
