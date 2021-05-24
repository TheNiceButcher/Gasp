let print_position outx lexbuf =
  Lexing.(
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "Ligne %d Col %d"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  )


let _ =
(* On verifie si on lance le programme avec ou sans un nom de fichier *)
  let nb_arg = Array.length Sys.argv in
  if (nb_arg = 2) then
	  let c = open_in Sys.argv.(1) in
	  let lb = Lexing.from_channel c
		 in
		 try
		    let ast =
		      Parser.s Lexer.lexeur lb
		    in Syntax.affiche_instruction (snd ast);Interpreter.exec ast;
		with
		| Lexer.Error msg ->
	     Printf.fprintf stderr "%a: Erreur lexeur reading %s\n" print_position lb msg;
	     exit (-1)
	  | Parser.Error ->
	     Printf.fprintf stderr "%a: Erreur de syntaxe \n" print_position lb;
	     exit (-1)
	  | Interpreter.Error s ->
	     Printf.fprintf stderr "%a Erreur de valeur %s\n" print_position lb s;
	     exit (-1)
  else
  begin
  	print_string "Tapez Arret quand on avez fini\n";
	let r = read_line() in
		if(r = "Arret") then print_string "Arret enregistre\n"
		else print_string "Autre chose\n" end
