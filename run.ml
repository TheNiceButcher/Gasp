let print_position outx lexbuf =
  Lexing.(
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "Ligne %d Col %d"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  )


let _ =
  let c = open_in Sys.argv.(1) in
  let lb = Lexing.from_channel c
	 in
	 try
	    let ast =
	      Parser.s Lexer.lexeur lb
	    in Syntax.affiche_instruction (snd ast);Interpreter.exec ast
	with
	| Lexer.Error msg ->
     Printf.fprintf stderr "%a: Lexer error reading %s\n" print_position lb msg;
     exit (-1)
  | Parser.Error ->
     Printf.fprintf stderr "%a: Syntax error\n" print_position lb;
     exit (-1)
  | Interpreter.Error s ->
     Printf.fprintf stderr "Type error: %s\n" s;
     exit (-1)
