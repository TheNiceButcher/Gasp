
let _ =
  let c = open_in Sys.argv.(1) in
  let lb = Lexing.from_channel c
	  in
	    let ast =
	      Parser.s Lexer.lexeur lb
	    in Interpreter.exec ast
