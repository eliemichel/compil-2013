

let () = 
	(*let rec print_lexbuf token lexbuf =
		try
			print_string (Parser.string_of_token (token lexbuf));
			print_string "\n";
			print_lexbuf token lexbuf
		with Lexer.Lexing_done -> print_string "EOF\n"
	in*)


	let f = open_in "tests/syntax/bad/testfile-illegal_char-2.cpp" in
	let buf = Lexing.from_channel f in

	let _ =
		try
			Parser.program Lexer.token buf
		with Lexer.Lexing_error err -> (
			Printf.eprintf "Lexing error: %s\n" err;
			exit 1
			)
	in

	exit 0

