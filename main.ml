

let () = 
	let rec print_lexbuf token lexbuf =
		try
			print_string (Parser.string_of_token (token lexbuf));
			print_string "\n";
			print_lexbuf token lexbuf
		with Lexer.Lexing_done -> print_string "EOF\n"
	in


	let f = open_in "tests/syntax/bad/testfile-illegal_char-2.cpp" in
	let buf = Lexing.from_channel f in


	try
		print_string "Printing...\n";
		print_lexbuf Lexer.token buf;
		print_string "done.\n"
	
	with Lexer.Lexing_error err -> print_string ("Lexing error : " ^ err ^ "\n");


	exit 0

