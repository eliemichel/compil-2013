let parse_only = ref false

let get_pos buf =
	let start = buf.Lexing.lex_start_p in
	let pos = buf.Lexing.lex_curr_p in
		pos.Lexing.pos_lnum,
		start.Lexing.pos_cnum - start.Lexing.pos_bol,
		pos.Lexing.pos_cnum - pos.Lexing.pos_bol

let print_error filename buf msg =
	let line, start_pos, end_pos = get_pos buf in
	Printf.eprintf
		"File \"%s\", line %d, caracters %d-%d:\n%s\n"
		filename
		line
		start_pos
		end_pos
		msg

let main filename = 
	let f = open_in filename in
	let buf = Lexing.from_channel f in
	let print_error = print_error filename buf in
	
	let _ =
		try
			Parser.program Lexer.token buf
		with
		| Lexer.Error err -> (
			print_error ("Lexing error: " ^ err);
			exit 1
			)
		| Parser.Error -> (
			print_error ("Parse error: unexpected token " ^ (Lexing.lexeme buf));
			exit 1
			)
		| _ -> exit 2 (* Unexpected error *)
	in
	if !parse_only then exit 0
	else
		exit 0 (* Étapes suivantes *)

let () = Arg.parse
	["--parse-only", Arg.Set parse_only,
	 "Do only lexing and parsing. Do not generate any output file."]
	main
	"Usage : minic++ [options] filename.\n\
	Compile the given file to MIPS assembly.\n\
	The available options are :"



