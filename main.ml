(* réponse à La Grande Question sur la vie, l'univers et le reste *)

let parse_only = ref false
let type_only = ref false

let get_pos start_pos end_pos =
	end_pos.Lexing.pos_lnum,
	start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol,
	end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol

let print_error filename msg start_pos end_pos =
	let line, sp, ep = get_pos start_pos end_pos in
	Printf.eprintf
		"File \"%s\", line %d, caracters %d-%d:\n%s\n"
		filename
		line
		sp
		ep
		msg


let print_syntax_error filename buf msg =
	print_error
		filename
		msg
		(buf.Lexing.lex_start_p)
		(buf.Lexing.lex_curr_p)

let main filename = 
	let f = open_in filename in
	let buf = Lexing.from_channel f in
	let print_syntax_error = print_syntax_error filename buf in
	let print_error = print_error filename in
	
	let pAst =
		try
			Parser.program Lexer.token buf
		with
		| Lexer.Error err -> (
			print_syntax_error ("Lexing error: " ^ err);
			exit 1
			)
		| Parser.Error -> (
			print_syntax_error
				("Parse error: unexpected token " ^ (Lexing.lexeme buf));
			exit 1
			)
		| _ -> exit 2
	in
	if !parse_only then exit 0
	else
	let tAst =
		try
			Typer.typing pAst
		with
		| Typer.Error (err, sp, ep) -> (
			print_error ("Typing error: " ^ err) sp ep;
			exit 1
			)
	in
	if !type_only then exit 0
	else
		Format.printf "%a" Mips.print_program (Prod.compile tAst);
		exit 0

let () = Arg.parse
	["--parse-only", Arg.Set parse_only,
	 "Do only lexing and parsing. Do not generate any output file.";
	 "--type-only", Arg.Set type_only,
	 "Do only lexing, parsing and typing. Do not generate any output file."]
	main
	"Usage : minic++ [options] filename.\n\
	Compile the given file to MIPS assembly.\n\
	The available options are :"



