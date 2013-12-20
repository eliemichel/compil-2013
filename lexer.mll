{
	open Lexing
	open Parser
	
	exception Error of string
	
	let keywords_assoc = [
		"class",   CLASS;
		"else",    ELSE;
		"false",   FALSE;
		"for",     FOR;
		"if",      IF;
		"int",     INT;
		"new",     NEW;
		"NULL",    NULL;
		"public",  PUBLIC;
		"return",  RETURN;
		"this",    THIS;
		"true",    TRUE;
		"virtual", VIRTUAL;
		"void",    VOID;
		"while",   WHILE
	]
	
	
	let id_or_keyword =
		let h = Hashtbl.create 97 in
			List.iter (fun (s, t) -> Hashtbl.add h s t) keywords_assoc;
			fun s ->
				try
					let s = String.lowercase s in
						Hashtbl.find h s
				with Not_found -> (
					if Hashtbl.mem Ast.tidentTbl s
					then TIDENT s
					else IDENT s
				)
	
	
	let newline lexbuf =
		let pos = lexbuf.lex_curr_p in
			lexbuf.lex_curr_p <- {
					pos with
					pos_lnum = pos.pos_lnum + 1;
					pos_bol = pos.pos_cnum
				}
}

let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_')(alpha | chiffre | '_')*
let tident = ident

let chiffre_octal = ['0'-'7']
let chiffre_hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let entier =
	  '0'
	| ['1'-'9'] chiffre*
	| '0' chiffre_octal+
	| "0x" chiffre_hexa+
let caractere =
	  [' '-'~'] # ['\\' '"']
	| "\\\\"
	| "\\\""
	| "\\n"
	| "\\t"
	| "\\x" chiffre_hexa chiffre_hexa
let chaine = '"' caractere* '"'

let whitespace = [' ' '\t']+
let commentaire_simple = "//" [^'\n']* '\n'

rule token = parse
	| '\n'                      { newline lexbuf ; token lexbuf}
	| whitespace                { token lexbuf }
	| ident as id               { id_or_keyword id }
	| entier as n               { INTEGER n }
	| '"' (caractere* as s) '"' { STRING s }
	| '{'    { LBRACE }
	| '}'    { RBRACE }
	| '('    { LPAR }
	| ')'    { RPAR }
	| '='    { EQ }
	| "||"   { OR }
	| "&&"   { AND }
	| "=="   { DBLEQ }
	| "!="   { NEQ }
	| '<'    { LT }
	| "<="   { LEQ }
	| '>'    { GT }
	| ">="   { GEQ }
	| '+'    { PLUS }
	| '-'    { MINUS }
	| '*'    { STAR }
	| '/'    { DIV }
	| '%'    { MOD }
	| '!'    { NOT }
	| "++"   { INCR }
	| "--"   { DECR }
	| '&'    { AMP }
	| "->"   { ARROW }
	| '.'    { DOT }
	| ';'    { SEMCOL }
	| ':'    { COL }
	| ','    { COMMA }
	| "#include <iostream>" { IOSTREAM }
	| "std::cout"           { COUT }
	| "<<"   { FLOW }
	| "/*"   { comment lexbuf }
	| commentaire_simple { newline lexbuf ; token lexbuf }
	| eof    { EOF }
	| _ as c { raise (Error ("unexpected character : " ^ String.make 1 c)) }

and comment = parse
	| '\n'  { newline lexbuf ; comment lexbuf }
	| "*/"  { token lexbuf }
	| _     { comment lexbuf }
	| eof   { raise (Error "unexpected end of file (unterminated commentary)")}



