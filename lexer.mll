{
	open Lexing
	open Parser
	
	exception Lexing_done
	exception Lexing_error of string
	
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
				with Not_found -> IDENT s
	
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
	  [' '-'~'] # ['\\' '"'] (* TODO : ajouter le caractère DEL *)
	| "\\\\"
	| "\\\""
	| "\\n"
	| "\\t"
	| "\\x" chiffre_hexa chiffre_hexa
let chaine = '"' caractere* '"'

let whitespace = [' ' '\t' '\n']+
let commentaire_simple = "//" [^'\n']* '\n'

rule token = parse
	| whitespace                { token lexbuf }
	| ident as id               { id_or_keyword id }
	| entier as n               { INTEGER n }
	| '"' (caractere* as s) '"' { STRING s }
	| '{'    { LBRACE }
	| '}'    { RBRACE }
	| '('    { LPAREN }
	| ')'    { RPAREN }
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
	| ','    { COMMA }
	| "#include <iostream>" { IOSTREAM }
	| "std::cout"           { COUT }
	| "<<"   { FLOW }
	| "/*"   { comment lexbuf }
	| commentaire_simple { token lexbuf }
	| eof    { raise Lexing_done }
	| _ as c { raise (Lexing_error ("Caractère inattendu : " ^ String.make 1 c)) }

and comment = parse
	| "*/"  { token lexbuf }
	| _     { comment lexbuf }
	| eof   { raise (Lexing_error "Fin de fichier inattendue : Commentaire non terminé")}



{
}
(*
let type_ = "void" | "int" | ident
let var = ident | '*' var | '&' var
let qvar = ident | '*' qvar | '&' qvar
let decl_vars = type_ var (',' var)* ';'

let argument = type_ var
let proto = (type_ qvar | tindent | tident "::" tident) argument (',' argument)*

let member = decl_vars | "virtual"? proto
let supers = ':' "public" tident (',' "public" tident)
let decl_class = "class" ident supers? "{ public :" member* "};"

let qident = ident | tident "::" ident

let operateur =
	  "=="
	| "!="
	| '<'
	| "<="
	| '>'
	| ">="
	| '+'
	| '-'
	| '*'
	| '/'
	| '%'
	| "&&"
	| "||"
let expr =
	  entier
	| "this"
	| "false"
	| "true"
	| "NULL"
	| qident
	| '*' expr
	| expr '.' ident
	| expr "->" ident
	| expr '=' expr
	| expr '(' expr (',' expr)* ')'
	| "new" tident expr (',' expr)*
	| "++" expr
	| "--" expr
	| expr "++"
	| expr "--"
	| "&" expr
	| "!" expr
	| "-" expr
	| "+" expr
	| expr operateur expr
	| '(' expr ')'
let expr_str = expr | chaine

let instruction =
	  ';'
	| expr ';'
	| type_ var ('=' expr | '=' tident '(' expr (',' expr)* ')')? ';'
	| "if (" expr ')' instruction
	| "if (" expr ')' instruction "else" instruction
	| "while (" expr ')' instruction
	| "for (" expr (',' expr)* ';' expr? ';' expr (',' expr)* ')' instruction
	| bloc
	| "std::cout" ("<<" expr_str)+ ';'
	| "return" expr? ';'
let bloc = '{' instruction* '}'

let decl = decl_vars | decl_class | proto bloc
let fichier = "#include <iostream>"? decl* eof

*)
