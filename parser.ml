
type token =
	| CLASS
	| ELSE
	| FALSE
	| FOR
	| IF
	| INT
	| NEW
	| NULL
	| PUBLIC
	| RETURN
	| THIS
	| TRUE
	| VIRTUAL
	| VOID
	| WHILE
	
	
	| LBRACE
	| RBRACE
	| LPAREN
	| RPAREN
	| EQ
	| OR
	| AND
	| DBLEQ
	| NEQ
	| LT
	| LEQ
	| GT
	| GEQ
	| PLUS
	| MINUS
	| STAR
	| DIV
	| MOD
	| NOT
	| INCR
	| DECR
	| AMP
	| ARROW
	| DOT
	| SEMCOL
	| COMMA
	
	
	| IOSTREAM
	| FLOW
	| COUT
	
	
	| IDENT   of string
	| STRING  of string
	| INTEGER of string


let string_of_token = function
	| CLASS   -> "CLASS"
	| ELSE    -> "ELSE"
	| FALSE   -> "FALSE"
	| FOR     -> "FOR"
	| IF      -> "IF"
	| INT     -> "INT"
	| NEW     -> "NEW"
	| NULL    -> "NULL"
	| PUBLIC  -> "PUBLIC"
	| RETURN  -> "RETURN"
	| THIS    -> "THIS"
	| TRUE    -> "TRUE"
	| VIRTUAL -> "VIRTUAL"
	| VOID    -> "VOID"
	| WHILE   -> "WHILE"
	
	
	| LBRACE -> "LBRACE"
	| RBRACE -> "RBRACE"
	| LPAREN -> "LPAREN"
	| RPAREN -> "RPAREN"
	| EQ     -> "EQ"
	| OR     -> "OR"
	| AND    -> "AND"
	| DBLEQ  -> "DBLEQ"
	| NEQ    -> "NEQ"
	| LT     -> "LT"
	| LEQ    -> "LEQ"
	| GT     -> "GT"
	| GEQ    -> "GEQ"
	| PLUS   -> "PLUS"
	| MINUS  -> "MINUS"
	| STAR   -> "STAR"
	| DIV    -> "DIV"
	| MOD    -> "MOD"
	| NOT    -> "NOT"
	| INCR   -> "INCR"
	| DECR   -> "DECR"
	| AMP    -> "AMP"
	| ARROW  -> "ARROW"
	| DOT    -> "DOT"
	| SEMCOL -> "SEMCOL"
	| COMMA  -> "COMMA"
	
	
	| IOSTREAM -> "IOSTREAM"
	| FLOW     -> "FLOW"
	| COUT     -> "COUT"
	
	
	| IDENT s   -> "IDENT \"" ^ s ^ "\""
	| STRING s  -> "STRING \"" ^ s ^ "\""
	| INTEGER s -> "INTEGER \"" ^ s ^ "\""



