%{
	open Ast
%}

%token CLASS ELSE FALSE FOR IF INT NEW NULL
%token PUBLIC RETURN THIS TRUE VIRTUAL VOID WHILE
%token LBRACE RBRACE LPAR RPAR EQ OR AND DBLEQ NEQ LT
%token LEQ GT GEQ PLUS MINUS STAR DIV MOD NOT INCR DECR AMP
%token ARROW DOT SEMCOL COL COMMA IOSTREAM COUT FLOW
%token EOF
%token <string> IDENT INTEGER STRING


%nonassoc NEQ LT LEQ GT GEQ EQ DBLEQ LPAR
%left PLUS MINUS DIV MOD OR AND DOT ARROW
%left STAR INCR DECR

%start <Ast.ast> program

%%

program:
	io = IOSTREAM?
	decls = decl*
	EOF
	{ Prog {
		decls = decls;
		iostream = io <> None
	}}

decl:
	| d = decl_vars        { Decl_vars  d      }
	| d = decl_class       { Decl_class d      }
	| p = proto b = bloc   { Proto_bloc (p, b) }

decl_vars:
	t = type_ v = var l = more_vars* SEMCOL { (t, v :: l) }

more_vars:
	COMMA v = var { v }

var:
	| id = IDENT    { Ident          id }
	| STAR v = var  { Pointer_value   v }
	| AMP  v = var  { Address         v }

qvar:
	| id = IDENT     { Ident_in_qvar   id }
	| STAR v = qvar  { QPointer_value   v }
	| AMP  v = qvar  { QAddress         v }


type_:
	|      VOID   { Void      }
	|      INT    { Int       }
	| id = IDENT  { TIdent id }


decl_class:
	CLASS
	id = IDENT
	s = supers?
	LBRACE PUBLIC COL m = member* RBRACE SEMCOL
	{
		let s = match s with
			| None   -> []
			| Some l -> l
		in {
			name    = id;
			supers  = s ;
			members = m
		}
	}


supers:
	COL PUBLIC id = IDENT l = more_supers* { id :: l }

more_supers:
	COMMA PUBLIC id = IDENT                { id }

member:
	| d = decl_vars          { Decl_vars_in_member d }
	| v = VIRTUAL? p = proto { if v = None then Proto p else Virtual_proto p }

proto:
	start = start_proto
	arg = argument larg = more_arguments*
	{{
		start = start;
		args  = arg :: larg
	}}

more_arguments:
	COMMA arg = argument   { arg }

argument:
	t = type_ v = var      { (t, v) }


start_proto:
	| t = type_ v = qvar             { Typed       (t, v)   }
	| id = IDENT                     { Constructor id       }
	| ns = IDENT COL COL id = IDENT  { Method      (ns, id) }



bloc:
	LBRACE l = instruction* RBRACE   { l }



instruction:
	| SEMCOL                        { Empty        }
	| e = expr SEMCOL               { Expression e }
	| t = type_ var = var value = var_val? SEMCOL
	                                { Var_init (t, var, value) }
	| IF LPAR e = expr RPAR instr = instruction
	                                { If (e, instr)              }
	| IF LPAR e = expr RPAR instr = instruction ELSE instr2 = instruction
	                                { If_else (e, instr, instr2) }
	| WHILE LPAR e = expr RPAR instr = instruction
	                                { While (e, instr)           }
	| FOR
		LPAR
			i = expr li = more_expr* SEMCOL
			e = expr? SEMCOL
			a = expr la = more_expr*
		RPAR instr = instruction
		{
			let e = match e with
				| None   -> True
				| Some c -> c
			in
			For (i :: li, e, a :: la, instr)
		}
	| b = bloc                      { Bloc b   }
	| COUT l = flow_expr+ SEMCOL    { Cout l   }
	| RETURN e = expr? SEMCOL       { Return e }


more_expr:
	COMMA e = expr { e }

flow_expr:
	FLOW e = expr_str { e }

expr_str:
	| e = expr   { Expression_in_flow e }
	| s = STRING { String_in_flow     s }

var_val:
	| EQ e = expr   { Value e                }
	| EQ id = IDENT LPAR e = expr le = more_expr* RPAR
	                { Returned (id, e :: le) }


expr:
	| THIS                                         { This                     }
	| TRUE                                         { True                     }
	| FALSE                                        { False                    }
	| NULL                                         { Null                     }
	| n = INTEGER                                  { Integer n                }
	| id = qident                                  { QIdent id                }
	| e = expr DOT   id = IDENT                    { Dot   (e, id)            }
	| e = expr ARROW id = IDENT                    { Arrow (e, id)            }
	| e1 = expr EQ e2 = expr                       { Eq    (e1, e2)           }
	| f = expr LPAR e = expr le = more_expr* RPAR  { Application (f, e :: le) }
	| NEW id = IDENT e = expr le = more_expr*      { New  (id, e :: le)       }
	| INCR e = expr                                { Unop (IncrLeft,  e)      }
	| DECR e = expr                                { Unop (DecrLeft,  e)      }
	| e = expr INCR                                { Unop (IncrRight, e)      }
	| e = expr DECR                                { Unop (DecrRight, e)      }
	| AMP e = expr                                 { Unop (Amp,       e)      }
	| NOT e = expr                                 { Unop (Not,       e)      }
	| MINUS e = expr                               { Unop (Minus,     e)      }
	| PLUS e = expr                                { Unop (Plus,      e)      }
	| STAR e = expr                                { Unop (Star,      e)      }
	| e1 = expr op = operateur e2 = expr           { Binop (op, e1, e2)       }
	| LPAR e = expr RPAR                           { e                        }



qident:
	| id = IDENT                    { Simple_qident    id       }
	| ns = IDENT COL COL id = IDENT { Namespace_qident (ns, id) }


operateur:
	| DBLEQ { Dbleq }
	| NEQ   { Neq   }
	| LT    { Lt    }
	| LEQ   { Leq   }
	| GT    { Gt    }
	| GEQ   { Geq   }
	| PLUS  { Add   }
	| MINUS { Sub   }
	| STAR  { Mult  }
	| DIV   { Div   }
	| MOD   { Mod   }
	| AND   { And   }
	| OR    { Or    }


