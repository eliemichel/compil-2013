%{
	open Ast
%}

%token CLASS ELSE FALSE FOR IF INT NEW NULL
%token PUBLIC RETURN THIS TRUE VIRTUAL VOID WHILE
%token LBRACE RBRACE LPAR RPAR EQ OR AND DBLEQ NEQ LT
%token LEQ GT GEQ PLUS MINUS STAR DIV MOD NOT INCR DECR AMP
%token ARROW DOT SEMCOL COL COMMA IOSTREAM COUT FLOW
%token EOF
%token <string> IDENT TIDENT INTEGER STRING

%right EQ
%left OR
%left AND
%left NEQ DBLEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left STAR DIV MOD
%right NOT INCR DECR AMP
%left DOT ARROW LPAR

%nonassoc IFX
%nonassoc ELSE


%start <Ast.pAst> program

%%

position(X):
	x = X { { node = x ; start_pos = $startpos ; end_pos = $endpos}}

program:
	io = IOSTREAM?
	decls = decl*
	EOF
	{{
		decls = decls;
		iostream = io <> None
	}}


decl:
	| d = decl_vars        { Decl_vars  d      }
	| d = decl_class       { Decl_class d      }
	| p = proto b = bloc   { Proto_bloc (p, b) }


decl_vars:
	t = type_ l = separated_nonempty_list(COMMA, var) SEMCOL { (t, l) }


var:
	| id = position(IDENT)    { Ident          id }
	| STAR v = var  { Pointer_value   v }
	| AMP  v = var  { Address         v }


qvar:
	| id = qident    { QIdent_in_qvar  id }
	| STAR v = qvar  { QPointer_value   v }
	| AMP  v = qvar  { QAddress         v }


type_:
	|      VOID   { Void      }
	|      INT    { Int       }
	| id = TIDENT { TIdent id }


decl_class:
	id = start_class
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


start_class:
	CLASS
	id = IDENT
	{
		if Hashtbl.mem tidentTbl id
		then raise Error
		else Hashtbl.add tidentTbl id ();
		id
	}


supers:
	COL l = separated_nonempty_list(COMMA, preceded (PUBLIC, TIDENT)) { l }


member:
	| d = decl_vars          { Decl_vars_in_member d                         }
	| v = ioption(VIRTUAL) p = proto SEMCOL
	                         { if v = None then Proto p else Virtual_proto p }


proto:
	start = start_proto
	LPAR args = separated_list(COMMA, argument) RPAR
	{{
		start = start;
		args  = args
	}}


argument:
	t = type_ v = var      { (t, v) }


start_proto:
	| t = type_ v = qvar              { Function    (t, v)   }
	| id = position(TIDENT)           { Constructor id       }
	| ns = TIDENT COL COL id = TIDENT { Method      (ns, id) }


bloc:
	LBRACE l = instruction* RBRACE   { l }


instruction:
	| SEMCOL                        { Empty        }
	| e = expr SEMCOL               { Expression e }
	| t = type_ var = var value = var_val? SEMCOL
	                                { Var_init (t, var, value)   }
	| IF LPAR e = expr RPAR instr = instruction                        %prec IFX
	                                { If_else (e, instr, Empty)  }
	| IF LPAR e = expr RPAR instr = instruction ELSE instr2 = instruction
	                                { If_else (e, instr, instr2) }
	| WHILE LPAR e = expr RPAR instr = instruction
	                                { For ([], e, [], instr)     }
	| FOR
		LPAR
			li = separated_list(COMMA, expr) SEMCOL
			e = expr? SEMCOL
			la = separated_list(COMMA, expr)
		RPAR instr = instruction
		{
			let e = match e with
				| None   -> {
					node = Integer "1";
					start_pos = $startpos;
					end_pos = $endpos
					}
				| Some c -> c
			in
				For (li, e, la, instr)
		}
	| b = bloc                           { Bloc b   }
	| COUT FLOW l = separated_nonempty_list(FLOW, expr_flow) SEMCOL
	                                     { Cout l   }
	| RETURN e = position(expr?) SEMCOL  { Return e }


expr_flow:
	| e = expr   { Expression_in_flow e }
	| s = STRING { String_in_flow     s }


var_val:
	| EQ e = expr   { Value e           }
	| EQ id = TIDENT LPAR le = separated_list(COMMA, expr) RPAR
	                { Returned (id, le) }

expr:
	e = position(expr_node) {e}

expr_node:
	| THIS                                         { This                     }
	| TRUE                                         { Integer "1"              }
	| FALSE                                        { Integer "0"              }
	| NULL                                         { Null                     }
	| n = INTEGER                                  { Integer n                }
	| id = qident                                  { QIdent id                }
	| e = expr DOT   id = IDENT                    { Dot (e, id)              }
	| e = expr ARROW id = IDENT                    {
		Dot (
			{
				node = Unop (Star, e);
				start_pos = $startpos;
				end_pos = $endpos;
			},
			id
			)
	                                                                          }
	| e1 = expr EQ e2 = expr                       { Eq    (e1, e2)           }
	| f = expr LPAR le = separated_list(COMMA, expr) RPAR
	                                               { Application (f, le)      }
	| NEW id = TIDENT LPAR le = separated_list(COMMA, expr) RPAR
	                                               { New  (id, le)            }
	| INCR e = expr                                { Unop (IncrLeft,  e)      }
	| DECR e = expr                                { Unop (DecrLeft,  e)      }
	| e = expr INCR                                { Unop (IncrRight, e)      }
	| e = expr DECR                                { Unop (DecrRight, e)      }
	| AMP   e = expr                               { Unop (Amp,       e)      }
	| NOT   e = expr                               { Unop (Not,       e)      }
	| MINUS e = expr                               { Unop (Minus,     e)      }
	| PLUS  e = expr                               { Unop (Plus,      e)      }
	| STAR  e = expr                               { Unop (Star,      e)      }
	| e1 = expr op = operateur e2 = expr           { Binop (op, e1, e2)       }
	| LPAR e = expr RPAR                           { e.node                   }


qident:
	| id = position(IDENT)      { Simple_qident    id       }
	| ns = position(TIDENT) COL COL id = position(IDENT)
	                            { Namespace_qident (ns, id) }


%inline operateur:
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



