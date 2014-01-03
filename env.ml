
exception Empty_env

module Local = Map.Make(String)
type 'a t = EnvRoot | EnvBloc of 'a Local.t * 'a t

let empty = EnvBloc (Local.empty, EnvRoot)

let rec is_empty = function
	| EnvRoot -> true
	| EnvBloc (local, parent) ->
		Local.is_empty local && (is_empty parent)

let push local env = EnvBloc (local, env)

let push_empty env = push Local.empty env

let pop = function
	| EnvRoot -> raise Empty_env
	| EnvBloc (local, parent) -> local, parent

let add key v = function
	| EnvRoot -> raise Empty_env
	| EnvBloc (local, parent) ->
		let local' = Local.add key v local in
			EnvBloc (local', parent)

let rec mem key = function
	| EnvRoot -> false
	| EnvBloc (local, parent) ->
		Local.mem key local || mem key parent

let mem_local key = function
	| EnvRoot -> false
	| EnvBloc (local, _) -> Local.mem key local

let rec find_and_localize key = function
	| EnvRoot -> raise Not_found
	| EnvBloc (local, parent) ->
		try Local.find key local, 0
		with Not_found ->
			let v, n = find_and_localize key parent in
				v, n + 1

let find key env =
	let v, n = find_and_localize key env in v

let find_local key = function
	| EnvRoot -> raise Not_found
	| EnvBloc (local, _) -> Local.find key local

let rec map f = function
	| EnvRoot -> EnvRoot
	| EnvBloc (local, parent) ->
		EnvBloc (Local.map f local, map f parent)

