
exception Empty_env

module Local = Map.Make(String)
type 'a t = Root | Bloc of 'a Local.t * 'a t

let empty = Bloc (Local.empty, Root)

let rec is_empty = function
	| Root -> true
	| Bloc (local, parent) ->
		Local.is_empty local && (is_empty parent)

let push local env = Bloc (local, env)

let push_empty env = push Local.empty env

let pop = function
	| Root -> raise Empty_env
	| Bloc (local, parent) -> local, parent

let add key v = function
	| Root -> raise Empty_env
	| Bloc (local, parent) ->
		let local' = Local.add key v local in
			Bloc (local', parent)

let rec get_globals = function
	| Root -> raise Empty_env
	| Bloc (local, parent) ->
		try get_globals parent
		with Empty_env -> local

let rec mem key = function
	| Root -> false
	| Bloc (local, parent) ->
		Local.mem key local || mem key parent

let mem_local key = function
	| Root -> false
	| Bloc (local, _) -> Local.mem key local

let rec find_and_localize key = function
	| Root -> raise Not_found
	| Bloc (local, parent) ->
		try Local.find key local, 0
		with Not_found ->
			let v, n = find_and_localize key parent in
				v, n + 1

let find key env =
	let v, n = find_and_localize key env in v

let find_local key = function
	| Root -> raise Not_found
	| Bloc (local, _) -> Local.find key local

let rec stack_map f = function
	| Root -> Root
	| Bloc (local, parent) ->
		Bloc (f local, stack_map f parent)

let map f = stack_map (Local.map f)

let filter f = stack_map (Local.filter f)

let of_bool_env env =
	let env' = filter (fun _ -> snd) env in
		map fst env'


let decl env = map (fun (t, _) -> (t, true)) env

let print_local print_node ff local =
	Local.iter (fun k v -> Printf.fprintf ff "%a ; " print_node (k, v)) local

let rec print print_node ff = function
	| Root -> Printf.fprintf ff " |"
	| Bloc (local, parent) ->
		Printf.fprintf ff "[%a] -- %a"
			(print_local print_node) local
			(print print_node) parent


