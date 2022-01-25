type name = string;;

type o =
	| Time_of
	| Most_recent
	| Message_type_is
	| Message_payload
	| Make_hold_message
	| Time_passed
	| Make_restart_message
	| Make_start_message
	| Print
;;

type e =
	| Add of name * e
	| Remove of name
	| Send of e
	| List of e list
	| Native of o * (e list)
	| Lambda of (name list) * e
	| Variable of name
	| Message of name * (e list)
	| Log of (name * (e list)) list
	| Begin of e * e
	| If of e * e * e
	| Void
;;

type v = 
	| Float of float
	| String of string
;;

type context = (name * v) list;;

(* Maps a context name to their name *)
let context_to_name_list context = 
	List.map (fun (a,b) -> a) context;;


(* Adds the (name,e) handler to the given handler_list *)
let add_handler name e handler_list =
	let rec browse l = match l with
		| [] -> (name,e)::handler_list
		| (n,_)::q -> if n = name then handler_list else browse q in
	browse handler_list;;

(* Adds the (name,e) handler to the given handler_list *)
let remove_handler name handler_list =
	let rec browse l = match l with
		| [] -> l
		| (n,e)::q -> if n = name then browse q else (n,e)::(browse q) in
	browse handler_list;;


(* Update the given couple (name,value) in the given context *)
let update_value (name,v) context =
	let rec aux l acc = match l with
		| [] -> List.rev acc
		| (n,v2)::q -> if name = n then aux q ((name,v)::acc) else aux q ((name,v2)::acc)
	in aux context [];;


(* Applies the values e to the variables names in the given evaluation_context *)
let apply_values variables e evaluation_context =
	let rec aux zipped context = match zipped with
		| [] -> context
		| (name,v)::q -> 
				if not(List.mem name (context_to_name_list context)) then 
					aux q ((name,v)::context) (* If var doesn't exist, we create it *)
				else
					aux q (update_value (name,v) context) (* Else we update it *)
	in aux (List.combine variables e) evaluation_context;;



(* Evaluates one handler, and returns the final set of handler H and the log *) 
let eval_handler handler handler_list log evaluation_context = 
	let rec browse l handler_list log evaluation_context = match l with
		| [] | [Void] -> (handler_list,log)
		| a::q -> match a with
			| Send(e) -> browse (Void::q) handler_list(e::log) evaluation_context
			| Add(name,e) -> browse (Void::q) (add_handler name e handler_list) log evaluation_context
			| Remove(name) -> browse (Void::q) (remove_handler (name) handler_list) log evaluation_context
			| Lambda(variables,e::b) -> browse (Void::q) handler_list log (apply_values variables (e::b) evaluation_context)
			in
	let (name,e) = handler in
	browse e handler_list log;;

(* Debug *)
let handler_list_test = [("h0",Void);("h1",Void);("h2",Void)];;
remove_handler "h1" handler_list_test ;;
add_handler "h1" Void handler_list_test ;;
(* Debug *)


		

