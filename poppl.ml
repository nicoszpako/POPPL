module Unix = UnixLabels;;
type name = string;;

type o =
	| Time_of (* log -> time of latest message *)
	| Most_recent (* (name, ?limit) -> message*)
	| Message_type_is (* (message, name) -> Float *)
	| Message_payload (* message -> e *)
	| Time_passed (* stamp_start, stamp_end, delay -> Float *)
	| Make_restart_message
	| Print
	| Is_more_than
	| Is_less_than
	| Is_equal_to
	| Is_in
;;

type e =
	| Add of name * (e -> e)
	| Remove of name
	| Send of e
	| Lambda of (e -> e)
	| List of e list
	| Native of o * (e list)
	| Variable of name
	| Message of name * (e list) (* A message is an identifier and data as a list of expressions *)
	| Log of (name * (e list)) list
	| Begin of e
	| If of e * e * e
	| Float of float
	| String of string
	| Void
;;

type handler = Handler of name * (e -> e);;
type actor = handler list;;
type context = (name * e) list;;

(* --- Context handling --- *)

(* Returns the list of current registered variables *)
let context_to_name_list context = 
	List.map (fun (a,b) -> a) context;;

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

(* --- Handlers handling --- *)

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

(* Evaluates one handler, and returns the final set of handler H and the log *) 
let eval_handler handler handler_list log evaluation_context = 
	let rec browse l handler_list log evaluation_context = match l with
		| [] | [Void] -> (handler_list,log)
		| a::q -> match a with
			| Send(e) -> browse (Void::q) handler_list(e::log) evaluation_context
			| Add(name,e) -> browse (Void::q) (add_handler name e handler_list) log evaluation_context
			| Remove(name) -> browse (Void::q) (remove_handler (name) handler_list) log evaluation_context
			in
	let (name,e) = handler in
	browse e handler_list log;;

(* --- Log handling --- *)

(* Retrieve the newest time message to return the actual system time *)
let now log = match log with
	| Log((a,time)::q) -> if a = "time_message" then time else now Log(q)
;;

(* Returns the number of registered messages *)
let log_length log = match log with
	| Log(l) -> List.length l
;;

(* --- Syntax shortcuts --- *)

(* Whenever a message is received *)
let whenever_message_type s body = If(Native(Message_type_is,[String(s)]),body,Void);; 

(* Log query *)
let whenever_last_messages_in s a b body = If(Native(Message_type_is,[String(s)]),body,Void);; 

(* After instruction *)
let after time body log =
	let n = "after_"^(string_of_int (log_length log)) in 
	Add(n,
		fun log -> If(
			Native(Time_passed,[Native(Time_of,[Native(Most_recent)]);now log;time]),
			Begin(List([body; Remove(n)])),
			Void
		)
	)
;;


(* --- Debug --- *)

let initially = 
	Handler("initially", fun log ->
		Begin(
			List([
				Send(Message("giveBolus",[Float(80.);String("HEParin");String("iv")]));
				Send(Message("start",[Float(3.);String("HEParin")]))
			])
		)
	)
;;

let infusion = 
	Handler("infusion", fun log -> (whenever_message_type "aPTTResult" 
		(Begin(
			List([
				If(
					Native(Is_less_than,[Variable("aPTT");Float(45.)]),
					Begin(
						List([
							Send(Message("giveBolus",[Float(80.);String("HEParin");String("iv")]));
							Send(Message("increase",[Float(3.);String("HEParin")]));
						])
					),
					Void
				);
				If(
					Native(Is_in,[Variable("aPTT");Float(45.);Float(59.)]),
					Begin(
						List([
							Send(Message("giveBolus",[Float(40.);String("HEParin");String("iv")]));
							Send(Message("increase",[Float(1.);String("HEParin")]));
						])
					),
					Void
				);
				If(
					Native(Is_in,[Variable("aPTT");Float(101.);Float(123.)]),
					Begin(
						List([
							Send(Message("decrease",[Float(1.);String("HEParin")]));
						])
					),
					Void
				);
				If(
					Native(Is_more_than,[Variable("aPTT");Float(123.)]),
					Begin(
						List([
							Send(Message("hold",[String("HEParin")]));
							after (Float(1.)) (Begin(
								List([
									Send(Message("restart",[String("HEParin")]));
									Send(Message("decrease",[Float(3.);String("HEParin")]));
								])
							)) log
						])
					),
					Void
				)
			])
		))
	)
)
;;

let apttchecking = 


(* Debug *)

let run =
	while true do
		Unix.sleep 1;
		
	done;;
run;;

		

