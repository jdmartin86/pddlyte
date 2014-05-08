(* compile.ml *)

(* 
   - push state preds on stack until goal is satisfied
   - if all preds are on stack, pop all off and push action
   - push state preds on stack until goal is satisfied
   - if all preds are on stack, pop all off and push next action
*)

type instruction =
  | Push_action of string
  | Push_pred of string
  | Pop_pred
  | Goal

let sprintf = Printf.sprintf 

let string_of_instruction instruction =
( match instruction with 
  | Push_action action -> sprintf "Push_action %s" action
  | Push_pred pred -> sprintf "Push_pred %s" pred
  | Pop_pred -> "Pop_pred"
  | Goal -> "Goal"
)


let print_bytecode instructions = 
  (string_of_syms List.map string_of_instruction instructions )

let rec string_of_syms sym_lst = 
  (match sym_lst with
    | []   -> ""
    | [s] -> s
    | h::t -> h ^ " " ^ (string_of_syms t)
  )

let string_of_atom atom =
  ( match atom with 
    | Atom_var a | Atom_gnd a -> a
    | Atom_nil -> ""
  )

(* check if name is a known action *)
let rec known_action name opset =
  ( match opset with
    | [] -> false
    | op::remaining_ops -> 
      if op.name = name then true
      else known_action name remaining_ops
  )

let rec translate_params params =
    sprintf "%s"
      (string_of_syms (List.map string_of_atom params))

let translate_action pred =
  ( match pred with
    | Pred_gnd( name , params ) ->
      let translated_params = translate_params params in
      let action = sprintf "%s( %s )" name translated_params in
      Push_action( action )
    | _ -> 
      let error_msg = "error translating predicate" in
      failwith error_msg
  )

let translate_pred pred = 
   ( match pred with 
    | Pred_gnd( name , params ) ->
      let predicate = 
	sprintf " %s( %s )" 
	  name 
	  (string_of_syms(List.map string_of_atom params)) 
      in 
      Push_pred predicate
      
    | _ -> 
      let error_msg = "error translating predicate" in
      failwith error_msg
  )

let pop_for num = 
  let rec loop acc count =
    if 0 < count then
      loop (Pop_pred::acc) (count - 1)
    else
      acc
  in loop [] num

(* translate plan into bytecode *)
let translate plan problem =
  let { init = s0 ; goal = g ; ops = opset } = problem in
  let num_preds = List.length s0 in
  let rec recurse instructions preds =
    ( match preds with
      | [] -> instructions
      | pred::remaining_preds ->
	( match pred with
	  | Pred_gnd( name , params ) ->
	    if known_action name opset then
	      let pred_pops = pop_for num_preds in
	      let translated_action = translate_action pred in
	      let transition = translated_action::pred_pops in
	      recurse (transition@instructions) remaining_preds
	    else
	      ( match remaining_preds with
		| [] -> recurse (Goal::instructions) remaining_preds
		| _ -> 
		  let translated_pred = translate_pred pred in
		  recurse (translated_pred::instructions) remaining_preds
	      )
	  | _ ->
	    let error_msg = "error translating predicate" in
	    failwith error_msg
	)
    )
  in recurse [] plan
