open Compile

let rec execute_instructions instructions =
  let stack = Array.make 1024 "" in
  let rec exec fp sp pc =
    ( match instructions.(pc) with 
      | Push_action action ->
	stack.(sp) <- action;
	let _ = Printf.printf "%s\n" (stack.(sp)) in
	exec fp (sp + 1) (pc + 1)
      | Push_pred predicate ->
	stack.(sp) <- predicate;
	exec fp (sp + 1) (pc + 1)
      | Pop_pred -> 
	exec fp (sp - 1) (pc + 1)
      | Goal -> ()
    )
  in exec 0 0 0

let execute_bytecode instructions = 
  execute_instructions (Array.of_list instructions) 
 
