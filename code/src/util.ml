(* util.ml *)
open Sexpr
open Ast 
open Strips
open Planner

let sprintf  = Printf.sprintf   (* to make the code cleaner *)
let spaces n = String.make n ' '

(* string from string list *)
let rec string_of_syms sym_lst = 
  (match sym_lst with
    | []   -> ""
    | [s] -> s
    | h::t -> h ^ " " ^ (string_of_syms t)
  )

(* returns a random permutation of a list *)
let rec permutation list =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h::t -> if n = 0 then (h, acc@t) else extract (h::acc) (n-1) t
  in 
  let extract_rand list len =
    extract [] (Random.int len) list
  in 
  let rec aux acc list len = 
    if len = 0 then acc else 
      let picked, rest = extract_rand list len in
      aux (picked::acc) rest (len-1)
  in aux [] list (List.length list)

(* deletes duplicates from a list *)
let rec filter_duplicates list =
  ( match list with
    | []->[]
    | h::t-> h::(filter_duplicates(List.filter(fun x -> x<>h )t))
  )
