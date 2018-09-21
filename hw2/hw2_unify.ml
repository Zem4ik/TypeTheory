type algebraic_term =
  | Var of string
  | Fun of string * (algebraic_term list);;

let unique_name = Stream.from (fun i -> Some ("unique_name" ^ string_of_int i));;

(* По списку уравнений вернуть одно уравнение *)
let system_to_equation system =
  let new_name = Stream.next unique_name and (left, right) = List.split system in
  (Fun (new_name, left), Fun (new_name, right));;

(* Применить подстановку к уравнению *)
let apply_substitution substitution_list term =
  let rec apply_substitution_impl substitution term =
    let (key, new_term) = substitution in
    match term with
    | Var v             -> if (key = v) then new_term else term
    | Fun (name, args)  -> Fun (name, List.map (apply_substitution_impl substitution) args)
  in
  List.fold_right apply_substitution_impl substitution_list term;;

let rec equals term1 term2 = 
	match (term1, term2) with
	| (Var x, Var y)                    -> x = y
	| (Fun (f, args1), Fun (g, args2))  -> f = g && List.for_all2 equals args1 args2
	| _                                 -> false
;;

(* Проверить решение *)
let check_solution substitution_list system =
  	List.for_all 
		  (fun (l, r) -> equals (apply_substitution substitution_list l) (apply_substitution substitution_list r)) 
		  system;;

exception CannotSolve;;

(* Решить систему; если решения нет -- вернуть None *)
let solve_system system =
  let rec contains variable term =
    match term with
    | Var v         -> v = variable
    | Fun (f, args) -> List.exists (contains variable) args
  in
  let rec unify current resolved = 
	match current with
	| [] -> 
		List.map 
			(fun (l, r) -> 
				match (l, r) with 
				| (Var x, _)  -> (x, r) 
				| _           -> failwith "Something went wrong")
      resolved
	| (l, r) :: tail -> 
		if equals l r 
      then
        unify tail resolved 
      else
        match (l, r) with
        | (Var x, _) ->
          if contains x r then raise CannotSolve else
          let mapping = fun (a, b)    -> (apply_substitution [(x, r)] a, apply_substitution [(x, r)] b) 
          in
          unify (List.map mapping current) ((l, r) :: (List.map mapping resolved))
        | (Fun (_, _), Var _)         -> unify ((r, l) :: tail) resolved
        | (Fun (f, a1), Fun (g, a2))  -> 
          if f = g then 
            (try let combined = List.combine a1 a2 in unify (combined @ tail) resolved
            with Invalid_argument msg -> raise CannotSolve)
          else raise CannotSolve
  in
  try Some (unify system []) with CannotSolve -> None;;
