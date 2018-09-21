(** A beta reduction (also written β reduction) is the process of
    calculating a result from the application of a function to an expression *)

open Hw1;;

module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

let unique_name = Stream.from (fun i -> Some ("unique_name" ^ string_of_int i));;

let rec free_var_set lambda =
  match lambda with
  | Var v       -> StringSet.singleton v
  | App (x, y)  -> StringSet.union (free_var_set x) (free_var_set y);
  | Abs (x, y)  -> StringSet.remove x (free_var_set y);;

(* Вернуть список имён свободных переменных *)
let free_vars lambda = StringSet.elements (free_var_set lambda);;

let rec has_free key lambda =
  match lambda with
  | Var v                   -> v = key
  | App (x, y)              -> (has_free key x) || (has_free key y)
  | Abs (x, y) when x = key -> false
  | Abs (x, y)              -> has_free key y;;

let substitute src dest key =
  let src_free_vars = free_var_set src in
  let no_key_to_replace x = not (has_free key x) in
  let not_bounding x = not (StringSet.mem x src_free_vars) in
  let str = string_of_lambda in
  let error() = "'" ^ (str src) ^ "' is not free for substitution in '" ^
                (str dest) ^ "' instead of '" ^ key ^ "'" in
  let rec substitute_rec dest =
    match dest with
    | Var v                                     -> if (v = key) then src else dest
    | App (x, y)                                -> App (substitute_rec x, substitute_rec y);
    | Abs (x, y) when no_key_to_replace dest    -> dest
    | Abs (x, y) when not_bounding x            -> Abs (x, substitute_rec y)
    | _                                         -> failwith (error())
  in substitute_rec dest;;

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let rec free_to_subst src dest var =
  match dest with
	| Var _       -> true
	| Abs (x, y)  -> x = var || (not (List.mem var (free_vars y))) || (not (List.mem x (free_vars src)) && free_to_subst src y var)
	| App (x, y)  -> free_to_subst src x var && free_to_subst src y var;;

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let rec is_normal_form lambda =
  match lambda with
  | Var _               -> true
  | Abs (x, y)          -> is_normal_form y
  | App (Abs (x, y), t) -> not (free_to_subst t y x)
  | App (x, y)          -> is_normal_form x && is_normal_form y;;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent lambda1 lambda2 =
  let rec is_alpha_equivalent_impl lambda1 lambda2 =
    match (lambda1, lambda2) with
    | (Var v1, Var v2)              -> v1 = v2
    | (App (x1, y1), App (x2, y2))  -> (is_alpha_equivalent_impl x1 x2 && is_alpha_equivalent_impl y1 y2)
    | (Abs (x1, y1), Abs (x2, y2))  ->  let t = Var (Stream.next unique_name) in
                                        is_alpha_equivalent_impl (substitute t y1 x1) (substitute t y2 x2)
    | _                             -> false
  in is_alpha_equivalent_impl lambda1 lambda2;;

type lambda_ref = Var_ref of string
                | Abs_ref of (string * lambda_ref ref)
                | App_ref of (lambda_ref ref * lambda_ref ref);;

let rec ref_of_lambda lambda =
  match lambda with
  | Var v       -> ref (Var_ref v)
  | App (x, y)  -> ref (App_ref (ref_of_lambda x, ref_of_lambda y))
  | Abs (x, y)  -> ref (Abs_ref (x, ref_of_lambda y));;

let rec lambda_of_ref lambda_ref =
  match !lambda_ref with
  | Var_ref v       -> Var v
  | App_ref (x, y)  -> App (lambda_of_ref x, lambda_of_ref y)
  | Abs_ref (x, y)  -> Abs (x, lambda_of_ref y);;

let rec reduction_step lambda_ref =
  let mem = StringMap.mem in
  let find = StringMap.find in
  let add = StringMap.add in
  let rec to_alpha_eq lambda_ref map =
    match !lambda_ref with
    | Var_ref v         -> if mem v map then ref (Var_ref (find v map)) else lambda_ref
    | App_ref (x, y)    -> ref (App_ref (to_alpha_eq x map, to_alpha_eq y map))
    | Abs_ref (x, y)    -> let temp = Stream.next unique_name in
                           ref (Abs_ref (temp, to_alpha_eq y (add x temp map)))
  in
  let rec try_to_subst src dest key =
    match !dest with
    | Var_ref a         -> if a = key then dest := !src
    | Abs_ref (a, b)    -> if a <> key then try_to_subst src b key
    | App_ref (a, b)    -> try_to_subst src a key;
                           try_to_subst src b key
  in
  let reduction_app a b =
    match !a with
    | Abs_ref (x, y)    ->  let temp = Stream.next unique_name in
                            lambda_ref := !(to_alpha_eq y (StringMap.singleton x temp));
                            try_to_subst b lambda_ref temp;
                            Some lambda_ref
    | _                 ->  match reduction_step a with
                            | Some _    ->  Some lambda_ref
                            | None      ->  match reduction_step b with
                                            | Some _    -> Some lambda_ref
                                            | None      -> None
  in
  match !lambda_ref with
  | Var_ref a       ->  None
  | App_ref (a, b)  ->  reduction_app a b
  | Abs_ref (a, b)  ->  match reduction_step b with
                        | Some _    -> Some lambda_ref
                        | None      -> None;;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction lambda =
  match reduction_step (ref_of_lambda lambda) with
  | Some x  ->  lambda_of_ref x
  | None    ->  lambda;;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
let reduce_to_normal_form lambda =
  let rec reduction lambda_ref =
    match reduction_step lambda_ref with
    | Some x    -> reduction x
    | None      -> lambda_ref
  in
  let result = reduction (ref_of_lambda lambda) in
  lambda_of_ref result;;
