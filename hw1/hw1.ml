type peano = Z | S of peano;; (* Типы необходимо копировать в реализацию *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x =
  match x with
  | 0 -> Z
  | i -> S (peano_of_int (i - 1));;

let rec int_of_peano p =
  match p with
  | Z -> 0
  | S x -> 1 + int_of_peano x;;

let inc x = S x;;

let rec add x y = 
  match (x, y) with
  | (x, Z) -> x
  | (Z, y) -> y
  | (x, S y) -> S (add x y);;

let rec sub x y = 
  match (x, y) with
  | (x, Z) -> x
  | (Z, y) -> Z
  | (S x, S y) -> sub x y;;

let rec mul x y =
  match (x, y) with
  | (x, Z) -> Z
  | (Z, y) -> Z
  | (x, S y) -> add x (mul x y);;

let rec power x y =
  match (x, y) with
  | (x, Z) -> S Z
  | (Z, y) -> Z
  | (x, S y) -> mul x (power x y);;

let rev x = 
  let rec rev_acc acc = function
    | [] -> acc
    | head::tail -> rev_acc (head::acc) tail
  in
  rev_acc [] x;;

let rec merge_sort x =
  let rec merge = function
    | list, []
    | [], list -> list
    | h1::t1, h2::t2 ->
        if h1 <= h2 then
          h1 :: merge (t1, h2::t2)
        else
          h2 :: merge (h1::t1, t2)
  in
  let rec halve = function
    | []
    | [_] as t1 -> (t1, [])
    | h::t ->
        let t1, t2 = halve t in
          (h::t2, t1)
  in
  match x with
  | []
  | [_] as list -> list
  | list ->
      let l1, l2 = halve list in
        merge (merge_sort l1, merge_sort l2)

let rec string_of_lambda lambda =
  match lambda with
  | Var x -> x
  | Abs (x, y) -> "\\" ^ x ^ ".(" ^ string_of_lambda y ^ ")"
  | App (x, y) -> "(" ^ string_of_lambda x ^ ") (" ^ string_of_lambda y ^ ")";;

let lambda_of_string x =
  let stream = Stream.of_string (x ^ ";") in
  let tokens = Genlex.make_lexer ["\\"; "."; "("; ")"; ";"] stream in
  let next() = Stream.next tokens in
  let peek() = Stream.peek tokens in
  let check_parenthesis() = if (next() <> Genlex.Kwd ")") then failwith "Parenthesis not closed" in
  let check_fullstop() = if (next() <> Genlex.Kwd ".") then failwith "Full stop symbol not found" in

  let rec parse_lambda() =
    match next() with
    | Genlex.Kwd "("  -> parse_parentheses()
    | Genlex.Kwd "\\" -> parse_abs()
    | Genlex.Ident v  -> parse_var v
    | _ -> failwith "Unexpected symbol"

  and parse_parentheses() =
    let lambda = parse_lambda() in
    check_parenthesis();
    check_app lambda;

  and parse_abs() =
    match next() with
    | Genlex.Ident v ->
      check_fullstop();
      let lambda = parse_lambda() in
      check_app (Abs (v, lambda));
    | _ -> failwith "Unexpected symbol"

  and parse_var v =
    check_app (Var v);

  and parse_app lambda token =
    match token with
    | Genlex.Kwd ")"  -> lambda
    | Genlex.Kwd ";"  -> lambda
    | Genlex.Kwd "\\" -> App(lambda, parse_lambda())
    | Genlex.Kwd "("  -> let _ = next() and arg = parse_lambda() in
      check_parenthesis();
      check_app (App (lambda, arg));
    | Genlex.Ident v  -> let _ = next() in check_app (App (lambda, Var v))
    | _ -> failwith "Unexpected symbol"

  and check_app lambda =
    match peek() with
    | None       -> failwith "Unexpected end of string"
    | Some token -> parse_app lambda token
  in parse_lambda();;

lambda_of_string "\\n.(\\p.p (\\x.\\y.x)) (n (\\p.\\f.f ((\\p.p (\\x.\\y.y)) p) ((\\n.\\f.\\x.f (n f x)) ((\\p.p (\\x.\\y.y)) p))) (\\f.f (\\f.\\x.x) (\\f.\\x.x)))";;
string_of_lambda (lambda_of_string "\\n.(\\p.p (\\x.\\y.x)) (n (\\p.\\f.f ((\\p.p (\\x.\\y.y)) p) ((\\n.\\f.\\x.f (n f x)) ((\\p.p (\\x.\\y.y)) p))) (\\f.f (\\f.\\x.x) (\\f.\\x.x)))");;