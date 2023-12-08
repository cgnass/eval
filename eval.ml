open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
    [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
    [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)

let rec eval_expr env e = match e with 
  | Value e -> 
      let v = match e with 
        | Int e -> Int e
        | Bool e -> Bool e
        | String e -> String e
        | Closure (a, b, c) -> Closure (a, b, c)
      in
      v
  | ID e -> ref_lookup env e  
  | Not e -> 
      let v = eval_expr env e in 
      let v = match v with 
        | Bool v -> not v
        | _ -> raise (TypeError "bool")
      in 
      Bool v
  | Binop (op, e1, e2) -> 
      let e1 = eval_expr env e1 in
      let e2 = eval_expr env e2 in
      let v = match e1 with 
        | Int e1 -> 
            let e2 = match e2 with 
              | Int e2 -> e2
              | _ -> raise (TypeError "e2Int")
            in 
            let v = match op with 
              | Add -> Int (e1 + e2)
              | Sub -> Int (e1 - e2)
              | Mult -> Int (e1 * e2)
              | Div -> 
                  if e2 = 0 then 
                    raise (DivByZeroError)
                  else 
                    Int (e1 / e2)
              | Greater -> Bool (e1 > e2)
              | Less -> Bool (e1 < e2)
              | GreaterEqual -> Bool (e1 >= e2)
              | LessEqual -> Bool (e1 <= e2)
              | Equal -> Bool (e1 = e2)
              | NotEqual -> Bool (e1 <> e2)
              | _ -> raise (TypeError "inttypeerror")
            in
            v
        | String e1 -> 
            let e2 = match e2 with 
              | String e2 -> e2 
              | _ -> raise (TypeError "e2String")
            in
            let v = match op with 
              | Concat -> String (e1 ^ e2)
              | Equal -> Bool (e1 = e2)
              | NotEqual -> Bool (e1 <> e2)
              | _ -> raise (TypeError "stringop")
            in
            v
        | Bool e1 ->
            let e2 = match e2 with 
              | Bool e2 -> e2
              | _ -> raise (TypeError "booler")
            in
            let v = match op with 
              | Equal -> Bool (e1 = e2) 
              | NotEqual -> Bool (e1 <> e2) 
              | And -> Bool (e1 && e2)
              | Or -> Bool (e1 || e2)
              | _ -> raise (TypeError "booltypeerror")
            in
            v
        | _ -> raise (TypeError "nogoodtype")
      in
      v
  | If (e1, e2, e3) -> 
      let e1 = eval_expr env e1 in
      let e1 = match e1 with 
        | Bool e1 -> e1
        | _ -> raise (TypeError "iftyper")
      in
      if e1 = true then 
        eval_expr env e2
      else 
        eval_expr env e3
  | Let (var, b, e1, e2) -> let out = match b with 
      | false ->
          let v = eval_expr env e1 in
          let env2 = ref_extend env var v in
          let v = eval_expr env2 e2 in
          v
      | true -> 
          let env2 = ref_extend env var (Int(0)) in 
          let v = eval_expr env2 e1 in 
          let () = ref_update env2 var v in 
          let c = eval_expr env2 e2 in 
          c
      in 
      out 
  | Fun (var, e) -> Closure (env, var, e) 
  | FunctionCall (e1, e2) ->
      let e1 = eval_expr env e1 in 
      let e2 = eval_expr env e2 in
      let v = match e1 with 
        | Closure (env, var, expr) -> 
            let env2 = ref_extend env var e2 in 
            let out = eval_expr env2 expr in 
            out 
        | _ -> raise (TypeError " asdflk")
      in v 

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
  | Def (var, e) -> 
      let temp = ref_extend env var (Int(0)) in 
      let out = eval_expr temp e in 
      let () = ref_update temp var out in 
      (temp, Some (out))
  | Expr e ->
      (env, Some(eval_expr env e))
  | NoOp ->
      ([], None)
