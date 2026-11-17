open Ast

let rec subst x e1 e2 =
  match e2 with
  | Var y -> if x = y then e1 else e2
  | Call (f, args) -> Call (f, List.map (subst x e1) args)
  | Let (y, e, body) when x = y -> Let (y, subst x e1 e, body)
  | Let (y, e, body) -> Let (y, subst x e1 e, subst x e1 body)

(* subst x foo (+ x (let ((x (+ x 5))) (+ x x))) *)

let rec fold (exp : expr) : expr =
  match exp with
  | Prim1 (prim, a) -> fold_unary prim (fold a)
  | Prim2 (prim, a, b) -> fold_binary prim (fold a) (fold b)
  (* | If (e_cond, e_then, e_else) -> exp *)
  | Let (var, e, body) -> (
      let e = fold e in
      match e with
      | Num _ | True | False | Nil -> fold (subst var e body)
      | _ -> Let (var, e, fold body))
  | Do exps -> Do (List.map fold exps)
  | Call (f, args) -> Call (f, List.map fold args)
  | _ -> exp

and fold_unary prim a =
  match (prim, a) with
  | Add1, Num n -> Num (n + 1)
  | Sub1, Num n -> Num (n - 1)
  | _ -> Prim1 (prim, a)

and fold_binary prim a b =
  match (prim, a, b) with
  | Plus, Num a, Num b -> Num (a + b)
  | Minus, Num a, Num b -> Num (a - b)
  | _ -> Prim2 (prim, a, b)

let fold_program (program : program) : program =
  {
    defns =
      List.map (fun (d : defn) -> { d with body = fold d.body }) program.defns;
    body = fold program.body;
  }
