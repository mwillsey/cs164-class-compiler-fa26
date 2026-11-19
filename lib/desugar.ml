open Ast
open Compile

let rec desugar_expr (defns : defn list ref) (e : expr) : expr =
  let go = desugar_expr defns in
  match e with
  | Num n -> Num n
  | Var x -> Var x
  | True -> True
  | False -> False
  | Nil -> Nil
  | Prim0 p -> Prim0 p
  | Prim1 (p, e) -> Prim1 (p, go e)
  | Prim2 (p, e1, e2) -> Prim2 (p, go e1, go e2)
  | Let (x, e, body) -> Let (x, go e, go body)
  | If (c, t, e) -> If (go c, go t, go e)
  | Do exps -> Do (List.map go exps)
  | Call (f, args) -> Call (go f, List.map go args)
  | Lambda (args, body) ->
      let name = gensym "_lambda" in
      let defn = { name; args; body = go body } in
      defns := defn :: !defns;
      Var name

let desugar (program : program) : program =
  let new_defns = ref [] in
  let defns =
    List.map
      (fun (d : defn) -> { d with body = desugar_expr new_defns d.body })
      program.defns
  in
  let body = desugar_expr new_defns program.body in
  { defns = defns @ !new_defns; body }
