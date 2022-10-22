open! IStd

let unfold_access_exp (aex : HilExp.access_expression) =
  let rec unfold_access_exp_rec (aex : HilExp.access_expression)
      (derefs : HilExp.access_expression list) (addrofs : HilExp.access_expression list) :
      HilExp.access_expression list =
    match aex with
    | Base _ ->
        derefs @ addrofs @[aex]
    | FieldOffset (inner, _) ->
        (derefs @ addrofs) @ [aex] @ unfold_access_exp_rec inner [] []
    | AddressOf inner ->
        if List.length derefs > 0 then
          unfold_access_exp_rec inner (List.drop_last_exn derefs) addrofs
        else unfold_access_exp_rec inner derefs (addrofs @ [aex])
    | Dereference inner ->
        if List.length addrofs > 0 then
          unfold_access_exp_rec inner derefs (List.drop_last_exn addrofs)
        else unfold_access_exp_rec inner (derefs @ [aex]) addrofs
    | ArrayOffset (inner, _, _) ->
        if List.length addrofs > 0 then
          unfold_access_exp_rec inner derefs (List.drop_last_exn addrofs)
        else unfold_access_exp_rec inner (derefs @ [aex]) addrofs
  in
  unfold_access_exp_rec aex [] []


let rec find_inner_access_exp (ex : HilExp.t):HilExp.access_expression option=
  match ex with
  | HilExp.AccessExpression acc ->
      Some acc
  | HilExp.UnaryOperator (_, inner, _) ->
      find_inner_access_exp inner
  | HilExp.BinaryOperator (op, l, _) -> (
    match op with Binop.MinusPI | Binop.PlusPI -> find_inner_access_exp l | _ -> None )
  | HilExp.Cast (_, inner) ->
      find_inner_access_exp inner
  | _ ->
      None