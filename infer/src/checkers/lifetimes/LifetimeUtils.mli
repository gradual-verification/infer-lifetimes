open! IStd

val unfold_access_exp : HilExp.access_expression -> (HilExp.access_expression list)

val find_inner_access_exp: HilExp.t -> HilExp.access_expression option