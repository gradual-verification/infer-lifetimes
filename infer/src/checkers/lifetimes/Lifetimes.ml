
  (*
    let generate_constraints (formal:(Mangled.t * Typ.t * Annot.Item.t)) (postmap:MayPointsToMap.t) =
    let ltvars = Variable.variables_of_formal formal in
    let base_loc = AbstractLocation.create_loc_for_formal formal in 
    let create_constraints ltroot set = (let ltvar_lst = AbstractLocationSet.to_ltvar_list set in
      List.fold ltvar_lst ~init: "" ~f:(fun prev ltv -> prev^" "^(string_of ltv)^" <: "^(string_of ltroot))) in
    let rec get_constraints map loc_set vars = 
      (match vars with
    | hd :: tl -> let str_of_curr_ltv = string_of hd in
      let generated_constraints_at_level = str_of_curr_ltv^": "^create_constraints hd loc_set in
      generated_constraints_at_level ^ "\n" ^ (get_constraints map (MayPointsToMap.dereference map loc_set) tl)
    | [] ->  "") in
    let base_pointing_to = MayPointsToMap.get_pointing_to postmap base_loc in
    get_constraints postmap base_pointing_to ltvars
  *)
