open Syntax
open Name
open Free_variables

let rec substitution ~from_ ~to_ ~in_ =
  let (LE { loc; desc = expr_in }) = in_ in

  let aux expr = substitution ~from_ ~to_ ~in_:expr in
  let pack desc = LE { loc; desc } in

  match expr_in with
  | LE_var { var } when var = from_ -> to_
  | LE_nat -> in_
  | LE_zero -> in_
  | LE_var _ -> in_
  | LE_type _ -> in_
  | LE_lambda { param; _ } when param = from_ -> in_
  | LE_lambda { param; arg; body} when NameSet.mem param (free_vars to_) ->
      let avoid = (NameSet.singleton from_) <@> (free_vars in_) <@> (free_vars to_) in
      let name = fresh ~avoid ~name:param in
      let body' = substitution ~from_:param ~to_:(pack (LE_var { var = name })) ~in_:body in
      pack (LE_lambda { param = name; arg; body = aux body' })
  | LE_pi { param; _ } when param = from_ -> in_
  | LE_pi { param; anno; arg; body } when NameSet.mem param (free_vars to_) ->
      let avoid = ((NameSet.singleton from_) <@> (free_vars in_) <@> (free_vars to_)) in
      let anno' = aux anno in
      let name = fresh ~avoid ~name:param in
      let body' = substitution ~from_:param ~to_:(pack (LE_var { var = name })) ~in_:body in
      pack (LE_pi { param = name; anno = anno'; arg; body = aux body'; })
  | LE_lambda { param; arg; body} -> 
    pack (LE_lambda { param; arg; body = aux body })
  | LE_pi { param; anno; arg; body } -> 
    pack (LE_pi { param; anno = aux anno; arg; body = aux body })
  | LE_app { lambda; arg } -> 
    pack (LE_app { lambda = aux lambda; arg = aux arg })
  | LE_pair { left; right } -> 
    pack (LE_pair { left = aux left; right = aux right })
  | LE_fst { expr } -> 
    pack (LE_fst { expr = aux expr })
  | LE_snd { expr } -> 
    pack (LE_snd { expr = aux expr })
  | LE_inl { expr } -> 
    pack (LE_inl { expr = aux expr })
  | LE_inr { expr } -> 
    pack (LE_inr { expr = aux expr })
  | LE_succ { expr } -> 
    pack (LE_succ { expr = aux expr })
  | LE_sum { head; tail } -> 
    pack (LE_sum { head = aux head; tail = aux tail })
  | LE_refl { left; right } -> 
    pack (LE_refl { left = aux left; right = aux right })
  | LE_propEq { left; right } -> 
    pack (LE_propEq { left = aux left; right = aux right })
  | LE_eqElim { pred; a; b; c } -> 
    pack (LE_eqElim { pred = aux pred; a = aux a; b = aux b; c = aux c })
  | LE_sumElim { pred; a; b; c } -> 
    pack (LE_sumElim { pred = aux pred; a = aux a; b = aux b; c = aux c })
  | LE_natElim { pred; a; b; c } -> 
    pack (LE_natElim { pred = aux pred; a = aux a; b = aux b; c = aux c })
;;