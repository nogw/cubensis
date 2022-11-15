open Syntax
open Name
open Free_vars

let rec substitution ~from_ ~to_ ~in_ =
  let (LE { loc; desc = expr_in }) = in_ in

  let aux expr = substitution ~from_ ~to_ ~in_:expr in  

  match expr_in with
  | LE_var { var } when var = from_ -> to_
  | LE_var _ -> in_
  | LE_type _ -> in_
  | LE_nat -> in_
  | LE_zero -> in_
  | LE_lambda { param; _ } when param = from_ -> in_
  | LE_lambda { param; arg; body} when NameSet.mem param (free_vars to_) ->
    let avoid = (NameSet.singleton from_) <@> (free_vars in_) <@> (free_vars to_) in
    let param = fresh ~avoid ~name:param in
    let body  = substitution ~from_:param ~to_:(le_var ~loc ~var:param) ~in_:body |> aux in
    le_lambda ~loc ~param ~arg ~body
  | LE_lambda { param; arg; body} -> 
    let body = aux body in
    le_lambda ~loc ~param ~arg ~body
  | LE_pi { param; _ } when param = from_ -> in_
  | LE_pi { param; anno; arg; body } when NameSet.mem param (free_vars to_) ->
    let avoid = ((NameSet.singleton from_) <@> (free_vars in_) <@> (free_vars to_)) in
    let anno  = aux anno in
    let param = fresh ~avoid ~name:param in
    let body  = substitution ~from_:param ~to_:(le_var ~loc ~var:param) ~in_:body |> aux in    
    le_pi ~loc ~param ~anno ~arg ~body
  | LE_pi { param; anno; arg; body } -> 
    let anno = aux anno in
    let body = aux body in
    le_pi ~loc ~param ~anno ~arg ~body
  | LE_app { lambda; arg } -> 
    let lambda = aux lambda in
    let arg = aux arg in
    le_app ~loc ~lambda ~arg
  | LE_pair { left; right } -> 
    let left = aux left in
    let right = aux right in 
    le_pair ~loc ~left ~right
  | LE_fst { expr } -> 
    let expr = aux expr in
    le_fst ~loc ~expr
  | LE_snd { expr } -> 
    let expr = aux expr in
    le_snd ~loc ~expr
  | LE_inl { expr } -> 
    let expr = aux expr in
    le_inl ~loc ~expr
  | LE_inr { expr } -> 
    let expr = aux expr in
    le_inr ~loc ~expr
  | LE_succ { expr } -> 
    let expr = aux expr in
    le_succ ~loc ~expr
  | LE_sum { head; tail } -> 
    let head = aux head in
    let tail = aux tail in
    le_sum ~loc ~head ~tail
  | LE_refl { left; right } ->
    let left = aux left in
    let right = aux right in 
    le_refl ~loc ~left ~right
  | LE_propEq { left; right } -> 
    let left = aux left in
    let right = aux right in
    le_propEq ~loc ~left ~right
  | LE_eqElim { pred; a; b; c } -> 
    let pred = aux pred in
    let a = aux a in 
    let b = aux b in 
    let c = aux c in 
    le_eqElim ~loc ~pred ~a ~b ~c
  | LE_sumElim { pred; a; b; c } -> 
    let pred = aux pred in
    let a = aux a in 
    let b = aux b in 
    let c = aux c in 
    le_sumElim ~loc ~pred ~a ~b ~c
  | LE_natElim { pred; a; b; c } -> 
    let pred = aux pred in
    let a = aux a in 
    let b = aux b in 
    let c = aux c in 
    le_natElim ~loc ~pred ~a ~b ~c
;;