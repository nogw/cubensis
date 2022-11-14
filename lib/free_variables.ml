open Syntax
open Name

let rec free_vars expr =
  let (LE { desc = expr_in; _ }) = expr in

  match expr_in with
  | LE_nat | LE_zero | LE_type _ -> 
    NameSet.empty
  | LE_var { var } -> 
    NameSet.singleton var
  | LE_lambda { param; body; _ } -> 
    NameSet.remove param (free_vars body)
  | LE_pi { param; anno; body; _ } -> 
    NameSet.union (free_vars anno) (NameSet.remove param (free_vars body))
  | LE_app { lambda = left; arg = right }
  | LE_sum { head = left; tail = right }
  | LE_propEq { left; right }
  | LE_refl { left; right }
  | LE_pair { left; right } -> 
    NameSet.union (free_vars left) (free_vars right)
  | LE_fst { expr }
  | LE_snd { expr }
  | LE_inl { expr }
  | LE_inr { expr }
  | LE_succ { expr } -> free_vars expr
  | LE_natElim { pred; a; b; c }
  | LE_sumElim { pred; a; b; c }
  | LE_eqElim { pred; a; b; c } ->
    free_vars pred
    |> NameSet.union (free_vars a)
    |> NameSet.union (free_vars b)
    |> NameSet.union (free_vars c)