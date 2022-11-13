type argument = Impl | Expl [@@deriving show]

type expr = LE of { loc: Location.t; desc: raw_expr }

and raw_expr =
  | LE_nat
  | LE_zero
  | LE_type of { var : int option }
  | LE_var of { var : Name.t }
  | LE_lambda of { param : raw_expr; arg : argument; body : raw_expr }
  | LE_pi of { param : raw_expr; anno : raw_expr; arg : argument; body : raw_expr }
  | LE_app of { lambda : raw_expr; arg : raw_expr }
  | LE_sum of { head : raw_expr; tail : raw_expr }
  | LE_pair of { left : raw_expr; right : raw_expr }
  | LE_fst of { expr : raw_expr }
  | LE_snd of { expr : raw_expr }
  | LE_inl of { expr : raw_expr }
  | LE_inr of { expr : raw_expr }
  | LE_succ of { expr : raw_expr }
  | LE_refl of { left : raw_expr; right : raw_expr }
  | LE_natElim of { pred : raw_expr; a : raw_expr; b : raw_expr; c : raw_expr }
  | LE_eqElim of { pred : raw_expr; a : raw_expr; b : raw_expr; c : raw_expr }
  | LE_sumElim of { pred : raw_expr; a : raw_expr; b : raw_expr; c : raw_expr }
  | LE_propEq of { left : raw_expr; right : raw_expr }
  [@@deriving show { with_path = false }]