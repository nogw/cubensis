type argument = Impl | Expl 
  [@@deriving show]

type expr = LE of { loc : Loc.t; [@opaque] desc : raw_expr }
  [@@deriving show]

and raw_expr =
  | LE_nat
  | LE_zero
  | LE_type of { var : int option }
  | LE_var of { var : Name.t }
  | LE_lambda of { param : Name.t; arg : argument; body : expr }
  | LE_pi of { param : Name.t; anno : expr; arg : argument; body : expr }
  | LE_app of { lambda : expr; arg : expr }
  | LE_sum of { head : expr; tail : expr }
  | LE_pair of { left : expr; right : expr }
  | LE_fst of { expr : expr }
  | LE_snd of { expr : expr }
  | LE_inl of { expr : expr }
  | LE_inr of { expr : expr }
  | LE_succ of { expr : expr }
  | LE_refl of { left : expr; right : expr }
  | LE_natElim of { pred : expr; a : expr; b : expr; c : expr }
  | LE_eqElim of { pred : expr; a : expr; b : expr; c : expr }
  | LE_sumElim of { pred : expr; a : expr; b : expr; c : expr }
  | LE_propEq of { left : expr; right : expr }
  [@@deriving show { with_path = false }]

let pack_le loc desc = 
  LE { loc; desc } 
;;

let unpack_le = function 
  | LE { desc; _ } -> desc 
;;

type bind = LB of { loc : Loc.t; [@opaque] desc : raw_bind }
  [@@deriving show]

and raw_bind =
  | LB_normalize of { expr : expr }
  | LB_declaration of { name : Name.t; body : expr }
  | LB_typeSignature of { name : Name.t; body : expr }
  | LB_typeDeclaration of { name : Name.t; bindings : bind list }
  [@@deriving show { with_path = false }]