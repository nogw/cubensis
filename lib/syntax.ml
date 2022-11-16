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
  | LE_sigma of { param : Name.t; anno : expr; body : expr }
  | LE_app of { lambda : expr; arg : expr }
  | LE_sum of { left : expr; right : expr }
  | LE_pair of { left : expr; right : expr }
  | LE_fst of { expr : expr }
  | LE_snd of { expr : expr }
  | LE_inl of { expr : expr }
  | LE_inr of { expr : expr }
  | LE_succ of { expr : expr }
  | LE_meta of { expr : Name.t }
  | LE_refl of { left : expr; right : expr }
  | LE_natElim of { pred : expr; a : expr; b : expr; c : expr }
  | LE_eqElim of { pred : expr; a : expr; b : expr; c : expr }
  | LE_sumElim of { pred : expr; a : expr; b : expr; c : expr }
  | LE_propEq of { left : expr; right : expr }
  [@@deriving show { with_path = false }]

let pack loc desc = LE { loc; desc } 
let unpack = function | LE { desc; _ } -> desc 

type bind = LB of { loc : Loc.t; [@opaque] desc : raw_bind }
  [@@deriving show]

and raw_bind =
  | LB_normalize of { expr : expr }
  | LB_declaration of { name : Name.t; body : expr }
  | LB_typeSignature of { name : Name.t; body : expr }
  | LB_typeDeclaration of { name : Name.t; bindings : bind list }
  [@@deriving show { with_path = false }]

let pack_lb loc desc = LB { loc; desc } 
let unpack_lb = function | LB { desc; _ } -> desc 

let le_var ~loc ~var = pack loc (LE_var { var })
let le_type ~loc ~var = pack loc (LE_type { var })
let le_lambda ~loc ~param ~arg ~body = pack loc (LE_lambda { param; arg; body })
let le_pi ~loc ~param ~anno ~arg ~body = pack loc (LE_pi { param; anno; arg; body })
let le_sigma ~loc ~param ~anno ~body = pack loc (LE_sigma { param; anno; body })
let le_app ~loc ~lambda ~arg = pack loc (LE_app { lambda; arg })
let le_sum ~loc ~left ~right = pack loc (LE_sum { left; right })
let le_pair ~loc ~left ~right = pack loc (LE_pair { left; right })
let le_meta ~loc ~expr = pack loc (LE_meta { expr })
let le_fst ~loc ~expr = pack loc (LE_fst { expr })
let le_snd ~loc ~expr = pack loc (LE_snd { expr })
let le_inl ~loc ~expr = pack loc (LE_inl { expr })
let le_inr ~loc ~expr = pack loc (LE_inr { expr })
let le_nat ~loc = pack loc LE_nat
let le_zero ~loc = pack loc LE_zero
let le_succ ~loc ~expr = pack loc (LE_succ { expr })
let le_refl ~loc ~left ~right = pack loc (LE_refl { left; right })
let le_propEq ~loc ~left ~right = pack loc (LE_propEq { left; right })
let le_natElim ~loc ~pred ~a ~b ~c = pack loc (LE_natElim { pred; a; b; c })
let le_eqElim ~loc ~pred ~a ~b ~c = pack loc (LE_eqElim { pred; a; b; c })
let le_sumElim ~loc ~pred ~a ~b ~c = pack loc (LE_sumElim { pred; a; b; c })

let lb_normalize ~loc ~expr = pack_lb loc (LB_normalize { expr })
let lb_declaration ~loc ~name ~body = pack_lb loc (LB_declaration { name; body })
let lb_typeSignature ~loc ~name ~body = pack_lb loc (LB_typeSignature { name; body })
let lb_typeDeclaration ~loc ~name ~bindings = pack_lb loc (LB_typeDeclaration { name; bindings })