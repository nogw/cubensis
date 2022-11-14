type t = string [@@deriving show]

let make t = t
let equal = String.equal
let compare = String.compare

module NameMap = Map.Make (String)
module NameSet = Set.Make (String)

let ( <@> ) s1 s2 = NameSet.union s1 s2

let rec fresh ~avoid ~name =
  if (NameSet.mem name avoid) 
    then fresh ~avoid ~name:(name ^ "\'")
    else name