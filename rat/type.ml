type typ = Bool | Int | Rat | Pt of typ | Tab of typ | Nomme of string | Undefined

let rec string_of_type t =
  match t with
  | Bool      ->  "Bool"
  | Int       ->  "Int"
  | Rat       ->  "Rat"
  | Pt typ  ->  "* "^(string_of_type typ)
  | Tab typ -> "[] "^(string_of_type typ)
  | Nomme n -> n
  | Undefined -> "Undefined"


let rec est_compatible t1 t2 =
  match t1, t2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true
  | Pt _, Undefined -> true
  | Undefined, Undefined -> true
  | Pt typ1, Pt typ2 -> est_compatible typ1 typ2
  | Tab typ1, Tab typ2 -> est_compatible typ1 typ2
  | _ -> false

let est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Pt _ -> 1 (* à vérifier *)
  | Tab _ -> 1 (* pareil *)
  | Undefined -> 0

let sumTaille s t = s + getTaille t
