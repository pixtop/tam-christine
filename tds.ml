open Hashtbl
open Type

(* Définition du type des informations associées aux identifiants *)
type info =
  | InfoConst of int
  | InfoVar of typ * int * string
  | InfoFun of typ * typ list

(* Données stockées dans la tds  et dans les AST : pointeur sur une information *)
type info_ast = info ref

(* Table des symboles hiérarchique *)
(* Les tables locales sont codées à l'aide d'une hashtable *)
type tds =
  | Nulle
  (* Table courante : la table mère - la table courante *)
  | Courante of tds * (string,info_ast) Hashtbl.t


let info_to_info_ast i = ref i

let info_ast_to_info i = !i

let creerTDSMere () = Courante (Nulle, Hashtbl.create 100)

let creerTDSFille mere = Courante (mere, Hashtbl.create 100)

let ajouter tds nom info =
  match tds with
  | Nulle -> failwith "Ajout dans une table vide"
  | Courante (m,c) -> Hashtbl.add c nom info

let chercherLocalement tds nom =
  match tds with
  | Nulle -> None
  | Courante (m,c) ->  find_opt c nom

let rec chercherGlobalement tds nom =
  match tds with
  | Nulle -> None
  | Courante (m,c) ->
    match find_opt c nom with
      | Some _ as i -> i
      | None -> chercherGlobalement m nom


let string_of_info info =
  match info with
  | InfoConst value -> "Constante "^(string_of_int value)
  | InfoVar (t,dep,base) -> "Variable "^(string_of_type t)^" "^(string_of_int dep)^"["^base^"]"
  | InfoFun (t,tp) -> "Fonction "^(List.fold_right (fun elt tq -> if tq = "" then (string_of_type elt) else (string_of_type elt)^" * "^tq) tp "" )^
                      " -> "^(string_of_type t)


let afficher_locale tds =
  match tds with
  | Nulle -> print_newline ()
  |Courante (m,c) -> Hashtbl.iter ( fun n info -> (print_string (n^" : "^(string_of_info (info_ast_to_info info))^"\n"))) c


let afficher_globale tds =
  let rec afficher tds indent =
    match tds with
    | Nulle -> print_newline ()
    | Courante (m,c) -> if Hashtbl.length c = 0
                        then print_string (indent^"<empty>\n")
                        else Hashtbl.iter ( fun n info -> (print_string (indent^n^" : "^(string_of_info (info_ast_to_info info))^"\n"))) c ; afficher m (indent^"  ")
  in afficher tds ""


let modifier_type_info t i =
   match !i with
   |InfoVar (_,dep,base) -> i:= InfoVar (t,dep,base)
   | _ -> ()

let modifier_type_fonction_info t tp i =
    match !i with
    |InfoFun(_,_) -> i:= InfoFun(t,tp)
    | _ -> ()

let modifier_adresse_info d b i =
    match !i with
    |InfoVar (t,_,_) -> i:= InfoVar (t,d,b)
    | _ -> ()
