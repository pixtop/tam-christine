(* Interface définissant une passe *)
module type Passe =
sig
  (* type des AST en entrée de la passe *)
  type t1
  (* type des AST en sortie de la passe *)
  type t2

  (* fonction d'analyse qui tranforme un AST de type t1
  en un AST de type t2 en réalisant des vérifications *)
  val analyser : t1 -> t2
end


(* Passe unit -> unit *)
(* Ne fait rien *)
(* Nécessaire aux compilateurs intermédiaires (non complets) *)
module PasseNop : Passe  with type t1 = unit and type t2 = unit=
struct
  type t1 = unit
  type t2 = unit

  let analyser _a = ()

end

(* Passe AstSyntax.programme -> unit *)
(* Ne fait rien *)
(* Nécessaire aux compilateurs intermédiaires (non complets) *)
module PasseTdsNop : Passe  with type t1 = Ast.AstSyntax.programme and type t2 = unit=
struct
  type t1 = Ast.AstSyntax.programme
  type t2 = unit

  let analyser _a = ()

end

(* Passe unit -> string *)
(* Ne fait rien *)
(* Nécessaire aux compilateurs intermédiaires (non complets) *)
module PasseCodeNopNop : Passe  with type t1 = unit and type t2 = string=
struct
  type t1 = unit
  type t2 = string

  let analyser _a = ""

end


(* Passe AstTds.programme -> unit *)
(* Ne fait rien *)
(* Nécessaire aux compilateurs intermédiaires (non complets) *)
module PasseTypeNop : Passe  with type t1 = Ast.AstTds.programme and type t2 = unit=
struct
  type t1 = Ast.AstTds.programme
  type t2 = unit

  let analyser _a = ()

end

(* Passe AstType.programme -> unit *)
(* Ne fait rien *)
(* Nécessaire aux compilateurs intermédiaires (non complets) *)
module PassePlacementNop : Passe  with type t1 =  Ast.AstType.programme and type t2 = unit=
struct
  type t1 = Ast.AstType.programme
  type t2 = unit

  let analyser _a = ()

end

(* Passe AstPlacement.programme -> string *)
(* Affiche les adresses des variables  *)
(* Pour tester les paramètres des fonctions, il est nécessaire de les mettre en retour *)
module PasseAffichePlacement : Passe  with type t1 =  Ast.AstPlacement.programme and type t2 = string=
struct
  type t1 = Ast.AstPlacement.programme
  type t2 = string

  let rec analyser_affectable af =
    match af with
    | Ast.AstType.Valeur(a) -> analyser_affectable a
    | Ast.AstType.Indice(a, e) -> analyser_affectable a ^ analyser_expression e
    | Ast.AstType.Ident (ia) ->
      begin
        match Tds.info_ast_to_info ia with
        | InfoVar (_,d,r) -> (string_of_int d)^"["^r^"]"
        | _ -> ""
      end

  (* Renvoie l'adresse quand l'expression est l'utilisation d'un identifiant *)
  (* Astuce pour afficher le placement des paramètres puisqu'on n'a plus la liste des paramètres *)
  and analyser_expression e =
    match e with
    | Ast.AstType.Acces af -> analyser_affectable af
    | _ -> ""

  (* Renvoie l'adresse d'une variable dans le cas d'une déclaration *)
  let rec analyser_instruction i =
    match i with
    | Ast.AstType.Declaration (_,info) ->
      begin
        match Tds.info_ast_to_info info with
        | InfoVar (_,d,r) -> (string_of_int d)^"["^r^"]"
        | _ -> ""
        end
    | Ast.AstType.Conditionnelle(_,bt,be) -> (String.concat "" (List.map (analyser_instruction) bt))^(String.concat "" (List.map (analyser_instruction) be))
    | Ast.AstType.TantQue (_,b) -> (String.concat "" (List.map (analyser_instruction) b))
    | _ -> "" ;;


  (* Renvoie la suite des adresses des variables déclarées dans la fonction *)
  (* Ainsi qu'une adresse d'identifiant si le retour est un identifiant *)
  let analyser_fonction (Ast.AstPlacement.Fonction(_,li,e,_)) =
    (*La liste des paramètres n'est plus présente, pour tester le placement des paramètres, on utilisera une astuce :
    il faudra écrire un programme qui renvoie le paramètre *)
    (String.concat "" (List.map (analyser_instruction) li))^(analyser_expression e)

  (* Renvoie la suite des adresses des variables déclarées dans les fonctions et dans le programme principal *)
  let analyser (Ast.AstPlacement.Programme (fonctions, prog)) =
    (String.concat "" (List.map (analyser_fonction) fonctions))^(String.concat "" (List.map (analyser_instruction) prog))

end

(* Passe AstPlacement.programme -> string *)
(* Ne fait rien *)
(* Nécessaire aux compilateurs intermédiaires (non complets) *)
module PasseCodeNop : Passe  with type t1 = Ast.AstPlacement.programme and type t2 = string=
struct
  type t1 = Ast.AstPlacement.programme
  type t2 = string

  let analyser _a = ""

end
