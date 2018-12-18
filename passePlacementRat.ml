module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Ast
  open AstPlacement
  open Type
  open Exceptions

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme


(* analyse_placement_instruction : int -> string -> Asttdstype.instruction -> Asttdstype.instruction *)
(* Paramètre dp : le déplacement à ajouter *)
(* Paramètre reg : le registre à ajouter *)
(* Paramètre i : l'instruction à analyser *)
(* Ajoute le déplacement et le registre dans la Tds de l'instruction *)
let rec analyse_placement_instruction dp reg i =
  match i with
  | AstType.Declaration(e,ia) ->
    begin
      match info_ast_to_info ia with
      | InfoVar(t,_,_) ->
          let _ = modifier_adresse_info dp reg ia in getTaille t + dp
      | _ -> dp
    end
  | AstType.Conditionnelle(_,tb,eb) ->
      let _ = analyse_placement_bloc dp reg tb and
      _ = analyse_placement_bloc dp reg eb in
      dp
  | AstType.TantQue(_,b) ->
      let _ = analyse_placement_bloc dp reg b in
      dp
  | _ -> dp


(* analyse_placement_bloc : int -> string -> Asttdstype.bloc -> Asttdstype.bloc *)
(* Paramètre dp : le déplacement à ajouter *)
(* Paramètre reg : le registre à ajouter *)
(* Paramètre b : le bloc d'instructions à analyser *)
(* Ajoute le déplacement et le registre dans les instructions du bloc *)
and analyse_placement_bloc dp reg b =
  List.fold_left (fun d i -> analyse_placement_instruction d reg i) dp b


(* analyse_placement_fonction : AstSyntax.fonction -> Astplacement.fonction *)
(* Paramètre : la fonction à analyser *)
(* Ajoute le déplacement et le registre des paramètres et du bloc d'instructions
et tranforme la fonction en une fonction de type Astdeplacement.fonction *)
(* Erreur si comportement inattendu *)
let analyse_placement_fonction (AstType.Fonction(n, params, b, ea, ai)) =
    let calcul_depl param d =
      match info_ast_to_info param with
      | InfoVar(t,_,_) ->  let nd = d-getTaille t in modifier_adresse_info nd "LB" param; nd
      | _ -> raise ErreurInattendue
    in
      let _ = List.fold_right calcul_depl params 0 in
        let _ = analyse_placement_bloc 3 "LB" b in
          Fonction(n, b, ea, ai)


(* analyser : Asttype.ast -> Astdeplacement.ast *)
(* Paramètre : le programme à analyser *)
(* Ajoute le déplacement et le registre dans le programme et tranforme le programme
en un programme de type Astdeplacement.ast *)
(* Erreur si comportement inattendu *)
let analyser (AstType.Programme(fonctions, prog)) =
  let _ = analyse_placement_bloc 0 "SB" prog in
    let fcts = List.map analyse_placement_fonction fonctions in Programme(fcts, prog)

end
