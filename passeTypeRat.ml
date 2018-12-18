module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstType
  open Type

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme


(* analyse_type_expression : Asttds.expression -> Asttype.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type Asttype.expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e =
  match e with
  | AstTds.AppelFonction(n, args, ia) ->
    let targs = List.map analyse_type_expression args in
      begin
      match info_ast_to_info ia with
      | InfoFun(typfun, typlist) ->
          let valid =
            try List.for_all2 (fun (x,_) y -> x = y) targs typlist
            with Invalid_argument(_) -> false in
              if valid then
                (typfun, AppelFonction(n, List.map (fun (x,y) -> y) targs, ia))
              else
                raise (TypesParametresInattendus(List.map (fun (x,y) -> x) targs, typlist))
      | _ -> raise ErreurInattendue
      end
  | AstTds.Rationnel(e1, e2) ->
    let (t1,v1) = analyse_type_expression e1 and (t2, v2) = analyse_type_expression e2 in
      if t1 = Int then
        if t2 = Int then
          (Rat, Rationnel(v1,v2))
        else raise (TypeInattendu(t2, Int))
      else raise (TypeInattendu(t1, Int))
  | AstTds.Numerateur(e) ->
    let (typ, v) = analyse_type_expression e in
      if typ = Rat then
        (Int, Numerateur(v))
      else raise (TypeInattendu(typ, Rat))
  | AstTds.Denominateur(e) ->
    let (typ, v) = analyse_type_expression e in
      if typ = Rat then
        (Int, Denominateur(v))
      else raise (TypeInattendu(typ, Rat))
  | AstTds.Ident(ia) ->
    begin
      match info_ast_to_info ia with
      | InfoConst(_) -> (Int, Ident(ia))
      | InfoVar(typ, _, _) -> (typ, Ident(ia))
      | InfoFun(_,_) -> raise ErreurInattendue
    end
  | AstTds.True -> (Bool, True)
  | AstTds.False -> (Bool, False)
  | AstTds.Entier(i) -> (Int, Entier(i))
  | AstTds.Binaire(b, e1, e2) ->
    let (t1,v1) = analyse_type_expression e1 and (t2,v2) = analyse_type_expression e2 in
    begin
      match b with
      | Plus ->
        if t1 = Int && t2 = Int then
         (Int, Binaire(PlusInt, v1, v2))
        else if t1 = Rat && t2 = Rat then
         (Rat, Binaire(PlusRat, v1, v2))
        else raise (TypeBinaireInattendu(Plus, t1, t2))
      | Mult ->
        if t1 = Int && t2 = Int then
         (Int, Binaire(MultInt, v1, v2))
        else if t1 = Rat && t2 = Rat then
         (Rat, Binaire(MultRat, v1, v2))
        else raise (TypeBinaireInattendu(Mult, t1, t2))
      | Equ ->
        if t1 = Int && t2 = Int then
         (Bool, Binaire(EquInt, v1, v2))
        else if t1 = Bool && t2 = Bool then
         (Bool, Binaire(EquBool, v1, v2))
        else raise (TypeBinaireInattendu(Equ, t1, t2))
      | Inf ->
        if t1 = Int && t2 = Int then
          (Bool, Binaire(Inf, v1, v2))
        else raise (TypeBinaireInattendu(Inf, t1, t2))
    end


(* analyse_type_instruction : Asttds.instruction -> Asttype.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type Asttype.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration(t, e, ia) ->
      modifier_type_info t ia;
      let (te, ea) = analyse_type_expression e in
        if te = t then
          Declaration(ea, ia)
        else raise (TypeInattendu(te, t))
  | AstTds.Affectation(e, ia) ->
    let (te,ea) = analyse_type_expression e in
      begin
      match info_ast_to_info ia with
      | InfoVar(t,_,_) ->
        if t = te then Affectation(ea, ia)
        else raise (TypeInattendu(te, t))
      | _ -> raise ErreurInattendue
      end
  | AstTds.Affichage(e) ->
    let (te, ea) = analyse_type_expression e in
      begin
        match te with
        | Bool -> AffichageBool(ea)
        | Int -> AffichageInt(ea)
        | Rat -> AffichageRat(ea)
        | Undefined -> raise ErreurInattendue
      end
  | AstTds.Conditionnelle(e,tb,eb) ->
    let (te, ea) = analyse_type_expression e in
      if te = Bool then Conditionnelle(ea, List.map analyse_type_instruction tb, analyse_type_bloc eb)
      else raise (TypeInattendu (te, Bool))
  | AstTds.TantQue(e,b) ->
    let (te, ea) = analyse_type_expression e in
      if te = Bool then TantQue(ea, analyse_type_bloc b)
      else raise (TypeInattendu (te, Bool))
  | AstTds.Empty -> Empty


(* analyse_type_bloc : Asttds.bloc -> Asttype.bloc *)
(* Paramètre b : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc
en un bloc de type Asttype.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc b =
  (* Analyse des instructions du bloc avec la tds du nouveau bloc
  Cette tds est modifiée par effet de bord *)
  List.map (analyse_type_instruction) b

(* analyse_type_fonction : Asttds.fonction -> Asttype.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme la fonction
en une fonction de type Asttype.fonction *)
(* Erreur si mauvaise utilisation des types *)
let analyse_type_fonction (AstTds.Fonction(typ, n, params, b, e, ai)) =
    modifier_type_fonction_info typ (List.map (fun (t,_) -> t) params) ai;
    List.iter (fun (t,ai) -> modifier_type_info t ai) params;
    let blck = List.map analyse_type_instruction b in
      let (te,ea) = analyse_type_expression e in
        if typ = te then
          Fonction(n, List.map (fun (_,ai) -> ai) params, blck, ea, ai)
          else raise (TypeInattendu(te, typ))


(* analyser : Asttds.ast -> Asttype.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type Asttype.ast *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme(fonctions, prog)) =
  let fcts = List.map analyse_type_fonction fonctions in
    let blc = analyse_type_bloc prog in Programme(fcts, blc)

end
