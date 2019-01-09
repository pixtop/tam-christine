(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct


  open Tds
  open Exceptions
  open Ast
  open AstTds


  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme


(* analyse_tds_type : tds -> Typ -> Typ *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre t : le type à analyser *)
(* Vérifie que le type et transforme si il est nommé par son véritable typ *)
(* Erreur si le type nommé n'a pas été déclaré *)
let rec analyse_tds_type tds t =
  match t with
  | Type.Tab tt -> Type.Tab(analyse_tds_type tds tt)
  | Type.Pt tp -> Type.Pt(analyse_tds_type tds tp)
  | Type.Nomme n ->
    begin
      match chercherGlobalement tds n with
      | None -> raise (IdentifiantNonDeclare n)
      | Some ia ->
        begin
        match info_ast_to_info ia with
        | InfoTyp tn -> tn
        | _ -> raise ErreurInattendue
        end
    end
  | _ -> t


(* analyse_tds_affectable : bool -> tds -> AstSyntax.ast -> Asttds.affectable *)
(* Paramètre gauche : précise si l'affectable est à gauche ou à droite du = *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre af : l'affectable à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'affectable
en un affectable de type Asttds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_affectable gauche tds af =
  match af with
  | AstSyntax.Valeur(a) -> Valeur(analyse_tds_affectable gauche tds a)
  | AstSyntax.Indice(a, e) -> Indice(analyse_tds_affectable gauche tds a, analyse_tds_expression tds e)
  | AstSyntax.Ident(n) ->
    begin
    match chercherGlobalement tds n with
      | None -> raise (IdentifiantNonDeclare n)
      | Some info_ast ->
        if gauche then
        match info_ast_to_info info_ast with
        | InfoVar _ -> Ident (info_ast)
        |  _ ->
          (* Modification d'une constante ou d'une fonction *)
          raise (MauvaiseUtilisationIdentifiant n)
        else
        match info_ast_to_info info_ast with
        | InfoFun (_,_) -> raise (MauvaiseUtilisationIdentifiant n)
        | _ -> Ident(info_ast)
    end

(* analyse_tds_expression : tds -> AstSyntax.ast -> Asttds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type Asttds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_expression tds e =
  match e with
    | AstSyntax.AppelFonction(n, args) ->
      begin
      match chercherGlobalement tds n with
        | None -> raise (IdentifiantNonDeclare n)
        | Some ia ->
          begin
          match (info_ast_to_info ia) with
          | InfoFun(_, _) ->
              AppelFonction(n, List.map (analyse_tds_expression tds) args , ia)
          | _ -> raise (MauvaiseUtilisationIdentifiant n)
          end
      end
    | AstSyntax.Rationnel(e1,e2) -> Rationnel(analyse_tds_expression tds e1, analyse_tds_expression tds e2)
    | AstSyntax.Numerateur(e) -> Numerateur(analyse_tds_expression tds e)
    | AstSyntax.Denominateur(e) -> Denominateur(analyse_tds_expression tds e)
    | AstSyntax.True -> True
    | AstSyntax.False -> False
    | AstSyntax.Entier(i) -> Entier(i)
    | AstSyntax.Binaire(op, a, b) -> Binaire(op, analyse_tds_expression tds a, analyse_tds_expression tds b)
    | AstSyntax.Acces(a) -> let ta = analyse_tds_affectable false tds a in Acces(ta)
    | AstSyntax.Vide -> Vide
    | AstSyntax.Adresse(n) ->
      begin
      match chercherGlobalement tds n with
        | None -> raise (IdentifiantNonDeclare n)
        | Some info_ast ->
        begin
        match (info_ast_to_info info_ast) with
        | InfoVar _ -> Adresse(info_ast)
        | _ -> raise (MauvaiseUtilisationIdentifiant n)
        end
      end
    | AstSyntax.Allocation(t) ->
      Allocation(analyse_tds_type tds t)
    | AstSyntax.Array_Allocation(t, e) ->
      Array_Allocation(analyse_tds_type tds t, analyse_tds_expression tds e)


(* analyse_tds_instruction : tds -> AstSyntax.instruction -> Asttds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type Asttds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
      match chercherLocalement tds n with
      | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
          il n'a donc pas été déclaré dans le bloc courant *)
          (* Vérification de la bonne utilisation des identifiants dans l'expression *)
          (* et obtention de l'expression transformée *)
          let ne = analyse_tds_expression tds e in
          (* Création de l'information associée à l'identfiant *)
          let info = InfoVar (Undefined, 0, "") in
          (* Création du pointeur sur l'information *)
          let ia = info_to_info_ast info in
          (* Ajout de l'information (pointeur) dans la tds *)
          ajouter tds n ia;
          (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
          et l'expression remplacée par l'expression issue de l'analyse *)
          Declaration (analyse_tds_type tds t, ne, ia)
      | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) ->
    let ta = analyse_tds_affectable true tds a in
      let te = analyse_tds_expression tds e in
        Affectation(ta,te)
  | AstSyntax.Constante (n,v) ->
      begin
      match chercherLocalement tds n with
      | None ->
      (* L'identifiant n'est pas trouvé dans la tds locale,
      il n'a donc pas été déclaré dans le bloc courant *)
      (* Ajout dans la tds de la constante *)
      ajouter tds n (info_to_info_ast (InfoConst v));
      (* Suppression du noeud de déclaration des constantes devenu inutile *)
      Empty
      | Some _ ->
        (* L'identifiant est trouvé dans la tds locale,
        il a donc déjà été déclaré dans le bloc courant *)
        raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)
  | AstSyntax.TypeNomme (n, t) ->
    begin
      match chercherLocalement tds n with
      | None ->
        ajouter tds n (info_to_info_ast (InfoTyp (analyse_tds_type tds t)));
        Empty
      | Some _ -> raise (DoubleDeclaration n)
    end


(* analyse_tds_bloc : AstSyntax.bloc -> Asttds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type Asttds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc
  Cette tds est modifiée par effet de bord *)
  List.map (analyse_tds_instruction tdsbloc) li


(* analyse_tds_fonction : AstSyntax.fonction -> Asttds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type Asttds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li,e))  =
    match chercherLocalement maintds n with
      | Some _ -> raise (DoubleDeclaration n)
      | None -> let tds = creerTDSFille maintds in
      let fill (t,s) =
        match chercherLocalement tds s with
        | Some _ -> raise (DoubleDeclaration s)
        | None -> let lia = info_to_info_ast (InfoVar(Undefined, 0, "")) in ajouter tds s lia;
                  (analyse_tds_type tds t,lia)
    in
      let args_list = List.map fill lp in
        let ia = info_to_info_ast (InfoFun(Undefined, [])) in ajouter maintds n ia;
          let blc = List.map (analyse_tds_instruction tds) li in
            let ret = analyse_tds_expression tds e in
              let nt = analyse_tds_type tds t in
                Fonction(nt, n, args_list, blc, ret, ia)


(* analyser : AstSyntax.ast -> Asttds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type Asttds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let mainTds = creerTDSMere () in
    let fcts = List.map (analyse_tds_fonction mainTds) fonctions in
      let blc = analyse_tds_bloc mainTds prog in Programme(fcts, blc);;

end
