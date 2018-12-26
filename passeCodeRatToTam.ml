module PasseCodeRatToTam : Passe.Passe  with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Ast
  open Tds
  open AstPlacement
  open Type
  open Exceptions

  type t1 = Ast.AstPlacement.programme
  type t2 = string

  let add_pop (code, pop) = (* Ajouter des pops pour variables locales si pop > 0 *)
    if pop > 0 then code ^ "\nPOP (0) " ^ (string_of_int pop)
      else code

  let pf = Printf.sprintf

  let rec analyse_valeur_affectable la a =
    match a with
    | AstType.Valeur af ->
      let (t,i) = analyse_valeur_affectable la af in (t,"LOADI (1)"::i)
    | AstType.Ident ia ->
      begin
        match info_ast_to_info ia with
        | InfoVar (t, d, r) -> (getTaillePt t, (pf "LOAD (1) %d[%s]" d r)::la)
        | _ -> raise ErreurInattendue
      end

  let analyse_code_affectable_d a =
    match a with
    | AstType.Valeur af ->
      let (t, la) = analyse_valeur_affectable [] af in String.concat "\n" (List.rev la@[pf "LOADI (%d)" t])
    | AstType.Ident ia ->
      begin
        match info_ast_to_info ia with
        | InfoVar (t, d, r) -> pf "LOAD (%d) %d[%s]" (getTaille t) d r
        | InfoConst i -> pf "LOADL %d" i
        | _ -> raise ErreurInattendue
      end

  let analyse_code_affectable_g a =
    match a with
    | AstType.Valeur af ->
      let (t, la) = analyse_valeur_affectable [] af in String.concat "\n" (List.rev la@[pf "STOREI (%d)" t])
    | AstType.Ident ia ->
      begin
        match info_ast_to_info ia with
        | InfoVar (t, d, r) -> pf "STORE (%d) %d[%s]" (getTaille t) d r
        | _ -> raise ErreurInattendue
      end

  let rec analyse_code_expression e =
    match e with
      | AstType.AppelFonction(n, le, _) ->
        (List.fold_right (fun a e -> (analyse_code_expression a) ^ "\n" ^ e) le "") ^
          "CALL (SB) " ^ n
      | AstType.Rationnel(num, den) ->
        (analyse_code_expression den) ^ "\n" ^ (analyse_code_expression num)
      | AstType.Numerateur e -> (analyse_code_expression e) ^ "\nPOP (1) 1"
      | AstType.Denominateur e -> (analyse_code_expression e) ^ "\nPOP (0) 1"
      (* | Ident(ia) ->
        begin
          match info_ast_to_info ia with
            | InfoVar(t, d, r) -> "LOAD (" ^ (getTaille t) ^ ") " ^ (string_of_int d) ^ "[" ^ r ^ "]"
            | InfoConst(i) -> "LOADL " ^ (string_of_int i)
            | _ -> failwith("they were both happy")
        end *)
      | AstType.True -> "LOADL 1"
      | AstType.False -> "LOADL 0"
      | AstType.Entier i -> "LOADL " ^ (string_of_int i)
      | AstType.Binaire(bin, e1, e2) ->
        let ea1 = (analyse_code_expression e1) and ea2 = (analyse_code_expression e2) in
          begin
            match bin with
            | PlusInt -> ea1 ^ "\n" ^ ea2 ^ "\n" ^ "SUBR IAdd"
            | PlusRat ->
              ea1 ^ "\nPOP (0) 1\n" ^ ea2 ^ "\nPOP (0) 1\nSUBR IMul\n" ^
              ea1 ^ "\nPOP (1) 1\n" ^ ea2 ^ "\nPOP (0) 1\nSUBR IMul\n" ^
              ea2 ^ "\nPOP (1) 1\n" ^ ea1 ^ "\nPOP (0) 1\nSUBR IMul\nSUBR IAdd\nCALL (SB) __pgcd__"
            | MultInt -> ea1 ^ "\n" ^ ea2 ^ "\n" ^ "SUBR IMul"
            | MultRat ->
              ea1 ^ "\nPOP (0) 1\n" ^ ea2 ^ "\nPOP (0) 1\nSUBR IMul\n" ^
              ea1 ^ "\nPOP (1) 1\n" ^ ea2 ^ "\nPOP (1) 1\nSUBR IMul\nCALL (SB) __pgcd__"
            | EquInt -> ea1 ^ "\n" ^ ea2 ^ "\n" ^ "SUBR IEq"
            | EquBool -> ea1 ^ "\n" ^ ea2 ^ "\n" ^ "SUBR IEq"
            | Inf -> ea1 ^ "\n" ^ ea2 ^ "\n" ^ "SUBR ILss"
          end
      | AstType.Acces af -> analyse_code_affectable_d af
      | AstType.Vide -> "\n SUBR MVoid"
      | AstType.Adresse ia ->
        begin
          match info_ast_to_info ia with
          | InfoVar(_, d, r) -> pf "LOADA %d[%s]" d r
          | _ -> raise ErreurInattendue
        end
      | AstType.Allocation t -> pf "LOADL %d\nSUBR MAlloc" (getTaille t)

  let rec analyse_code_instruction i ord pop_size =
    match i with
      | AstType.Declaration(e, ia) ->
        begin
          match info_ast_to_info ia with
            | InfoVar(t, _, _) -> (analyse_code_expression e, ord, pop_size + getTaille t)
            | _ -> raise ErreurInattendue
        end
      | AstType.Affectation(af, e) ->
        (String.concat "\n" ((analyse_code_expression e)::(analyse_code_affectable_g af)::[]), ord, pop_size)
      | AstType.AffichageInt(e) ->
        (String.concat "\n" ((analyse_code_expression e)::"SUBR IOut"::[]), ord, pop_size)
      | AstType.AffichageRat(e) ->
        (String.concat "\n" ("LOADL 91"::"SUBR COut"::(analyse_code_expression e)::"SUBR IOut"::
          "LOADL 47"::"SUBR COut"::"SUBR IOut"::"LOADL 93"::"SUBR COut"::[]), ord, pop_size)
      | AstType.AffichageBool(e) ->
        (String.concat "\n" ((analyse_code_expression e)::"SUBR BOut"::[]), ord, pop_size)
      | AstType.AffichagePt(e) ->
        (String.concat "\n" ((analyse_code_expression e)::"SUBR IOut"::[]), ord, pop_size)
      | AstType.Conditionnelle(e, b1, b2) ->
        (String.concat "\n" ((analyse_code_expression e)::("JUMPIF (1) cond_" ^ (string_of_int ord))::
          (add_pop (analyse_code_bloc b2))::("JUMP cond_end_" ^ (string_of_int ord))::("cond_" ^ (string_of_int ord))::
          (add_pop (analyse_code_bloc b1))::("cond_end_" ^ (string_of_int ord))::[]), ord + 1, pop_size)
      | AstType.TantQue(e, b) ->
        (String.concat "\n" (("tq_" ^ (string_of_int ord))::(analyse_code_expression e)::("JUMPIF (0) tq_end_" ^ (string_of_int ord))::
          (add_pop (analyse_code_bloc b))::("JUMP tq_" ^ (string_of_int ord))::("tq_end_" ^ (string_of_int ord))::[]), ord + 1, pop_size)
      | AstType.Empty -> ("", ord, pop_size)


  and analyse_code_bloc blc =
    let (str, ord, pop) = (List.fold_left
    (fun (str, ord, pop) a -> let (strn, ordn, popn) = analyse_code_instruction a ord pop in ((if (str = "") then (strn) else (str ^ "\n" ^ strn)), ordn, popn))
      ("", 0, 0)
        blc)
    in (str, pop)

  let analyse_code_fonction (Fonction(n, blc, ret_exp, ia)) =
    match info_ast_to_info ia with
      | InfoFun(ret, args) ->
        let args_size = (List.fold_right (fun a e -> e + getTaille a) args 0)
          and (blc_str, blc_pop) = analyse_code_bloc blc in
          n ^ "; fonction " ^ n ^ "\n" ^ blc_str ^ "\n" ^
          (analyse_code_expression ret_exp) ^ "\n" ^ (if blc_pop > 0 then ("POP (" ^ string_of_int (getTaille ret) ^ ") " ^ (string_of_int blc_pop) ^ "\n") else "") ^
          "RETURN (" ^ string_of_int (getTaille ret) ^ ") " ^ (string_of_int args_size) ^ "\n"
      | _ -> raise ErreurInattendue


  let analyser (Programme(fcts, prog)) =
      let main = "; Point d'entrée\n" ^ (add_pop (analyse_code_bloc prog)) ^ "\nHALT\n" and fcts_str = String.concat "" (List.map (analyse_code_fonction) fcts) in
        main ^ fcts_str ^ Pgcd.tam_pgcd

end
