module PasseCodeRatToTam : Passe.Passe  with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

open Ast
open Tds
open AstPlacement
open Type
open Exceptions
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string

let pf = Printf.sprintf

let rec analyse_valeur_affectable a =
  match a with
  | AstType.Valeur af ->
    let (t,s) = analyse_valeur_affectable af in
      begin
      match t with
        | Pt t_var | Tab t_var -> (t_var, s^"LOADI (1)\n")
        | _ -> raise ErreurInattendue
      end
  | AstType.Ident ia ->
    begin
      match info_ast_to_info ia with
      | InfoVar (Pt t_var, d, r) | InfoVar (Tab t_var, d, r) -> (t_var, pf "LOAD (1) %d[%s]\n" d r)
      | _ -> raise ErreurInattendue
    end
  | AstType.Indice (af, e) ->
    let (t,s) = analyse_valeur_affectable af in
      begin
      match t with
        | Tab t_var | Pt t_var -> (t_var, pf "%s%sSUBR IAdd\nLOADI (1)\n" s (analyse_code_expression e))
        | _ -> raise ErreurInattendue
      end


and analyse_code_affectable_d a =
  match a with
  | AstType.Valeur af ->
    let (t, s) = analyse_valeur_affectable af in s^pf "LOADI (%d)\n" (getTaille t)
  | AstType.Ident ia ->
    begin
      match info_ast_to_info ia with
      | InfoVar (t, d, r) -> pf "LOAD (%d) %d[%s]\n" (getTaille t) d r
      | InfoConst i -> pf "LOADL %d\n" i
      | _ -> raise ErreurInattendue
    end
  | AstType.Indice (af, e) ->
    let (t,s) = analyse_valeur_affectable af in
      pf "%s%sLOADL %d\nSUBR IMul\nSUBR IAdd\nLOADI (%d)\n" s (analyse_code_expression e) (getTaille t) (getTaille t)

and analyse_code_affectable_g a =
  match a with
  | AstType.Valeur af ->
    let (t, s) = analyse_valeur_affectable af in s^pf "STOREI (%d)\n" (getTaille t)
  | AstType.Ident ia ->
    begin
      match info_ast_to_info ia with
      | InfoVar (t, d, r) -> pf "STORE (%d) %d[%s]\n" (getTaille t) d r
      | _ -> raise ErreurInattendue
    end
  | AstType.Indice (af, e) ->
    let (t,s) = analyse_valeur_affectable af in
      pf "%s%sLOADL %d\nSUBR IMul\nSUBR IAdd\nSTOREI (%d)\n" s (analyse_code_expression e) (getTaille t) (getTaille t)



and analyse_code_expression e =
  match e with
    | AstType.AppelFonction(n, le, _) ->
      (List.fold_left (fun ac e -> ac ^ analyse_code_expression e) "" le) ^
        pf "CALL (SB) %s\n" n
    | AstType.Rationnel(num, den) ->
      analyse_code_expression num ^ analyse_code_expression den
    | AstType.Numerateur e -> analyse_code_expression e ^ "POP (0) 1\n"
    | AstType.Denominateur e -> analyse_code_expression e ^ "POP (1) 1\n"
    | AstType.True -> "LOADL 1\n"
    | AstType.False -> "LOADL 0\n"
    | AstType.Entier i -> pf "LOADL %d\n" i
    | AstType.Binaire(bin, e1, e2) ->
      let ea1 = (analyse_code_expression e1) and ea2 = (analyse_code_expression e2) in
        begin
          match bin with
          | PlusInt ->  ea1 ^ ea2 ^ "SUBR IAdd\n"
          | PlusRat ->  ea1 ^ ea2 ^ "CALL (ST) RAdd\n"
          | MultInt ->  ea1 ^ ea2 ^ "SUBR IMul\n"
          | MultRat ->  ea1 ^ ea2 ^ "CALL (ST) RMul\n"
          | EquInt ->   ea1 ^ ea2 ^ "SUBR IEq\n"
          | EquBool ->  ea1 ^ ea2 ^ "SUBR IEq\n"
          | Inf ->      ea1 ^ ea2 ^ "SUBR ILss\n"
        end
    | AstType.Acces af -> analyse_code_affectable_d af
    | AstType.Vide -> "SUBR MVoid\n"
    | AstType.Adresse ia ->
      begin
        match info_ast_to_info ia with
        | InfoVar(_, d, r) -> pf "LOADA %d[%s]\n" d r
        | _ -> raise ErreurInattendue
      end
    | AstType.Allocation t -> pf "LOADL %d\nSUBR MAlloc\n" (getTaille t)
    | AstType.Array_Allocation (t, e) ->
      pf "%sLOADL %d\nSUBR IMul\nSUBR MAlloc\n" (analyse_code_expression e) (getTaille t)


let rec analyse_code_instruction i pop_size =
    match i with
      | AstType.Declaration(e, ia) ->
        begin
          match info_ast_to_info ia with
            | InfoVar(t, _, _) -> (analyse_code_expression e, pop_size + getTaille t)
            | _ -> raise ErreurInattendue
        end
      | AstType.Affectation(af, e) ->
        analyse_code_expression e ^ analyse_code_affectable_g af, pop_size
      | AstType.AffichageInt(e) ->
        analyse_code_expression e ^ "SUBR IOut\n", pop_size
      | AstType.AffichageRat(e) ->
        analyse_code_expression e ^ "CALL (ST) ROut\n", pop_size
      | AstType.AffichageBool(e) ->
        analyse_code_expression e ^ "SUBR BOut\n", pop_size
      | AstType.Conditionnelle(e, b1, b2) ->
        let etiq1 = getEtiquette () and etiq2 = getEtiquette () in
        analyse_code_expression e ^ "JUMPIF (1) "^etiq1 ^
          addPop (analyse_code_bloc b2) 0 ^ "JUMP "^etiq2 ^
          etiq1 ^ addPop (analyse_code_bloc b1) 0 ^
        etiq2, pop_size
      | AstType.TantQue(e, b) ->
        let etiq1 = getEtiquette () and etiq2 = getEtiquette () in
        etiq1 ^ analyse_code_expression e ^ "JUMPIF (0) "^etiq2 ^
          addPop (analyse_code_bloc b) 0 ^ "JUMP " ^etiq1 ^
        etiq2, pop_size
      | AstType.Empty -> "", pop_size


and analyse_code_bloc blc =
  List.fold_left
  (fun (ls, pop) a -> let (s,p) = analyse_code_instruction a pop in ls ^ s, p) ("", 0) blc


let analyse_code_fonction (Fonction(n, blc, ret_exp, ia)) =
  match info_ast_to_info ia with
    | InfoFun(ret, args) ->
      let ret_size = getTaille ret
      and args_size = List.fold_left sumTaille 0 args
      and (blc_str, blc_pop) = analyse_code_bloc blc in
        pf "%s ; fonction\n" n ^
        blc_str ^
        (analyse_code_expression ret_exp) ^ addPop ("", blc_pop) ret_size ^
        pf "RETURN (%d) %d\n" ret_size args_size
    | _ -> raise ErreurInattendue


let analyser (Programme(fcts, prog)) =
    let fcts_str = List.fold_left (fun ls fct -> ls ^ analyse_code_fonction fct ^ "\n") "" fcts
    and main = "LABEL main\n" ^ (addPop (analyse_code_bloc prog) 0) ^ "HALT\n" in
    getEntete () ^ fcts_str ^ main


end
