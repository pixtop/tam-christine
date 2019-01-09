open Type

(* Interface des arbres abstraits *)
module type Ast =
sig
   type affectable
   type expression
   type instruction
   type fonction
   type programme
end

(* Interface d'affichage des arbres abstraits *)
module type PrinterAst =
sig
  module A:Ast

(* string_of_expression :  expression -> string *)
(* transforme une expression en chaîne de caractère *)
val string_of_expression : A.expression -> string

(* string_of_affectable : affectable -> string *)
(* transforme un affectable en chaîne de caractère *)
val string_of_affectable : A.affectable -> string

(* string_of_instruction :  instruction -> string *)
(* transforme une instruction en chaîne de caractère *)
val string_of_instruction : A.instruction -> string

(* string_of_fonction :  fonction -> string *)
(* transforme une fonction en chaîne de caractère *)
val string_of_fonction : A.fonction -> string

(* string_of_ast :  ast -> string *)
(* transforme un ast en chaîne de caractère *)
val string_of_programme : A.programme -> string

(* print_ast :  ast -> unit *)
(* affiche un ast *)
val print_programme : A.programme -> unit

end


(*******************************************)
(* AST après la phase d'analyse syntaxique *)
(*******************************************)
module AstSyntax =
struct

(* Opérateurs binaires de Rat *)
type binaire = Plus | Mult | Equ | Inf

(* Affectable de Rat *)
type affectable =
  (* Accès à la valeur du pointeur + décalage si indice de tableau (0 si pointeur) *)
  | Valeur of affectable
  (* l'identifiant de l'affectable *)
  | Ident of string
  (* Accès indice d'un tableau *)
  | Indice of affectable * expression

(* Expressions de Rat *)
and expression =
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AppelFonction of string * expression list
  (* Rationnel représenté par le numérateur et le dénominateur *)
  | Rationnel of expression * expression
  (* Accès au numérateur d'un rationnel *)
  | Numerateur of expression
  (* Accès au dénominateur d'un rationnel *)
  | Denominateur of expression
  (* Accès à un identifiant représenté par son nom *)
  (* | Ident of string *)
  (* Booléen vrai *)
  | True
  (* Booléen faux *)
  | False
  (* Entier *)
  | Entier of int
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | Binaire of binaire * expression * expression
  (* Accès à la variable en mémoire du pointeur *)
  | Acces of affectable
  (* Pointeur null *)
  | Vide
  (* Accès à l'adresse du pointeur *)
  | Adresse of string
  (* Allocation pointeur *)
  | Allocation of typ
  (* Allocation tableau *)
  | Array_Allocation of typ * expression

(* Instructions de Rat *)
type bloc = instruction list
and instruction =
  (* Déclaration de variable représentée par son type, son nom et l'expression d'initialisation *)
  | Declaration of typ * string * expression
  (* Affectation d'une variable représentée par son nom/pointeur et la nouvelle valeur affectée *)
  | Affectation of affectable * expression
  (* Déclaration d'une constante représentée par son nom et sa valeur (entier) *)
  | Constante of string * int
  (* Affichage d'une expression *)
  | Affichage of expression
  (* Conditionnelle représentée par la condition, le bloc then et le bloc else *)
  | Conditionnelle of expression * bloc * bloc
  (* Boucle TantQue représentée par la conditin d'arrêt de la boucle et le bloc d'instructions *)
  | TantQue of expression * bloc
  (* Déclaration d'un type nommé représenté par son nom et son type *)
  | TypeNomme of string * typ

(* Structure des fonctions de Rat *)
(* type de retour - nom - liste des paramètres (association type et nom) - corps de la fonction - valeur de retour *)
type fonction = Fonction of typ * string * (typ * string) list * bloc * expression

(* Structure d'un programme Rat *)
(* liste de fonction - programme principal *)
type programme = Programme of fonction list * bloc

end


(*Module d'affiche des AST issus de la phase d'analyse syntaxique *)
module PrinterAstSyntax : PrinterAst with module A = AstSyntax =
struct

  module A = AstSyntax
  open A

  (* Conversion des opérateurs binaires *)
  let string_of_binaire b =
    match b with
    | Plus -> "+ "
    | Mult -> "* "
    | Equ -> "= "
    | Inf -> "< "

  (* Conversion des affectables *)
  let rec string_of_affectable af =
    match af with
    | Valeur a -> "(*"^(string_of_affectable a)^")"
    | Ident n -> n^" "
    | Indice (a,e) -> "("^(string_of_affectable a)^"["^(string_of_expression e)^"])"

  (* Conversion des expressions *)
  and string_of_expression e =
    match e with
    | AppelFonction (n,le) -> "call "^n^"("^((List.fold_right (fun i tq -> (string_of_expression i)^tq) le ""))^") "
    | Rationnel (e1,e2) -> "["^(string_of_expression e1)^"/"^(string_of_expression e2)^"] "
    | Numerateur e1 -> "num "^(string_of_expression e1)^" "
    | Denominateur e1 ->  "denom "^(string_of_expression e1)^" "
    | True -> "true "
    | False -> "false "
    | Entier i -> (string_of_int i)^" "
    | Binaire (b,e1,e2) -> (string_of_expression e1)^(string_of_binaire b)^(string_of_expression e2)^" "
    | Acces a -> (string_of_affectable a)
    | Vide -> "null "
    | Allocation t -> "(new "^(string_of_type t)^")"
    | Adresse n -> "& "^n^" "
    | Array_Allocation (t,e) -> "(new "^(string_of_type t)^" ["^(string_of_expression e)^"]) "

  (* Conversion des instructions *)
  let rec string_of_instruction i =
    match i with
    | Declaration (t, s, e) -> "Declaration  : "^(string_of_type t)^" "^s^" = "^(string_of_expression e)^"\n"
    | Affectation (a,e) ->  "Affectation  : "^(string_of_affectable a)^"= "^(string_of_expression e)^"\n"
    | Constante (n,i) ->  "Constante  : "^n^" = "^(string_of_int i)^"\n"
    | Affichage e ->  "Affichage  : "^(string_of_expression e)^"\n"
    | Conditionnelle (c,t,e) ->  "Conditionnelle  : IF "^(string_of_expression c)^"\n"^
                                  "THEN \n"^((List.fold_right (fun i tq -> (string_of_instruction i)^tq) t ""))^
                                  "ELSE \n"^((List.fold_right (fun i tq -> (string_of_instruction i)^tq) e ""))^"\n"
    | TantQue (c,b) -> "TantQue  : TQ "^(string_of_expression c)^"\n"^
                                  "FAIRE \n"^((List.fold_right (fun i tq -> (string_of_instruction i)^tq) b ""))^"\n"
    | TypeNomme(n,t) -> "TypeNomme  : TYPE "^n^" = "^(string_of_type t)^"\n"

  (* Conversion des fonctions *)
  let string_of_fonction (Fonction(t,n,lp,li,e)) = (string_of_type t)^" "^n^" ("^((List.fold_right (fun (t,n) tq -> (string_of_type t)^" "^n^" "^tq) lp ""))^") = \n"^
                                        ((List.fold_right (fun i tq -> (string_of_instruction i)^tq) li ""))^
                                        "Return "^(string_of_expression e)^"\n"

  (* Conversion d'un programme Rat *)
  let string_of_programme (Programme (fonctions, instruction)) =
    (List.fold_right (fun f tq -> (string_of_fonction f)^tq) fonctions "")^
    (List.fold_right (fun i tq -> (string_of_instruction i)^tq) instruction "")

  (* Affichage d'un programme Rat *)
  let print_programme programme =
    print_string "AST : \n";
    print_string (string_of_programme programme);
    flush_all ()

end

(*************************************************)
(* AST après la phase d'analyse des identifiants *)
(*************************************************)
module AstTds =
struct

  (* Affectations existantes dans notre langage *)
  (* ~ affectation de l'AST syntaxique où les noms des identificateurs ont synthétisé
  remplacés par les informations associées aux identificateurs *)
  type affectable =
    | Valeur of affectable
    | Ident of Tds.info_ast (* le nom de l'identifiant est remplacé par ses informations *)
    | Indice of affectable * expression

  (* Expressions existantes dans notre langage *)
  (* ~ expression de l'AST syntaxique où les noms des identifiants ont été
  remplacés par les informations associées aux identificateurs *)
  and expression =
    | AppelFonction of string * expression list * Tds.info_ast (* le nom de la fonction est gardé car il sera nécessaire au moment de la génération de code*)
    | Rationnel of expression * expression
    | Numerateur of expression
    | Denominateur of expression
    (* | Ident of Tds.info_ast (* le nom de l'identifiant est remplacé par ses informations *) *)
    | True
    | False
    | Entier of int
    | Binaire of AstSyntax.binaire * expression * expression
    | Acces of affectable
    | Vide
    | Adresse of Tds.info_ast
    | Allocation of typ
    | Array_Allocation of typ * expression

  (* instructions existantes dans notre langage *)
  (* ~ instruction de l'AST syntaxique où les noms des identifiants ont été
  remplacés par les informations associées aux identificateurs
  + suppression de nœuds (const) *)
  type bloc = instruction list
  and instruction =
    | Declaration of typ * expression * Tds.info_ast (* le nom de l'identifiant est remplacé par ses informations *)
    | Affectation of affectable * expression
    | Affichage of expression
    | Conditionnelle of expression * bloc * bloc
    | TantQue of expression * bloc
    | TypeNomme of typ * Tds.info_ast (* le nom de l'identifiant est remplacé par ses informations *)
    | Empty (* les nœuds ayant disparus: Const *)


  (* Structure des fonctions dans notre langage *)
  (* type de retour - nom - liste des paramètres (association type et information sur les paramètres) - corps de la fonction - valeur de retour - information sur la fonction*)
  (* le nom de la fonction est gardé car il sera nécessaire au moment de la génération de code*)
  type fonction = Fonction of typ * string * (typ * Tds.info_ast ) list * bloc * expression * Tds.info_ast

  (* Structure d'un programme dans notre langage *)
  type programme = Programme of fonction list * bloc

end


(***********************************)
(* AST après la phase de typage *)
(***********************************)
module AstType =
struct

(* Affectations existantes dans notre langage *)
(* = affectable de AstTds *)
type affectable =
  | Valeur of affectable
  | Ident of Tds.info_ast
  | Indice of affectable * expression

(* Opérateurs binaires existants dans Rat - résolution de la surcharge *)
and binaire = PlusInt | PlusRat | MultInt | MultRat | EquInt | EquBool | Inf

(* Expressions existantes dans Rat *)
(* = expression de AstTds *)
and expression =
  | AppelFonction of string * expression list * Tds.info_ast
  | Rationnel of expression * expression
  | Numerateur of expression
  | Denominateur of expression
  | True
  | False
  | Entier of int
  | Binaire of binaire * expression * expression
  | Acces of affectable
  | Vide
  | Adresse of Tds.info_ast
  | Allocation of typ
  | Array_Allocation of typ * expression

(* instructions existantes Rat *)
(* = instruction de AstTds + informations associées aux identificateurs, mises à jour *)
(* + résolution de la surcharge de l'affichage *)
type bloc = instruction list
 and instruction =
  | Declaration of expression * Tds.info_ast
  | Affectation of affectable * expression
  | AffichageInt of expression
  | AffichageRat of expression
  | AffichageBool of expression
  | Conditionnelle of expression * bloc * bloc
  | TantQue of expression * bloc
  | TypeNomme of Tds.info_ast
  | Empty (* les nœuds ayant disparus: Const *)

(* nom, liste des paramètres, corps, expression de retour, informations associées à l'identificateur *)
type fonction = Fonction of string * Tds.info_ast list * bloc * expression * Tds.info_ast

(* Structure d'un programme dans notre langage *)
type programme = Programme of fonction list * bloc

end

(***********************************)
(* AST après la phase de placement *)
(***********************************)
module AstPlacement =
struct

(* Affectables existants dans notre langage *)
(* = affectable de AstType  *)
type affectable = AstType.affectable

(* Expressions existantes dans notre langage *)
(* = expression de AstType  *)
type expression = AstType.expression

(* instructions existantes dans notre langage *)
(* = instructions de AstType  *)
type bloc = instruction list
 and instruction = AstType.instruction

(* nom, corps, expression de retour, informations associées à l'identificateur *)
(* Plus besoin de la liste des paramètres *)
type fonction = Fonction of string * bloc * expression * Tds.info_ast

(* Structure d'un programme dans notre langage *)
type programme = Programme of fonction list * bloc

end
