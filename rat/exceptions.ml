open Type
open Ast.AstSyntax

(* Exceptions pour la gestion des identificateurs *)
exception DoubleDeclaration of string
exception IdentifiantNonDeclare of string
exception MauvaiseUtilisationIdentifiant of string

(* Exceptions pour le typage *)
(* Le premier type est le type réel, le second est le type attendu *)
exception TypeInattendu of typ * typ
exception TypesParametresInattendus of typ list * typ list
exception TypeBinaireInattendu of binaire * typ * typ (* les types sont les types réels non compatible avec les signatures connues de l'opérateur *)

(* pour le projet *)
exception PasUnPointeur (* * sur quelque chose qui n'est pas un pointeur *)
exception PasUnTableau
exception FonctionDeclareeNonImplantee of string
exception ImplantationNonCompatibleDeclaration of string

(* dans un cas inattendu *)
exception ErreurInattendue
