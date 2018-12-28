(* getEtiquette : unit -> string *)
(* Renvoie une étiquette TAM *)
(* Chaque appel donne une étiquette différente *)
val getEtiquette : unit -> string ;;

(* addPop : string * int -> int -> string *)
(* Ajout de POP pour variables locales *)
(* Premier paramètre : le bloc de code et le nombre de pop *)
(* Second paramètre : le déplacement dans le cas d'une fonction *)
val addPop : string * int -> int -> string

(* getEntete : unit -> string *)
(* Renvoie ce qui doit être mis en entête du fichier TAM *)
val getEntete : unit -> string ;;

(* ecrireFichier : string -> string -> unit *)
(* écrit une chaine de caractère dans un fichier *)
(* Premier paramètre : l'URL du fichier *)
(* Second paramètre : le texte à écrire *)
(* Erreur si l'URL du fichier n'est pas valide *)
val ecrireFichier : string -> string -> unit ;;
