open Compilateur
open Exceptions
open TestTam
open Type

exception ErreurNonDetectee;;

(* Tests erreurs tds *)
let%test_unit "test_tds_2" =
try
  let _ = compiler "../../../fichiersRat/src-rat-tableau-test/test_tds_2.rat"
    in raise ErreurNonDetectee
with
| IdentifiantNonDeclare(_) -> ()

let%test_unit "test_tds_3" =
try
  let _ = compiler "../../../fichiersRat/src-rat-tableau-test/test_tds_3.rat"
    in raise ErreurNonDetectee
with
| DoubleDeclaration(_) -> ()

(* Tests erreurs typage *)
let%test_unit "test_typ_1" =
try
  let _ = compiler "../../../fichiersRat/src-rat-tableau-test/test_type_1.rat"
    in raise ErreurNonDetectee
with
| TypeInattendu(Bool, Int) -> ()

let%test_unit "test_typ_2" =
try
  let _ = compiler "../../../fichiersRat/src-rat-tableau-test/test_type_2.rat"
    in raise ErreurNonDetectee
with
| TypeBinaireInattendu(Ast.AstSyntax.Plus, Tab Int, Int) -> ()

let%test_unit "test_typ_3" =
try
  let _ = compiler "../../../fichiersRat/src-rat-tableau-test/test_type_3.rat"
    in raise ErreurNonDetectee
with
| PasUnTableau -> ()

let%test_unit "test_typ_4" =
try
  let _ = compiler "../../../fichiersRat/src-rat-tableau-test/test_type_4.rat"
    in raise ErreurNonDetectee
with
| PasUnTableau -> ()

(* Tests résultats programmes *)
let%expect_test "test_tds_1" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_tds_1.rat";
  [%expect{| 3 |}]

let%expect_test "test_doubleTableau" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_doubleTableau.rat";
  [%expect{| 2 |}]

let%expect_test "test_doubleTableauPointeur" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_doubleTableauPointeur.rat";
  [%expect{| 2 |}]

let%expect_test "test_affichage" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_affichage.rat";
  [%expect{| 1234 |}]

let%expect_test "test_tableauRat" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_tableauRat.rat";
  [%expect{| [2/2][1/2][3/2] |}]

let%expect_test "test_tableauPointeurRat" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_tableauPointeurRat.rat";
  [%expect{| [2/2][1/2][3/2] |}]

let%expect_test "test_boucle" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_boucle.rat";
  [%expect{| 012345678 |}]

let%expect_test "test_boucleRat" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_boucleRat.rat";
  [%expect{| [0/2][1/2][2/2][3/2][4/2][5/2][6/2][7/2][8/2] |}]

let%expect_test "test_fonction" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_fonction.rat";
  [%expect{| [5/4] |}]

let%expect_test "test_fonction" =
  runtam "../../../fichiersRat/src-rat-tableau-test/test_fonction.rat";
  [%expect{| [5/4] |}]
