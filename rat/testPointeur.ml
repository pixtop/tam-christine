open Compilateur
open Exceptions
open Type
open TestTam

exception ErreurNonDetectee;;

let%test_unit "test" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/test.rat" in ()

let%test_unit "test2" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/test2.rat" in ()

let%test_unit "testAffectation1" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation1.rat" in ()

let%test_unit "testAffectation2" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation2.rat"
      in raise ErreurNonDetectee
  with
  | TypeInattendu(Rat, Int) -> ()

let%test_unit "testAffectation3" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation3.rat" in ()

let%test_unit "testAffectation4" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation4.rat"
      in raise ErreurNonDetectee
  with
  | TypeInattendu(Rat, Pt(Rat)) -> ()

let%test_unit "testAffectation5" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation5.rat" in ()

let%test_unit "testAffectation6" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation6.rat"
      in raise ErreurNonDetectee
  with
  | TypeInattendu(Pt(Int), Pt(Rat)) -> ()

let%test_unit "testAffectation7" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation7.rat" in ()

let%test_unit "testAffectation8" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation8.rat" in ()

let%test_unit "testAffectation9" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation9.rat"
      in raise ErreurNonDetectee
  with
  | PasUnPointeur -> ()

let%test_unit "testAffectation10" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation10.rat"
    in raise ErreurNonDetectee
  with
  | TypeInattendu(Undefined, Int) -> ()

let%test_unit "testAffectation11" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testAffectation11.rat" in ()

let%test_unit "testConst" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testConst.rat"
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("a") -> ()

let%test_unit "testUtilisation1" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testUtilisation1.rat" in ()

let%test_unit "testUtilisation2" =
  let _ = compiler "../../../fichiersRat/src-rat-pointeur-test/testUtilisation2.rat" in ()

let%expect_test "testDoublePointeur" =
  runtam "../../../fichiersRat/src-rat-pointeur-test/testDoublePointeur.rat";
  [%expect{| [1/2] |}]

let%expect_test "testFonctionPt" =
  runtam "../../../fichiersRat/src-rat-pointeur-test/testFonctionPt.rat";
  [%expect{| 01 |}]
