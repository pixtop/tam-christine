open Compilateur
open Exceptions
open Type
open TestTam

exception ErreurNonDetectee;;

(* Test passe tds *)
let%test_unit "test_tds" =
try
  let _ = compiler "../../../fichiersRat/src-rat-for-test/testTds.rat"
    in raise ErreurNonDetectee
with
| IdentifiantNonDeclare("a") -> ()

let%test_unit "test_tds2" =
try
  let _ = compiler "../../../fichiersRat/src-rat-for-test/testTds2.rat"
    in raise ErreurNonDetectee
with
| DoubleDeclaration("i") -> ()

let%test_unit "test_tds3" =
  let _ = compiler "../../../fichiersRat/src-rat-for-test/testTds3.rat"
    in ()

(* Test passe type *)

let%test_unit "test_typ" =
try
  let _ = compiler "../../../fichiersRat/src-rat-for-test/testType.rat"
    in raise ErreurNonDetectee
with
| TypeInattendu(Rat, Int) -> ()

let%test_unit "test_typ2" =
try
  let _ = compiler "../../../fichiersRat/src-rat-for-test/testType2.rat"
    in raise ErreurNonDetectee
with
| TypeInattendu(Int, Pt Int) -> ()

let%test_unit "test_typ2" =
try
  let _ = compiler "../../../fichiersRat/src-rat-for-test/testType3.rat"
    in raise ErreurNonDetectee
with
| TypeInattendu(Int, Bool) -> ()


(* Tests résultats programmes *)
let%expect_test "test_tds_1" =
  runtam "../../../fichiersRat/src-rat-for-test/testRun.rat";
  [%expect{| 01 |}]

let%expect_test "test_tds_2" =
  runtam "../../../fichiersRat/src-rat-for-test/testRun2.rat";
  [%expect{| [6/3] |}]

let%expect_test "test_tds_3" =
  runtam "../../../fichiersRat/src-rat-for-test/testRun3.rat";
  [%expect{| 012 |}]
