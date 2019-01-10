open Compilateur
open Exceptions
open TestTam


exception ErreurNonDetectee

let%test_unit "test"=
  let _ = compiler "../../../fichiersRat/src-rat-typenomme-test/test.rat" in ()

let%test_unit "test2"=
  let _ = compiler "../../../fichiersRat/src-rat-typenomme-test/test2.rat" in ()

let%expect_test "test3"=
  runtam "../../../fichiersRat/src-rat-typenomme-test/test3.rat";
  [%expect{| 1 |}]

let%test_unit "testDeclaration"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-typenomme-test/testDeclaration.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration ("Int2") -> ()

let%test_unit "testDeclaration2"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-typenomme-test/testDeclaration2.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare ("Int") -> ()

let%test_unit "testDeclaration3"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-typenomme-test/testDeclaration3.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare ("Couple") -> ()
