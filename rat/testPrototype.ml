open Compilateur
open Exceptions
open TestTam

exception ErreurNonDetectee

let%expect_test "testImplantation"=
  runtam "../../../fichiersRat/src-rat-prototype-test/testImplantation.rat";
  [%expect{| 3 |}]

let%test_unit "testImplantationNonCompatible"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-prototype-test/testImplantationNonCompatible.rat"
    in raise ErreurNonDetectee
  with
  | ImplantationNonCompatibleDeclaration ("somme") -> ()

let%test_unit "testDoubleDeclaration"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-prototype-test/testDoubleDeclaration.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration ("somme") -> ()

let%test_unit "testDoubleDeclaration2"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-prototype-test/testDoubleDeclaration2.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration ("TabInt") -> ()

let%test_unit "testFonctionDeclareeNonImplantee"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-prototype-test/testFonctionDeclareeNonImplantee.rat"
    in raise ErreurNonDetectee
  with
  | FonctionDeclareeNonImplantee ("somme") -> ()

let%expect_test "testTypeDefini"=
  runtam "../../../fichiersRat/src-rat-prototype-test/testTypeDefini.rat";
  [%expect{| [5/2] |}]

let%expect_test "testDefinitionLocale"=
  runtam "../../../fichiersRat/src-rat-prototype-test/testDefinitionLocale.rat";
  [%expect{| 42 |}]

let%expect_test "test"=
  runtam "../../../fichiersRat/src-rat-prototype-test/test.rat";
  [%expect{| 5[2/3] |}]
