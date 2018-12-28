open Compilateur
open Exceptions

exception ErreurNonDetectee;;

let%test_unit "test"=
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/test.rat" in ()

let%test_unit "test2" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/test2.rat" in ()

let%test_unit "testDeclarationFonction" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDeclarationFonction.rat" in ()

let%test_unit "testDoubleDeclarationFonction" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationFonction.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("add") -> ()

let%test_unit "testDoubleDeclarationParametre1" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationParametre1.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("a") -> ()

let%test_unit "testDoubleDeclarationParametre2" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationParametre2.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("a") -> ()

let%test_unit "testDoubleDeclarationVariable1" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationVariable1.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()

let%test_unit "testDoubleDeclarationVariable2" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationVariable2.rat" in ()

let%test_unit "testDoubleDeclarationVariable3" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationVariable3.rat" in ()

let%test_unit "testDoubleDeclarationVariable4" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationVariable4.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()

let%test_unit "testDoubleDeclarationVariable5" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationVariable5.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()

let%test_unit "testDoubleDeclarationVariable6" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationVariable6.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()

let%test_unit "testDoubleDeclarationVariable7" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testDoubleDeclarationVariable7.rat"
    in raise ErreurNonDetectee
  with
  | DoubleDeclaration("x") -> ()

let%test_unit "testAffectation1" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testAffectation1.rat" in ()

let%test_unit "testAffectation2"=
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testAffectation2.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testAffectation3" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testAffectation3.rat" in ()

let%test_unit "testAffectation4" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testAffectation4.rat"
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("x") -> ()

let%test_unit "testAffectation5" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testAffectation5.rat"
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("add") -> ()
  | MauvaiseUtilisationIdentifiant("add2") -> ()

let%test_unit "testUtilisation1" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation1.rat" in ()

let%test_unit "testUtilisation2" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation2.rat" in ()

let%test_unit "testUtilisation3" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation3.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation4" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation4.rat"
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("add") -> ()

let%test_unit "testUtilisation5" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation5.rat"
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("add") -> ()

let%test_unit "testUtilisation6" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation6.rat" in ()

let%test_unit "testUtilisation7" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation7.rat" in ()

let%test_unit "testUtilisation8" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation8.rat"
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("x") -> ()

let%test_unit "testUtilisation9" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation9.rat"
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("z") -> ()

let%test_unit "testUtilisation10" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation10.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()

let%test_unit "testUtilisation11" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation11.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation12" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation12.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation13" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation13.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation14" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation14.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation15" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation15.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation16" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation16.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation17" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation17.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation18" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation18.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation19" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation19.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation20" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation20.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation21" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation21.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation22" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation22.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation23" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation23.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()

let%test_unit "testUtilisation24" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation24.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()

let%test_unit "testUtilisation25" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation25.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("a") -> ()

let%test_unit "testUtilisation26" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testUtilisation26.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("a") -> ()

let%test_unit "testRecursiviteVariable" =
  try
    let _ = compiler "../../../fichiersRat/src-rat-tds-test/testRecursiviteVariable.rat"
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()

let%test_unit "testRecursiviteFonction" =
  let _ = compiler "../../../fichiersRat/src-rat-tds-test/testRecursiviteFonction.rat" in ()
