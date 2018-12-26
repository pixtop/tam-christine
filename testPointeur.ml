open Ast
open Compilateur
open Exceptions
open Type

exception ErreurNonDetectee;;

let path = "../../fichiersRat/src-rat-pointeur-test/"

let%test_unit "test"=
  let _ = compiler path^"test.rat" in ()

let%test_unit "test2" =
  let _ = compiler path^"test2.rat" in ()

let%test_unit "testAffectation1" =
  let _ = compiler path^"testAffectation1.rat" in ()

let%test_unit "testAffectation2" =
  try
    let _ = compiler path^"testAffectation2.rat"
      in raise ErreurNonDetectee
  with
  | TypeInattendu(Rat, Int) -> ()

let%test_unit "testAffectation3" =
  let _ = compiler path^"testAffectation3.rat" in ()

let%test_unit "testAffectation4" =
  try
    let _ = compiler path^"testAffectation4.rat"
      in raise ErreurNonDetectee
  with
  | TypeInattendu(Rat, Pt(Rat)) -> ()

let%test_unit "testAffectation5" =
  let _ = compiler path^"testAffectation5.rat" in ()

let%test_unit "testAffectation6" =
  try
    let _ = compiler path^"testAffectation6.rat"
      in raise ErreurNonDetectee
  with
  | TypeInattendu(Pt(Int), Pt(Rat)) -> ()

let%test_unit "testAffectation7" =
  let _ = compiler path^"testAffectation7.rat" in ()

let%test_unit "testAffectation8" =
  let _ = compiler path^"testAffectation8.rat" in ()

let%test_unit "testAffectation9" =
  try
    let _ = compiler path^"testAffectation9.rat"
      in raise ErreurNonDetectee
  with
  | PasUnPointeur -> ()

let%test_unit "testAffectation10" =
  try
    let _ = compiler path^"testAffectation10.rat"
    in raise ErreurNonDetectee
  with
  | TypeInattendu(Undefined, Int) -> ()
