open Ast
open Compilateur
open Exceptions

exception ErreurNonDetectee;;

let%test_unit "test"=
  let _ = compiler "../../fichiersRat/src-rat-pointeur-test/test.rat" in ()

let%test_unit "test2" =
  let _ = compiler "../../fichiersRat/src-rat-pointeur-test/test2.rat" in ()
