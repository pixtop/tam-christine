open Compilateur
(* open Exceptions *)

(* exception ErreurNonDetectee *)

let%test_unit "test"=
  let _ = compiler "../../../fichiersRat/src-rat-typenomme-test/test.rat" in ()
