open Compilateur
(* open Exceptions *)
open TestTam

let%expect_test "test"=
  runtam "../../../fichiersRat/src-rat-prototype-test/test.rat";
  [%expect{| 5[2/3] |}]

let%test_unit "test2"=
  let _ = compiler "../../../fichiersRat/src-rat-prototype-test/test2.rat" in ()
