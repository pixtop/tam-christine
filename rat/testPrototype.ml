(* open Compilateur *)
(* open Exceptions *)
open TestTam

let%expect_test "test"=
  runtam "../../../fichiersRat/src-rat-prototype-test/test.rat";
  [%expect{| 5[2/3] |}]

let%expect_test "test2"=
  runtam "../../../fichiersRat/src-rat-prototype-test/test2.rat";
  [%expect{| 01010 |}]
