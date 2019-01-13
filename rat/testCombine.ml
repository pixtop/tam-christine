open TestTam

let%expect_test "test"=
  runtam "../../../fichiersRat/src-rat-combine-test/test.rat";
  [%expect{| 01010 |}]
