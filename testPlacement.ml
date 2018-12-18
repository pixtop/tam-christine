open Ast
open Compilateur
open Exceptions


let%expect_test "test1" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test1.rat");
 [%expect{| 0[SB] |}]

let%expect_test "test2" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test2.rat");
 [%expect{| 0[SB]1[SB]2[SB] |}]

let%expect_test "test3" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test3.rat");
 [%expect{| 0[SB]1[SB]2[SB] |}]

let%expect_test "test4" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test4.rat");
 [%expect{| 0[SB]2[SB]4[SB] |}]

let%expect_test "test5" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test5.rat");
 [%expect{| 0[SB]1[SB]3[SB] |}]

let%expect_test "test6" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test6.rat");
 [%expect{| 0[SB]1[SB]3[SB]4[SB]5[SB]7[SB]4[SB]5[SB]7[SB]4[SB]5[SB]7[SB] |}]

let%expect_test "test7" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test7.rat");
 [%expect{| 0[SB]1[SB]3[SB]4[SB]5[SB]7[SB]4[SB]5[SB]7[SB] |}]

let%expect_test "test8" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test8.rat");
 [%expect{| 3[LB]4[LB]6[LB]7[LB]8[LB]10[LB]7[LB]8[LB]10[LB]0[SB]1[SB]3[SB]4[SB]5[SB]7[SB]4[SB]5[SB]7[SB] |}]

let%expect_test "test9" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test9.rat");
 [%expect{| -1[LB] |}]

let%expect_test "test10" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test10.rat");
 [%expect{| -2[LB] |}]

let%expect_test "test10" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test10.rat");
 [%expect{| -2[LB] |}]

let%expect_test "test10" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test10.rat");
 [%expect{| -2[LB] |}]

let%expect_test "test11" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test11.rat");
 [%expect{| -1[LB] |}]

let%expect_test "test12" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test12.rat");
 [%expect{| -4[LB] |}]

let%expect_test "test13" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test13.rat");
 [%expect{| -3[LB] |}]

let%expect_test "test14" =
 print_string (afficherDep "../../fichiersRat/src-rat-placement-test/test14.rat");
 [%expect{| -1[LB] |}]
