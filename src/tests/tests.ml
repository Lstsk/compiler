(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/before_4.bird" "3";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/plus2.bird" "6";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/minus2.bird" "-2";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";

    test_success "test_code/arithmetic.bird" "-9";

    test_success "test_code/let1.bird" "7";
    test_success "test_code/let2.bird" "9";
    test_success "test_code/let3.bird" "10";

    test_success "test_code/complex1.bird" "12";
    test_success "test_code/complex2.bird" "10";
    test_success "test_code/complex3.bird" "40";
    test_success "test_code/complex4.bird" "-18";
    test_success "test_code/complex5.bird" "9";
    test_success "test_code/complex6.bird" "9";

    (* Bluebird *)
    test_success "test_code/bluebird/isint1.bird" "true";
    test_success "test_code/bluebird/isint2.bird" "true";
    test_success "test_code/bluebird/isint3.bird" "false";
    test_success "test_code/bluebird/isint4.bird" "false";
    test_success "test_code/bluebird/isbool1.bird" "true";
    test_success "test_code/bluebird/isbool2.bird" "true";
    test_success "test_code/bluebird/isbool3.bird" "false";
    test_success "test_code/bluebird/isbool4.bird" "false";
    test_success "test_code/bluebird/if1.bird" "3";
    test_success "test_code/bluebird/if2.bird" "1";
    test_success "test_code/bluebird/if3.bird" "7";
    test_success "test_code/bluebird/operator_lt.bird" "false";
    test_success "test_code/bluebird/operator_lt_2.bird" "false";
    test_success "test_code/bluebird/operator_lt_3.bird" "true";
    test_success "test_code/bluebird/if4.bird" "false";
    test_success "test_code/bluebird/if5.bird" "false";
    test_success "test_code/bluebird/if_nested.bird" "10";
    test_success "test_code/bluebird/if_nested_1.bird" "8";
    test_success "test_code/bluebird/if_nested_2.bird" "20";



    test_success "test_code/bluebird/operator_gt.bird" "true";
    test_success "test_code/bluebird/operator_gt_2.bird" "false";
    test_success "test_code/bluebird/operator_gt_3.bird" "false";
    test_success "test_code/bluebird/operator_eq.bird" "true";
    test_success "test_code/bluebird/operator_eq_1.bird" "false";



    (* Cardinal *)
    test_success "test_code/cardinal/print1.bird" "739\n739";
    test_success "test_code/cardinal/print2.bird" "4\nfalse\nfalse";
    test_runtime_failure "test_code/cardinal/stopWithError1.bird" 1;
    test_runtime_failure "test_code/cardinal/stopWithError2.bird" 1;
    test_runtime_failure "test_code/cardinal/errorBinaryOp1.bird" 1;
    test_runtime_failure "test_code/cardinal/errorBinaryOp2.bird" 1;
    test_runtime_failure "test_code/cardinal/errorBinaryOp3.bird" 1;
    test_runtime_failure "test_code/cardinal/errorBinaryOp4.bird" 1;
    test_success "test_code/cardinal/errorBinaryOp5.bird" "false";
    test_runtime_failure "test_code/cardinal/errorBinaryOp6.bird" 1;
    test_runtime_failure "test_code/cardinal/errorBinaryOp7.bird" 2;
    test_runtime_failure "test_code/cardinal/errorBinaryOp8.bird" 2;
    test_runtime_failure "test_code/cardinal/errorBinaryOp9.bird" 2;
    test_success "test_code/cardinal/orderOfOperation.bird" "3\n2\n1\n";

    (* Dove
    test_success "test_code/dove/def1.bird" "1";
    test_success "test_code/dove/def2.bird" "18";
    test_success "test_code/dove/def3.bird" "28";
    test_success "test_code/dove/def4.bird" "2";
    test_success "test_code/dove/def5.bird" "15";
    test_success "test_code/dove/def6.bird" "10\n1\n2\n3\n4";
    test_success "test_code/dove/def7.bird" "0";
    test_success "test_code/dove/def8.bird" "true";
    test_success "test_code/dove/unbound_variable_2.bird" "3";
    test_success "test_code/dove/order_of_op.bird" "1\n3\n4";

    test_compile_failure "test_code/dove/duplicate_param_1.bird" "Illformed code: \nFunction f declares a duplicate parameter x";
    test_compile_failure "test_code/dove/duplicate_func_def_1.bird" "Illformed code: \nDuplicate function definition f";
    test_compile_failure "test_code/dove/function_not_defined_1.bird" "Illformed code: \nUnbound variable g.";
    test_compile_failure "test_code/dove/function_not_defined_2.bird" "Illformed code: \nUnbound variable c.";
    test_compile_failure "test_code/dove/function_not_defined_3.bird" "Illformed code: \nUnbound variable c.";
    test_compile_failure "test_code/dove/function_not_defined_5.bird" "Illformed code: \nUnbound variable g.";
    test_compile_failure "test_code/dove/unbound_variable_1.bird" "Illformed code: \nUnbound variable y.";
    test_compile_failure "test_code/dove/unbound_variable_3.bird" "Illformed code: \nUnbound variable x.\nUnbound variable y.";
    test_compile_failure "test_code/dove/unbound_variable_4.bird" "Illformed code: \nUnbound variable l.";

    test_compile_failure "test_code/dove/all_error.bird" "Illformed code: \nUnbound variable g.\nUnbound variable y.\nUnbound variable z.";

    (* Eagle *)
    test_success "test_code/eagle/eagle_simple1.bird" "(6, (6, 2))";
    test_success "test_code/eagle/eagle_simple2.bird" "(true, false)";
    test_success "test_code/eagle/eagle_simple3.bird" "6";
    test_success "test_code/eagle/eagle_simple5.bird" "1\n2\n1";
    test_runtime_failure "test_code/eagle/eagle_simple4.bird" 4;
    test_runtime_failure "test_code/eagle/eagle_simple6.bird" 4;
    test_runtime_failure "test_code/eagle/eagle_simple7.bird" 3;
    test_runtime_failure "test_code/eagle/eagle_simple8.bird" 1;
    test_runtime_failure "test_code/eagle/eagle_simple9.bird" 1;
    test_success "test_code/eagle/simple10.bird" "true";
    test_success "test_code/eagle/simple11.bird" "false";
    test_success "test_code/eagle/eagle_complex1.bird" "5";
    test_success "test_code/eagle/eagle_complex2.bird" "(7, 11, 15, 9)";
    test_runtime_failure "test_code/eagle/eagle_bool.bird" 2;
    test_compile_failure "test_code/eagle/eagle_error1.bird" "Illformed code: \nUnbound variable f.";
    test_compile_failure "test_code/eagle/eagle_error2.bird" "Illformed code: \nUnbound variable g.";
    test_compile_failure "test_code/eagle/eagle_error3.bird" "Illformed code: \nUnbound variable f.\nUnbound variable g.\nUnbound variable h.";
    test_compile_failure "test_code/eagle/eagle_error4.bird" "Illformed code: \nUnbound variable x.";
    test_compile_failure "test_code/eagle/eagle_error5.bird" "Illformed code: \nUnbound variable y.";
    test_success "test_code/eagle/eagle_error6.bird" "(15, 21, 36)";


    (* Falcon *)
    (* test_success "test_code/falcon/zero_closure1.bird" "<closure@000000000040117f>[0/1](?)";
    test_success "test_code/falcon/zero_closure2.bird" "<closure@000000000040117f>[0/2](?, ?)"; *)
    test_success "test_code/falcon/falcon_func1.bird" "2";
    test_success "test_code/falcon/falcon_func3.bird" "(-2, -6)";
    test_success "test_code/falcon/falcon_func4.bird" "6";
    test_success "test_code/falcon/falcon_func5.bird" "7";
    test_success "test_code/falcon/falcon_func6.bird" "10";
    test_success "test_code/falcon/falcon_func7.bird" "12";
    test_success "test_code/falcon/falcon_func9.bird" "8";
    test_success "test_code/falcon/test1.bird" "8";


    (* Gull *)
    test_success "test_code/gull/tuple_set.bird" "12";
    test_compile_failure "test_code/gull/tuple_set2.bird" "Illformed code: \nUnbound variable x.\nUnbound variable y.\nUnbound variable z.";
    test_success "test_code/gull/gc_1.bird" "1048576";
    test_runtime_failure "test_code/gull/gc_2.bird" 7 ;
    test_success "test_code/gull/gc_3.bird" "(55, 56)" ; *)
  ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;
