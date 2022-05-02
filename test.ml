open Compile
open Assembly
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Errors
open Graph

let t name program input expected =
  (*TODO put this back to Naive*)
  name >:: test_run ~args:[] ~std_input:input Register program name expected
;;

let tr name program input expected =
  name >:: test_run ~args:[] ~std_input:input Register program name expected
;;

let ta name program input expected =
  name >:: test_run_anf ~args:[] ~std_input:input program name expected
;;

let tgc name include_builtins heap_size program input expected =
  name
  >:: test_run
        ~no_builtins:(not include_builtins)
        ~args:[ string_of_int heap_size ]
        ~std_input:input
        Register
        program
        name
        expected
;;

let tvg name program input expected =
  name >:: test_run_valgrind ~args:[] ~std_input:input Register program name expected
;;

let tvgc name heap_size program input expected =
  name
  >:: test_run_valgrind
        ~args:[ string_of_int heap_size ]
        ~std_input:input
        Register
        program
        name
        expected
;;

let terr name program input expected =
  name >:: test_err ~args:[] ~std_input:input Register program name expected
;;

let tgcerr name heap_size program input expected =
  name
  >:: test_err
        ~args:[ string_of_int heap_size ]
        ~std_input:input
        Register
        program
        name
        expected
;;

let tanf name program input expected =
  name >:: fun _ -> assert_equal expected (anf (tag program)) ~printer:string_of_aprogram
;;

let tparse name program expected =
  name
  >:: fun _ ->
  assert_equal
    (untagP expected)
    (untagP (parse_string name program))
    ~printer:string_of_program
;;

let teq name actual expected =
  name >:: fun _ -> assert_equal expected actual ~printer:(fun s -> s)
;;

(* let tfvs name program expected = name>:: *)
(*   (fun _ -> *)
(*     let ast = parse_string name program in *)
(*     let anfed = anf (tag ast) in *)
(*     let vars = free_vars_P anfed [] in *)
(*     let c = Stdlib.compare in *)
(*     let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in *)
(*     assert_equal (List.sort c vars) (List.sort c expected) ~printer:str_list_print) *)
(* ;; *)

let builtins_size =
  4 (* arity + 0 vars + codeptr + padding *) * List.length Compile.native_fun_bindings
;;

let forty_one = "41"
let forty_one_a = AProgram (ACExpr (CImmExpr (ImmNum (41L, ()))), ())

let test_prog =
  "let x = if sub1(55) < 54: (if 1 > 0: add1(2) else: add1(3)) else: (if 0 == 0: sub1(4) \
   else: sub1(5)) in x"
;;

let anf1 = anf (tag (parse_string "test" test_prog))

(*regression tests*)
let nan_arith_err = "arithmetic expected a number"
let nan_comp_err = "comparison expected a number"
let nab_logic_err = "logic expected a boolean"
let nab_if_err = "if expected a boolean"
let overflow_err = "overflow"
let forty = "let x = 40 in x"
let fals = "let x = false in x"
let tru = "let x = true in x"
let add1_test = "let x = add1(2) in x"
let add1_test_err_nan = "let x = add1(true) in x"
let sub1_test_err_nan = "let x = sub1(false) in x"
let sub1_test = "let x = sub1(2) in x"
let not_test_true = "let x = !(true) in x"
let not_test_false = "let x = !(false) in x"
let not_test_err_nab = "let x = !(2) in x"
let is_bool_test_true = "let x = isbool(true) in x"
let is_bool_test_false = "let x = isbool(1) in x"
let is_num_test_true = "let x = isnum(1) in x"
let is_num_test_false = "let x = isnum(false) in x"
let print_alone = "print(3)"
let add1_to_print_3 = "add1(print(3))"
let plus_test = "3 + 2"
let plus_test_err = "true + 2"
let minus_test = "3 - 2"
let minus_test_err = "true - false"
let times_test = "3 * 2"
let times_test_err = "2 * true"
let and_test = "true && true"
let and_short_circuit_test = "print(false) && print(true)"
let and_short_circuit_test_bad_type = "print(false) && print(7)"
let or_short_circuit_test = "print(true) || print(false)"
let or_short_circuit_test_bad_type = "print(true) || print(7)"
let and_test_err = "true && 1"
let or_test = "true || false"
let three_and_test = "true && true && false"
let three_or_test = "false || false || true"
let or_test_err = "false || 1"
let greater_test_true = "1 > 0"
let greater_equal_false = "1 > 1"
let greater_test_false = "3 > 4"
let greater_test_err = "true > 2"
let less_test_true = "1 < 4"
let less_equal_false = "1 < 1"
let less_test_false = "3 < 2"
let less_test_err = "true < 100"
let lesseq_test_true = "1 <= 4"
let lesseq_equal_true = "1 <= 1"
let lesseq_test_false = "3 <= 2"
let lesseq_test_err = "true <= false"
let greatereq_test_true = "1 >= 0"
let greatereq_equal_true = "1 >= 1"
let greatereq_test_false = "3 >= 4"
let greatereq_test_err = "2 >= true"
let eq_test_true = "1 == 1"
let eq_test_true_bool = "true == true"
let eq_test_false_bool = "true == false"
let eq_test_num_and_bool = "1 == true"
let eq_test_false = "3 == 2"
let print_true = "print(true)"
let print_false = "print(false)"
let print_true_eq = "print(1 == 1)"
let print_false_eq = "print(2 == 1)"
let if_true = "if true: 3 else: 4"
let if_true_eq = "if 1 == 1: 3 else: 4"
let if_false = "if false: 3 else: 4"
let if_false_eq = "if 2 == 1: 3 else: 4"
let if_run_only_true_branch = "if true: print(true) else: print(false)"
let if_run_only_false_branch = "if false: print(true) else: print(false)"
let true_base = "true"
let false_base = "false"
let overflow_plus_err = "9223372036854775807 + 2147483647"
let overflow_minus_err = "-9223372036854775807 - 9223372036854775807"
let overflow_times_err = "-9223372036854775807 * 9223372036854775807"
let overflow_add1_err = "add1(9223372036854775807)"

let regression_compile_passing =
  [ t "forty" forty "" "40"
  ; t "fals" fals "" "false"
  ; t "tru" tru "" "true"
  ; t "add1_1" add1_test "" "3"
  ; t "sub1_1" sub1_test "" "1"
  ; t "is_bool_true" is_bool_test_true "" "true"
  ; t "is_bool_false" is_bool_test_false "" "false"
  ; t "is_num_true" is_num_test_true "" "true"
  ; t "is_num_false" is_num_test_false "" "false"
  ; t "print_just_3" print_alone "" "3\n3"
  ; t "add1_to_print_3" add1_to_print_3 "" "3\n4"
  ; t "plus" plus_test "" "5"
  ; t "minus" minus_test "" "1"
  ; t "times" times_test "" "6"
  ; t "and" and_test "" "true"
  ; t "or" or_test "" "true"
  ; t "greater_true" greater_test_true "" "true"
  ; t "greater_false" greater_test_false "" "false"
  ; t "greater_equal_false" greater_equal_false "" "false"
  ; t "less_true" less_test_true "" "true"
  ; t "less_false" less_test_false "" "false"
  ; t "less_equal_false" less_equal_false "" "false"
  ; t "greatereq_true" greatereq_test_true "" "true"
  ; t "greatereq_false" greatereq_test_false "" "false"
  ; t "greatereq_equal_true" greatereq_equal_true "" "true"
  ; t "lesseq_true" lesseq_test_true "" "true"
  ; t "lesseq_false" lesseq_test_false "" "false"
  ; t "lesseq_equal_true" lesseq_equal_true "" "true"
  ; t "eq_true" eq_test_true "" "true"
  ; t "eq_false" eq_test_false "" "false"
  ; t "eq_true_bool" eq_test_true_bool "" "true"
  ; t "eq_false_bool" eq_test_false_bool "" "false"
  ; t "eq_test_num_bool" eq_test_num_and_bool "" "false"
  ; t "print_true" print_true "" "true\ntrue"
  ; t "print_false" print_false "" "false\nfalse"
  ; t "print_false_eq" print_false_eq "" "false\nfalse"
  ; t "print_true_eq" print_true_eq "" "true\ntrue"
  ; t "if_true" if_true "" "3"
  ; t "if_true_eq" if_true_eq "" "3"
  ; t "if_false" if_false "" "4"
  ; t "if_false_eq" if_false_eq "" "4"
  ; t "true_base" true_base "" "true"
  ; t "false_base" false_base "" "false"
  ; t "and_short_circuit" and_short_circuit_test "" "false\nfalse"
  ; t "and_short_circuit_test_bad_type" and_short_circuit_test_bad_type "" "false\nfalse"
  ; t "or_short_circuit" or_short_circuit_test "" "true\ntrue"
  ; t "or_short_circuit_test_bad_type" or_short_circuit_test_bad_type "" "true\ntrue"
  ; t "if_run_only_true_branch" if_run_only_true_branch "" "true\ntrue"
  ; t "if_run_only_false_branch" if_run_only_false_branch "" "false\nfalse"
  ; t "three_and_test" three_and_test "" "false"
  ; t "three_or_test" three_or_test "" "true"
  ]
;;

let regression_compile_failing =
  [ terr "add1_err_nan" add1_test_err_nan "" nan_arith_err
  ; terr "not_err_nab" not_test_err_nab "" nab_logic_err
  ; terr "sub1_err_nan" sub1_test_err_nan "" nan_arith_err
  ; terr "plus_err" plus_test_err "" nan_arith_err
  ; terr "minus_err" minus_test_err "" nan_arith_err
  ; terr "times_err" times_test_err "" nan_arith_err
  ; terr "and_err" and_test_err "" nab_logic_err
  ; terr "or_err" or_test_err "" nab_logic_err
  ; terr "greater_err" greater_test_err "" nan_comp_err
  ; terr "less_err" greater_test_err "" nan_comp_err
  ; terr "greatereq_err" greatereq_test_err "" nan_comp_err
  ; terr "lesseq_err" lesseq_test_err "" nan_comp_err
  ; (* teprog "do_err/complicated_if_no_bools.cobra" nab_if_err; *)
    terr "overflow_plus" overflow_plus_err "" overflow_err
  ; terr "overflow_times" overflow_times_err "" overflow_err
  ; terr "overflow_sub" overflow_minus_err "" overflow_err
  ]
;;

let regression_func_tests =
  [ t "fun_simple" "def myfun(): 2 1" "" "1"
  ; t "fun_simple_2" "def myfun(): 2 myfun()" "" "2"
  ; t "fun_with_args" "def myfun(a): a + 2 myfun(3)" "" "5"
  ; t "fun_with_args_2" "def myfun(a): 2 + 2 myfun(3)" "" "4"
  ; t "fun_with_args_3" "def myfun(a): 2 + 2 def myfun2(): 4 myfun2()" "" "4"
  ; t
      "fun_with_3_args"
      "def myfun(a, b, c): (a && b) || c myfun(true, false, true)"
      ""
      "true"
  ; t
      "fun_with_3_args_print"
      "def myfun(a, b, c): (print(a) && print(b)) || print(c) myfun(true, false, true)"
      ""
      "true\nfalse\ntrue\ntrue"
  ; t
      "fact"
      "def fact(acc, n): if n < 1: acc else: fact(acc * n, n - 1) + 0 fact(1, 3)"
      ""
      "6"
  ; t
      "functions_calling_other_functions"
      "def myfun1(x, y): x + y def myfun2(): myfun1(1, 2) + 3 myfun2()"
      ""
      "6"
  ]
;;

let regression_well_formed_error_tests =
  [ terr
      "unbound_id"
      "x + 1"
      ""
      "The identifier x, used at <unbound_id, 1:0-1:1>, is not in scope"
  ; terr
      "unbound_fun"
      "myfun()"
      ""
      "The function name myfun, used at <unbound_fun, 1:0-1:7>, is not in scope"
  ; terr
      "dup_fun"
      ""
      "def myfun(): 1 and def myfun2(): 2 and def myfun(): 3 6"
      "The function name myfun, redefined at <dup_fun, 1:0-1:14>, duplicates one at \
       <dup_fun, 1:15-1:30>"
  ; terr
      "dup_var"
      ""
      "def myfun(x, x): 1 myfun()"
      "The identifier x, redefined at <dup_var, 1:10-1:11>, duplicates one at <dup_var, \
       1:13-1:14>"
  ; terr
      "arity_err_toofew"
      "def myfun(x, y): x + y myfun(2)"
      ""
      "The function called at <arity_err_toofew, 1:23-1:31> expected an arity of 2, but \
       received 1 arguments"
  ; terr
      "arity_err_toomany"
      "def myfun(x, y): x + y myfun(2, 3, 4)"
      ""
      "The function called at <arity_err_toomany, 1:23-1:37> expected an arity of 2, but \
       received 3 arguments"
  ; terr
      "mult_unbound_ids"
      "def myfun(x): y + 1 myfun(n) + x"
      ""
      "The identifier y, used at <mult_unbound_ids, 1:14-1:15>, is not in scope\n\
       The identifier n, used at <mult_unbound_ids, 1:26-1:27>, is not in scope\n\
       The identifier x, used at <mult_unbound_ids, 1:31-1:32>, is not in scope"
  ; terr
      "dup_fun_and_dup_var"
      ""
      "def myfun(): 1 and def myfun2(): y and def myfun(): 3 6"
      "The function name myfun, redefined at <dup_fun_and_dup_var, 1:0-1:14>, duplicates \
       one at <dup_fun_and_dup_var, 1:15-1:30>\n\
       The function name myfun, redefined at <dup_fun_and_dup_var, 1:0-1:14>, duplicates \
       one at <dup_fun_and_dup_var, 1:31-1:45>\n\
       The identifier y, used at <dup_fun_and_dup_var, 1:29-1:30>, is not in scope"
  ; terr
      "dup_fun_arrity"
      ""
      "def myfun(): 1 def myfun(x): 2 myfun(2)"
      "The function name myfun, redefined at <dup_fun_arrity, 1:0-1:14>, duplicates one \
       at <dup_fun_arrity, 1:15-1:30>\n\
       The function called at <dup_fun_arrity, 1:31-1:39> expected an arity of 0, but \
       received 1 arguments"
  ; terr
      "all_the_errors"
      ""
      "def myfun1(): 2 def myfun2(x, x): y + x def myfun1(): 2 myfun2(1) + myfun3()"
      "The function name myfun1, redefined at <all_the_errors, 1:0-1:15>, duplicates one \
       at <all_the_errors, 1:16-1:39>\n\
       The function name myfun1, redefined at <all_the_errors, 1:0-1:15>, duplicates one \
       at <all_the_errors, 1:40-1:55>\n\
       The identifier x, redefined at <all_the_errors, 1:27-1:28>, duplicates one at \
       <all_the_errors, 1:30-1:31>\n\
       The identifier y, used at <all_the_errors, 1:34-1:35>, is not in scope\n\
       The function called at <all_the_errors, 1:56-1:65> expected an arity of 2, but \
       received 1 arguments\n\
       The function name myfun3, used at <all_the_errors, 1:68-1:76>, is not in scope"
  ; terr
      "overflow"
      "9223372036854775805"
      ""
      "The number literal 9223372036854775805, used at <overflow, 1:0-1:19>, is not \
       supported in this language"
  ]
;;

let pair_tests =
  [ t
      "tup1"
      "let t = (4, (5, 6)) in\n\
      \            begin\n\
      \              t[0] := 7;\n\
      \              t\n\
      \            end"
      ""
      "(7, (5, 6))"
  ; t
      "tup2"
      "let t = (4, (5, nil)) in\n\
      \            begin\n\
      \              t[1] := nil;\n\
      \              t\n\
      \            end"
      ""
      "(4, nil)"
  ; t
      "tup3"
      "let t = (4, (5, nil)) in\n\
      \            begin\n\
      \              t[1] := t;\n\
      \              t\n\
      \            end"
      ""
      "(4, <cyclic tuple 1>)"
  ; t "tup4" "let t = (4, 6) in\n            (t, t)" "" "((4, 6), (4, 6))"
  ]
;;

let nested_tup_bindings_test =
  [ t "nested_let_1" "let (a, b, c) = (1, 2, 3) in a + b + c" "" "6"
  ; t
      "nested_let_2"
      "let (a, (_, _, b, (e, (f, _)), _), c) = (1, (2, 8, -2, (5, (8, ()))), 3) in a + b \
       + c + e + f"
      ""
      "15"
  ]
;;

let tuple_test =
  [ t "tup1" "(1, 0)[0]" "" "1"
  ; t "tup2" "(1, 9)[1]" "" "9"
  ; t "tup_access_from_tup" "(1, 9)[(1, 4)[0]]" "" "9"
  ; t "tup3" "(1, 9, 7, 1888, -1)[3]" "" "1888"
  ; t "tup_nested_access" "(1, 9, (29, 39, true), 1888, -1)[2][2]" "" "true"
  ; t "tup_print" "print((1, 0))" "" "(1, 0)\n(1, 0)"
  ; t
      "tup_ret_nested"
      "(1, 9, (29, 39, true), 1888, -1)"
      ""
      "(1, 9, (29, 39, true), 1888, -1)"
  ; t "tup_simple" "(1, 2)" "" "(1, 2)"
  ; t "tup_nil" "(1, nil)" "" "(1, nil)"
  ; t "tup_nil_access" "(1, nil)[0]" "" "1"
  ; t "tup_nil_access_nil" "(1, nil)[1]" "" "nil"
  ]
;;

let tuple_set_tests =
  [ t
      "tup_set_1"
      "let x = (1, 0), y = x[1] := 3, z = x[0] := 2 in equal(x, (2,3)) && (y == 3) && (z \
       == 2)"
      ""
      "true"
  ; terr
      "tup_set_too_high"
      "let x = (1, 0), y = x[2] := 3, z = x[0] := 2 in equal(x, (2,3)) && (y == 3) && (z \
       == 2)"
      ""
      "Error 8: Error: index too large to get"
  ; terr
      "tup_set_too_low"
      "let x = (1, 0), y = x[0] := 3, z = x[-1] := 2 in equal(x, (2,3)) && (y == 3) && \
       (z == 2)"
      ""
      "Error 7: Error: index too small to get"
  ]
;;

let tuple_error_test =
  [ terr "tup_access_too_big" "(1, 0)[2]" "" "Error 8: Error: index too large to get"
  ; terr "tup_access_too_small" "(1, 0)[-1]" "" "Error 7: Error: index too small to get"
  ; terr "tup_access_nil_1" "nil[1]" "" "access component of nil"
  ; terr
      "tup_access_too_big_empty_tup"
      "()[0]"
      ""
      "Error 8: Error: index too large to get"
  ]
;;

let sequence_tests =
  [ t
      "seq_test_1"
      "let t = 4 in\n\
      \            begin\n\
      \              t + 1;\n\
      \              t\n\
      \            end"
      ""
      "4"
  ; t
      "seq_test_2"
      "let t = 4 in\n\
      \            begin\n\
      \              8 + 1;\n\
      \              9 * 2\n\
      \            end"
      ""
      "18"
  ]
;;

let equal_tests =
  [ t "equal_tup1" "let x = (1, 0) in equal(x,x)" "" "true"
  ; t
      "equal_tup_nested"
      "let x = (1, (2, 8, -2, (5, (8, ()))), 3) in equal(x,x)"
      ""
      "true"
  ; t "not_equal_tup1" "let x = (1, 0), y = (1, 1, 9) in equal(x,y)" "" "false"
  ; t
      "not_equal_tup_nested"
      "let x = (1, (2, 8, -2, (5, (8, ()))), 3), y = (1, (2, 8, -2, (5, (9, ()))), 3) in \
       equal(x,y)"
      ""
      "false"
  ]
;;

let function_desugaring_tests =
  [ t
      "desugar_pair_args"
      "def addPairs((x1, y1), (x2, y2)): (x1 + x2, y1 + y2) addPairs((1, 2), (3, 4))"
      ""
      "(4, 6)"
  ; t
      "desugar_nested_pair_args"
      "def addPairs((x1, (x2, y2), y1), z): (x1 + x2, y1 + y2, z) addPairs((1, (3, 4), \
       2), true)"
      ""
      "(4, 6, true)"
  ]
;;

let input_tests =
  [ t "input1" "let x = input() in x + 2" "123" "125"
  ; t "input2" "let x = input() in x && true" "true" "true"
  ; t "input3" "let x = input() in x" "false" "false"
  ; t "input4" "print(input())" "-3" "-3\n-3"
  ]
;;

let let_rec_tests =
  [ terr
      "let_rec_not_lambda"
      "let rec x = 10 in x"
      ""
      "Binding error at let_rec_not_lambda, 1:8-1:14: Let-rec expected a name binding to \
       a lambda; got x"
  ; terr
      "let_rec_dup_ids"
      "let rec x = (lambda(x): 1), x = (lambda(x): 2) in 3"
      ""
      "The identifier x, redefined at <let_rec_dup_ids, 1:28-1:29>, duplicates one at \
       <let_rec_dup_ids, 1:8-1:9>"
  ; terr
      "let_dup_ids"
      "let x = 3, x = 5 in x + x"
      ""
      "The identifier x, redefined at <let_dup_ids, 1:11-1:12>, duplicates one at \
       <let_dup_ids, 1:4-1:5>"
  ; terr
      "let_rec_not_all_lambdas"
      "let rec y = (lambda(x): x), x = 10 in x"
      ""
      "Binding error at let_rec_not_all_lambdas, 1:28-1:34: Let-rec expected a name \
       binding to a lambda; got x"
  ; terr
      "dup_func_args"
      "let rec x = (lambda(x, y, x): 4) in x"
      ""
      "The identifier x, redefined at <dup_func_args, 1:26-1:27>, duplicates one at \
       <dup_func_args, 1:20-1:21>"
  ]
;;

let arity_err_built_in =
  [ terr
      "print_arity_many_err"
      "print(1, 2)"
      ""
      "The function called at <print_arity_many_err, 1:0-1:11> expected an arity of 1, \
       but received 2 arguments"
  ; terr
      "print_arity_few_err"
      "print()"
      ""
      "The function called at <print_arity_few_err, 1:0-1:7> expected an arity of 1, but \
       received 0 arguments"
  ; terr
      "equal_arity_many_err"
      "equal(1, 2, 3)"
      ""
      "The function called at <equal_arity_many_err, 1:0-1:14> expected an arity of 2, \
       but received 3 arguments"
  ; terr
      "equal_arity_few"
      "equal(1)"
      ""
      "The function called at <equal_arity_few, 1:0-1:8> expected an arity of 2, but \
       received 1 arguments"
  ; terr
      "input_arity_many"
      "input(1, 2)"
      ""
      "The function called at <input_arity_many, 1:0-1:11> expected an arity of 0, but \
       received 2 arguments"
  ]
;;

let old_suite =
  [ t "test_is_bool1" "isbool(true)" "" "true"
  ; t "test_is_bool2" "isbool(false)" "" "true"
  ; t "test_is_bool3" "isbool(0)" "" "false"
  ; t "test_is_bool4" "isbool(123)" "" "false"
  ; t "test_is_bool5" "isbool((0,123))" "" "false"
  ; t "test_is_bool6" "isbool((true,123))" "" "false"
  ; t "test_is_bool7" "isbool((123,123))" "" "false"
  ; t "test_is_bool8" "isbool((false,123))" "" "false"
  ; t "test_is_tuple1" "istuple(true)" "" "false"
  ; t "test_is_tuple2" "istuple(false)" "" "false"
  ; t "test_is_tuple3" "istuple(0)" "" "false"
  ; t "test_is_tuple4" "istuple(123)" "" "false"
  ; t "test_is_tuple5" "istuple((0,123))" "" "true"
  ; t "test_is_tuple6" "istuple((true,123))" "" "true"
  ; t "test_is_tuple7" "istuple((123,123))" "" "true"
  ; t "test_is_tuple8" "istuple((false,123))" "" "true"
  ; t "test_is_num1" "isnum(true)" "" "false"
  ; t "test_is_num2" "isnum(false)" "" "false"
  ; t "test_is_num3" "isnum(0)" "" "true"
  ; t "test_is_num4" "isnum(123)" "" "true"
  ; t "test_is_num5" "isnum((0,123))" "" "false"
  ; t "test_is_num6" "isnum((true,123))" "" "false"
  ; t "test_is_num7" "isnum((123,123))" "" "false"
  ; t "test_is_num8" "isnum((false,123))" "" "false"
  ; tanf "forty_one_anf" (Program ([], ENumber (41L, ()), ())) "" forty_one_a
  ; (* terr "scope_err1" "let x = true in (let y = (let x = false in x) in y)"
       "" "shadows one defined"; This is optional so we're just letting everything be shadowable*)
    (* ta "forty_one_run_anf" (atag forty_one_a, []) "" "41"; *)
    t "forty_one" forty_one "" "41"
  ; t "test" test_prog "" "3"
  ; (* Some useful if tests to start you off *)
    t "if1" "if 7 < 8: 5 else: 3" "" "5"
  ; t "if2" "if 0 > 1: 4 else: 2" "" "2"
  ; terr "overflow" "add1(5073741823000000000)" "" "overflow"
  ; t "funcalls" "def fact(n): if n < 2: 1 else: n * fact(n - 1)\n\nfact(5)" "" "120"
  ; t "outer_scope" "let x = 3, z = 3 in (lambda(y): x + y + z)(2)" "" "8"
  ; t
      "lambda_with_many_unused_outer_scope"
      "let a = 1, b = 2, c = 3, d = 4, e = 5 in let f = (lambda: a + e) in f()"
      ""
      "6"
  ; t
      "lambdas_with_unused_outer_scope"
      "let x = 5 in let y = 6 in let z = y in let f = (lambda: x + z) in let h = \
       (lambda: z + y) in f()"
      ""
      "11"
  ]
  @ regression_compile_failing
  @ regression_compile_failing
  @ regression_func_tests
  (*regression_well_formed_error_tests @*) @ tuple_test
  @ tuple_error_test
  @ nested_tup_bindings_test
  @ sequence_tests
  @ input_tests
  @ tuple_set_tests
  @ equal_tests
  @ function_desugaring_tests
  @ let_rec_tests
  @ arity_err_built_in
;;

let pair_tests =
  [ t
      "tup1"
      "let t = (4, (5, 6)) in\n\
      \            begin\n\
      \              t[0] := 7;\n\
      \              t\n\
      \            end"
      ""
      "(7, (5, 6))"
  ; t
      "tup2"
      "let t = (4, (5, nil)) in\n\
      \            begin\n\
      \              t[1] := nil;\n\
      \              t\n\
      \            end"
      ""
      "(4, nil)"
  ; t "tup3" "let t = (4, 6) in\n            (t, t)" "" "((4, 6), (4, 6))"
  ]
;;

let oom =
  [ tgcerr "oomgc1" (7 + builtins_size) "(1, (3, 4))" "" "Out of memory"
  ; tgc "oomgc2" true (8 + builtins_size) "(1, (3, 4))" "" "(1, (3, 4))"
  ; tvgc "oomgc3" (8 + builtins_size) "(1, (3, 4))" "" "(1, (3, 4))"
  ; tgc "oomgc4" true (4 + builtins_size) "(3, 4)" "" "(3, 4)"
  ; tgc
      "oomgc5"
      true
      (3 + builtins_size)
      "(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)"
      ""
      "(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)"
  ; tgcerr
      "oomgc6"
      7
      "(1, (3, 4))"
      ""
      "Out of memory: needed 4 words, but only 3 remain after collection"
  ]
;;

let gc =
  [ tgc
      "gc_lam1"
      false
      10
      "let f = (lambda: (1, 2)) in\n\
      \       begin\n\
      \         f();\n\
      \         f();\n\
      \         f();\n\
      \         f()\n\
      \       end"
      ""
      "(1, 2)"
    (* ; tgc
      "gc_lam_with_string"
      false
      10
      "let f = (lambda: \"this is a funky string\") in\n\
      \       begin\n\
      \         f();\n\
      \         f();\n\
      \         f()\n\
      \       end"
      ""
      "this is a funky string" *)
  ]
;;

let interference_mock_pipeline prog =
  prog
  |> is_well_formed
  |> Result.get_ok
  |> add_native_lambdas
  |> desugar
  |> tag
  |> rename_and_tag
  |> anf
  |> atag
  |> free_vars_cache
  |> fun p ->
  match p with
  | AProgram (aexp, _) -> interfere aexp StringSet.empty |> string_of_graph
;;

let color_graph_mock_pipeline include_builtins prog =
  prog
  |> is_well_formed
  |> Result.get_ok
  |> (fun a -> if include_builtins then a |> add_native_lambdas else a)
  |> desugar
  |> tag
  |> rename_and_tag
  |> anf
  |> atag
  |> free_vars_cache
  |> fun p ->
  match p with
  | AProgram (aexp, _) ->
    let g = interfere aexp StringSet.empty in
    print_env
      (let env, _ = color_graph g [] 1 in
       env)
      arg_to_asm
;;

let simple_prog_graph =
  "3 + 4" |> parse_string "simple_prog" |> interference_mock_pipeline
;;

let prog_with_lets_but_vars_unused =
  "let x = true, y = false, z = 7, a = (1, 2) in 1 + 2"
  |> parse_string "prog_with_lets_but_vars_unused"
  |> interference_mock_pipeline
;;

let simple_let_prog =
  "let x = 1, y = 3 in (x, y)"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let simple_let_prog_let_ref_itself =
  "let x = 1, y = x + 3 in x"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let simple_nested_let =
  "let x = 1 in let y = 2 in 3"
  |> parse_string "simple_nested_let"
  |> interference_mock_pipeline
;;

let simple_nested_let_2 =
  "let x = 1, y = 2, z = x in 3"
  |> parse_string "simple_nested_let_2"
  |> interference_mock_pipeline
;;

let nested_lets =
  "let x = 1, y = x + 3 in let z = y + 7 + x, g = let q = z + x + 4 in q, r = 3 in z + g \
   + r"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let simple_lambda =
  "(lambda(x, y): x + y)" |> parse_string "simple_let_prog" |> interference_mock_pipeline
;;

let lambda_with_edges =
  "let y = 3, z = 4 in (lambda(x): x + y + z)"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let if_test =
  "if 2 > 1: let x = 1 in x else: let y = 2 in y"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let simple_let_rec =
  "let rec x = (lambda: 1), y = (lambda: 2) in x + y"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let let_rec_using_vars =
  "let a = 5 in let rec x = (lambda(z): a + z), y = (lambda: x) in x"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let live_test =
  "let x = true in let y = if true: let b = 5 in b else: 6 in x"
  |> parse_string "simple_let_prog"
  |> interference_mock_pipeline
;;

let let_using_builtins =
  "let a = input(), b = print(a), c = equal(a, b) in a && b && c"
  |> parse_string "let_using_builtins_graph_coloring"
  |> interference_mock_pipeline
;;

let buncha_free_vars =
  "let a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9, j = 10, k = 11, l \
   = 12 in (lambda: a + b + c + d + e + f + g + h + i + j + k + l)"
  |> parse_string "let_using_builtins_graph_coloring"
  |> interference_mock_pipeline
;;

let nested_tup_equal =
  "let x = (1, (2, 8, -2, (5, (8, ()))), 3), y = (1, (2, 8, -2, (5, (9, ()))), 3) in \
   equal(x,y)"
  |> parse_string "nested_tup"
  |> interference_mock_pipeline
;;

let input_print_call =
  "print(input())" |> parse_string "print_input" |> interference_mock_pipeline
;;

let interference =
  [ teq "simple_interference_no_edges" simple_prog_graph ""
  ; teq
      "lets_but_vars_unused"
      prog_with_lets_but_vars_unused
      "a_128: \nx_116: \ny_120: \nz_124: "
  ; teq "simple_let_2_vars_both_used" simple_let_prog "x_116: y_120\ny_120: x_116"
  ; teq
      "simple_let_prog_let_ref_itself"
      simple_let_prog_let_ref_itself
      "x_116: y_120\ny_120: x_116"
  ; teq "simple_nested_let" simple_nested_let "x_116: \ny_120: "
  ; teq "simple_nested_let_2" simple_nested_let_2 "x_116: y_120\ny_120: x_116\nz_124: "
  ; teq "simple_lambda" simple_lambda ""
  ; teq "lambda_with_edges" lambda_with_edges "y_116: z_120\nz_120: y_116"
  ; teq "if_test" if_test "binop_115: \nx_120: \ny_125: "
  ; teq "simple_let_rec" simple_let_rec "x_116: y_120\ny_120: x_116"
  ; teq
      "let_rec_using_vars"
      let_rec_using_vars
      "a_116: y_128, x_120\n\
       equal_92: x_120\n\
       input_102: x_120\n\
       print_108: x_120\n\
       snakeStringCmp_82: x_120\n\
       snakeStringConcat_72: x_120\n\
       snakeStringContains_16: x_120\n\
       snakeStringIdxOf_26: x_120\n\
       snakeStringReplace_4: x_120\n\
       snakeStringSubstring_60: x_120\n\
       snakeStringToLower_44: x_120\n\
       snakeStringToUpper_52: x_120\n\
       snakeStringTrim_36: x_120\n\
       x_120: y_128, snakeStringTrim_36, snakeStringToUpper_52, snakeStringToLower_44, \
       snakeStringSubstring_60, snakeStringReplace_4, snakeStringIdxOf_26, \
       snakeStringContains_16, snakeStringConcat_72, snakeStringCmp_82, print_108, \
       input_102, equal_92, a_116\n\
       y_128: x_120, a_116"
  ; teq
      "nested_lets"
      nested_lets
      "binop_128: x_116\n\
       binop_139: z_126\n\
       binop_149: r_146\n\
       g_134: z_126, r_146\n\
       q_137: z_126\n\
       r_146: z_126, g_134, binop_149\n\
       x_116: z_126, y_120, binop_128\n\
       y_120: x_116\n\
       z_126: x_116, r_146, q_137, g_134, binop_139"
  ; teq "live_test" live_test "b_125: x_116\nx_116: y_120, b_125\ny_120: x_116"
  ; teq
      "let_using_builtins"
      let_using_builtins
      "a_116: print_108, equal_92, c_127, b_121\n\
       and_133: c_127\n\
       b_121: equal_92, c_127, a_116\n\
       c_127: b_121, and_133, a_116\n\
       equal_92: print_108, input_102, b_121, a_116\n\
       input_102: print_108, equal_92\n\
       print_108: input_102, equal_92, a_116"
  ; teq
      "buncha_free_vars"
      buncha_free_vars
      "a_116: l_160, k_156, j_152, i_148, h_144, g_140, f_136, e_132, d_128, c_124, b_120\n\
       b_120: l_160, k_156, j_152, i_148, h_144, g_140, f_136, e_132, d_128, c_124, a_116\n\
       c_124: l_160, k_156, j_152, i_148, h_144, g_140, f_136, e_132, d_128, b_120, a_116\n\
       d_128: l_160, k_156, j_152, i_148, h_144, g_140, f_136, e_132, c_124, b_120, a_116\n\
       e_132: l_160, k_156, j_152, i_148, h_144, g_140, f_136, d_128, c_124, b_120, a_116\n\
       f_136: l_160, k_156, j_152, i_148, h_144, g_140, e_132, d_128, c_124, b_120, a_116\n\
       g_140: l_160, k_156, j_152, i_148, h_144, f_136, e_132, d_128, c_124, b_120, a_116\n\
       h_144: l_160, k_156, j_152, i_148, g_140, f_136, e_132, d_128, c_124, b_120, a_116\n\
       i_148: l_160, k_156, j_152, h_144, g_140, f_136, e_132, d_128, c_124, b_120, a_116\n\
       j_152: l_160, k_156, i_148, h_144, g_140, f_136, e_132, d_128, c_124, b_120, a_116\n\
       k_156: l_160, j_152, i_148, h_144, g_140, f_136, e_132, d_128, c_124, b_120, a_116\n\
       l_160: k_156, j_152, i_148, h_144, g_140, f_136, e_132, d_128, c_124, b_120, a_116"
  ; teq
      "nested_tup_equals_interference"
      nested_tup_equal
      "equal_92: y_131, x_116, tup_142, tup_140, tup_138, tup_134, tup_127, tup_125, \
       tup_123, tup_119, print_108, input_102\n\
       input_102: equal_92\n\
       print_108: equal_92\n\
       tup_119: equal_92\n\
       tup_123: equal_92\n\
       tup_125: equal_92\n\
       tup_127: equal_92\n\
       tup_134: x_116, equal_92\n\
       tup_138: x_116, equal_92\n\
       tup_140: x_116, equal_92\n\
       tup_142: x_116, equal_92\n\
       x_116: y_131, tup_142, tup_140, tup_138, tup_134, equal_92\n\
       y_131: x_116, equal_92"
  ; teq
      "print_input_call"
      input_print_call
      "app_115: print_108\ninput_102: print_108\nprint_108: input_102, app_115"
  ]
;;

let simple_prog_graph_coloring =
  "3 + 4" |> parse_string "simple_prog_graph_coloring" |> color_graph_mock_pipeline false
;;

let prog_with_lets_but_vars_unused_graph_coloring =
  "let x = true, y = false, z = 7, a = (1, 2) in 1 + 2"
  |> parse_string "prog_with_lets_but_vars_unused"
  |> color_graph_mock_pipeline false
;;

let simple_let_prog_graph_coloring =
  "let x = 1, y = 3 in (x, y)"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline false
;;

let simple_let_prog_let_ref_itself_graph_coloring =
  "let x = 1, y = x + 3 in x"
  |> parse_string "simple_let_prog_graph_coloring"
  |> color_graph_mock_pipeline false
;;

let simple_nested_let_graph_coloring =
  "let x = 1 in let y = 2 in 3"
  |> parse_string "simple_nested_let_graph_coloring"
  |> color_graph_mock_pipeline false
;;

let simple_nested_let_2_graph_coloring =
  "let x = 1, y = 2, z = x in 3"
  |> parse_string "simple_nested_let_2_graph_coloring"
  |> color_graph_mock_pipeline false
;;

let nested_lets_graph_coloring =
  "let x = 1, y = x + 3 in let z = y + 7 + x, g = let q = z + x + 4 in q, r = 3 in z + g \
   + r"
  |> parse_string "simple_let_prog_graph_coloring"
  |> color_graph_mock_pipeline false
;;

let simple_lambda_graph_coloring =
  "(lambda(x, y): x + y)"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline false
;;

let lambda_with_edges_graph_coloring =
  "let y = 3, z = 4 in (lambda(x): x + y + z)"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline false
;;

let if_test_graph_coloring =
  "if 2 > 1: let x = 1 in x else: let y = 2 in y"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline false
;;

let simple_let_rec_graph_coloring =
  "let rec x = (lambda: 1), y = (lambda: 2) in x + y"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline false
;;

let let_rec_using_vars_graph_coloring =
  "let a = 5 in let rec x = (lambda(z): a + z), y = (lambda: x) in x"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline false
;;

let let_using_builtins_graph_coloring =
  "let a = input(), b = print(a), c = equal(a, b) in a && b && c"
  |> parse_string "let_using_builtins_graph_coloring"
  |> color_graph_mock_pipeline true
;;

let deep_primop_thingy_graph_coloring =
  "1 + 2 + 3 - 4 * 5 + 6 + 7 * 8 + 9 + 10"
  |> parse_string "let_using_builtins_graph_coloring"
  |> color_graph_mock_pipeline false
;;

let buncha_free_vars_go_to_stack_space_graph_coloring =
  "let a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9, j = 10, k = 11, l \
   = 12 in (lambda: a + b + c + d + e + f + g + h + i + j + k + l)"
  |> parse_string "buncha_free_vars_go_to_stack_space_graph_coloring"
  |> color_graph_mock_pipeline false
;;

let outer_scope_graph_coloring =
  "let x = 3, z = 3 in (lambda(y): x + y + z)(2)"
  |> parse_string "outer_scope"
  |> color_graph_mock_pipeline false
;;

let nested_tup_equal_graph_coloring =
  "let x = (1, (2, 8, -2, (5, (8, ()))), 3), y = (1, (2, 8, -2, (5, (9, ()))), 3) in \
   equal(x,y)"
  |> parse_string "nested_tup"
  |> color_graph_mock_pipeline true
;;

let input_print_call_graph_coloring =
  "print(input())" |> parse_string "print_input" |> color_graph_mock_pipeline true
;;

let live_test_graph_coloring =
  "let x = true in let y = if true: let b = 5 in b else: 6 in x"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline true
;;

let three_nested_let_last_one_live_graph_coloring =
  "let x = 44 * input() in\nlet y = 10 in\nlet z = x + y in\nprint(z)"
  |> parse_string "simple_let_prog"
  |> color_graph_mock_pipeline true
;;

let many_nested_ifs_live_graph_coloring =
  "let x = 7, y = 5, z = (let b = 8 + (if true: let z = (1,2)[1] in z else: 4) in b + 7) \
   in x + y + z"
  |> parse_string "many_nested_ifs_live_graph_coloring"
  |> color_graph_mock_pipeline true
;;

(* let testing_tests = [ teq "testing_test" nested_tup_equal_graph_coloring "" ] *)

(* let testing_tests =
  [ (let str, _ = Result.get_ok (compile_string_to_string Register "" "print(input())") in
     teq "bruh" str "7")
  ]
;; *)

let graph_coloring =
  [ teq "simple_prog_graph_coloring" simple_prog_graph_coloring ""
  ; teq
      "prog_with_lets_but_vars_unused_graph_coloring"
      prog_with_lets_but_vars_unused_graph_coloring
      "a_16 -> R10\nx_4 -> R10\ny_8 -> R10\nz_12 -> R10"
  ; teq
      "simple_let_prog_graph_coloring"
      simple_let_prog_graph_coloring
      "x_4 -> R12\ny_8 -> R10"
  ; teq
      "simple_let_prog_let_ref_itself_graph_coloring"
      simple_let_prog_let_ref_itself_graph_coloring
      "x_4 -> R12\ny_8 -> R10"
  ; teq
      "simple_nested_let_graph_coloring"
      simple_nested_let_graph_coloring
      "x_4 -> R10\ny_8 -> R10"
  ; teq
      "simple_nested_let_2_graph_coloring"
      simple_nested_let_2_graph_coloring
      "z_12 -> R10\nx_4 -> R12\ny_8 -> R10"
  ; teq
      "nested_lets_graph_coloring"
      nested_lets_graph_coloring
      "binop_16 -> R10\n\
       binop_27 -> R12\n\
       binop_37 -> R10\n\
       q_25 -> R12\n\
       y_8 -> R10\n\
       g_22 -> R13\n\
       r_34 -> R12\n\
       x_4 -> R12\n\
       z_14 -> R10"
  ; teq "simple_lambda_graph_coloring" simple_lambda_graph_coloring ""
  ; teq
      "lambda_with_edges_graph_coloring"
      lambda_with_edges_graph_coloring
      "y_4 -> R12\nz_8 -> R10"
  ; teq
      "if_test_graph_coloring"
      if_test_graph_coloring
      "binop_3 -> R10\nx_8 -> R10\ny_13 -> R10"
  ; teq
      "simple_let_rec_graph_coloring"
      simple_let_rec_graph_coloring
      "x_4 -> R12\ny_8 -> R10"
  ; teq
      "let_rec_using_vars_graph_coloring"
      let_rec_using_vars_graph_coloring
      "a_4 -> R13\nx_8 -> R12\ny_16 -> R10"
  ; teq
      "let_using_builtins_graph_coloring"
      let_using_builtins_graph_coloring
      "and_133 -> R12\n\
       input_102 -> R12\n\
       b_121 -> R13\n\
       c_127 -> R10\n\
       print_108 -> R13\n\
       a_116 -> R12\n\
       equal_92 -> R10"
  ; teq
      "deep_primop_thingy_graph_coloring"
      deep_primop_thingy_graph_coloring
      "binop_10 -> R10\n\
       binop_3 -> R10\n\
       binop_4 -> R10\n\
       binop_5 -> R10\n\
       binop_6 -> R10\n\
       binop_7 -> R10\n\
       binop_8 -> R10\n\
       binop_9 -> R10"
  ; teq
      "buncha_free_vars_go_to_stack_space_graph_coloring"
      buncha_free_vars_go_to_stack_space_graph_coloring
      "a_4 -> [RBP+56]\n\
       b_8 -> [RBP+48]\n\
       c_12 -> [RBP+40]\n\
       d_16 -> [RBP+32]\n\
       e_20 -> [RBP+24]\n\
       f_24 -> [RBP+16]\n\
       g_28 -> [RBP+8]\n\
       h_32 -> RBX\n\
       i_36 -> R14\n\
       j_40 -> R13\n\
       k_44 -> R12\n\
       l_48 -> R10"
  ; teq
      "outer_scope_graph_coloring"
      outer_scope_graph_coloring
      "lam_12 -> R10\nx_4 -> R12\nz_8 -> R10"
  ; teq
      "nested_tup_equal_graph_coloring"
      nested_tup_equal_graph_coloring
      "input_102 -> R12\n\
       print_108 -> R12\n\
       tup_119 -> R12\n\
       tup_123 -> R12\n\
       tup_125 -> R12\n\
       tup_127 -> R12\n\
       tup_134 -> R13\n\
       tup_138 -> R13\n\
       tup_140 -> R13\n\
       tup_142 -> R13\n\
       y_131 -> R13\n\
       x_116 -> R12\n\
       equal_92 -> R10"
  ; teq
      "print_input_graph_coloring"
      input_print_call_graph_coloring
      "app_115 -> R12\ninput_102 -> R12\nprint_108 -> R10"
  ; teq
      "live_test_graph_coloring"
      live_test_graph_coloring
      "b_125 -> R12\ny_120 -> R12\nx_116 -> R10"
  ; teq
      "live_test_graph_coloring"
      three_nested_let_last_one_live_graph_coloring
      "app_119 -> R12\n\
       input_102 -> R12\n\
       z_127 -> R12\n\
       x_116 -> R13\n\
       y_123 -> R12\n\
       print_108 -> R10"
  ; teq
      "many_nested_ifs_live_graph_coloring"
      many_nested_ifs_live_graph_coloring
      "binop_146 -> R10\n\
       b_127 -> R13\n\
       tup_137 -> R14\n\
       z_124 -> R13\n\
       z_134 -> R14\n\
       if_130 -> R13\n\
       x_116 -> R12\n\
       y_120 -> R10"
  ]
;;

let reg_alloc_tests =
  [ t
      "live_test_running"
      "let x = true in let y = if true: let b = 5 in b else: 6 in x"
      ""
      "true"
  ; t
      "live_test_using_y"
      "let x = true in let y = if true: let b = 5 in b else: 6 in y"
      ""
      "5"
  ; t
      "three_nested_let_last_one_live"
      "let x = 44 * input() in\nlet y = 10 in\nlet z = x + y in\nprint(z)"
      "100"
      "4410\n4410"
  ; t
      "many_nested_ifs_live_graph_coloring"
      "let x = 7, y = 5, z = (let b = 8 + (if true: let z = (1,2)[1] in z else: 4) in b \
       + 7) in x + y + z"
      ""
      "29"
  ; t "lambda_no_let_rec" "(lambda(x): x + 1)(2)" "" "3"
  ; t
      "lambda_with_lets"
      "(lambda(x): let y = 3, z = let a = true in a in if z: y else: x)(2)"
      ""
      "3"
  ; t
      "lambda_with_lets_2"
      "(lambda(x): let y = 3, z = let a = false in a in if z: y else: x)(2)"
      ""
      "2"
  ; t
      "lets_and_lambdas"
      "let a = (lambda(b): b + 5) in let g = (lambda(h): 3) in (lambda(d): a(6) + \
       g(true) + d)(9)"
      ""
      "23"
  ; t
      "lots_of_let_recs"
      "let rec a = (lambda(b): b + 2), b = (lambda(z): z + 4), c = (lambda(z): z + 6) in \
       let rec d = (lambda(x): x + 20), e = (lambda(x): if x: 2 else: 3) in e(true) + \
       d(0) + c(3) + b(4) + a(5)"
      ""
      "46"
  ; t
      "lots_of_let_recs_and_let_and_lambda"
      "let rec a = (lambda(b): b + 2), b = (lambda(z): z + 4), c = (lambda(z): z + 6) in \
       let rec d = (lambda(x): x + 20), e = (lambda(x): if x: 2 else: 3) in e(false) + \
       d(0) + c(3) + b(4) + a(5) + (lambda(j): j)(1000)"
      ""
      "1047"
  ]
;;

let string_tests_error =
  [ terr
      "err_ptr_to_components_number"
      "snakeStringToUpper(1)"
      ""
      "Error 18: Error: Expected a string value: 18, instead got val: 1"
  ; terr
      "err_ptr_to_components_bool"
      "snakeStringToUpper(true)"
      ""
      "Error 18: Error: Expected a string value: 18, instead got val: true"
  ; terr
      "err_ptr_to_components_bool_false"
      "snakeStringToUpper(false)"
      ""
      "Error 18: Error: Expected a string value: 18, instead got val: false"
  ; terr
      "err_ptr_to_components_tuple"
      "snakeStringToUpper((\"hi\", 3))"
      ""
      "Error 18: Error: Expected a string value: 18, instead got val: (hi, 3)"
  ; terr
      "err_idxOf_notFound"
      "snakeStringIdxOf(\"my string\", \"z\")"
      ""
      "Error 20: Error: Substring not found in given string"
  ; terr
      "err_idxOf_notFound_2"
      "snakeStringIdxOf(\"my string\", \"my stringm\")"
      ""
      "Error 19: Error: Substring requires that start/end > 0, start <= end, and end <= \
       len of str: 19"
  ; terr
      "err_substring_neg_idx"
      "snakeStringSubstring(\"my string\", -1, 5)"
      ""
      "Error 19: Error: Substring requires that start/end > 0, start <= end, and end <= \
       len of str: 19"
  ; terr
      "err_substring_first_idx_after_second"
      "snakeStringSubstring(\"my string\", 7, 5)"
      ""
      "Error 19: Error: Substring requires that start/end > 0, start <= end, and end <= \
       len of str: 19"
  ; terr
      "err_substring_out_of_bounds"
      "snakeStringSubstring(\"my string\", 7, 10)"
      ""
      "Error 19: Error: Substring requires that start/end > 0, start <= end, and end <= \
       len of str: 19"
  ]
;;

let string_tests_passing =
  [ teq
      "string_test_1"
      (ast_of_pos_program (parse_string "bruh" "\"hello there!\""))
      "\nEString<bruh, 1:13-1:14>(hello there!)"
  ; t "string_test_only_string" "\"hello there\"" "" "hello there"
  ; t
      "string_test_only_string_much_longer"
      "\"hello there my name is joe. How are you? This is a loooooooooooooong string. \n\
      \ even has a newline and slashes \\ \t and tabs\""
      ""
      "hello there my name is joe. How are you? This is a loooooooooooooong string. \n\
      \ even has a newline and slashes \\ \t and tabs"
  ; t
      "string_test_concat"
      "let a = \"hello \", b = \"there\" in snakeStringConcat(a, b)"
      ""
      "hello there"
  ; t "string_test_cmp_same" "snakeStringCmp(\"abcdefg\", \"abcdefg\")" "" "0"
  ; t "string_test_cmp_empty_str" "snakeStringCmp(\"\", \"\")" "" "0"
  ; t "string_test_cmp_less_than" "snakeStringCmp(\"abcde\", \"abcdefg\")" "" "-1"
  ; t "string_test_cmp_less_than_empty_str" "snakeStringCmp(\"\", \"abcdefg\")" "" "-1"
  ; t "string_test_cmp_greater_than" "snakeStringCmp(\"abcdefg\", \"abcd\")" "" "1"
  ; t "string_test_cmp_greater_than_empty_str" "snakeStringCmp(\"abcdefg\", \"\")" "" "1"
  ; t "string_test_cmp_same_len_g_t" "snakeStringCmp(\"gggaazz\", \"gggaaaa\")" "" "25"
  ; t "string_test_cmp_same_len_l_t" "snakeStringCmp(\"gggaaaa\", \"gggaazz\")" "" "-25"
  ; t
      "string_test_substring"
      "snakeStringSubstring(\"what is good everyone\", 13, 21)"
      ""
      "everyone"
  ; t
      "string_test_substring_same_start_end"
      "snakeStringSubstring(\"what is good everyone\", 1, 1)"
      ""
      ""
  ; t
      "string_test_substring_one_char"
      "snakeStringSubstring(\"what is good everyone\", 0, 1)"
      ""
      "w"
  ; t
      "string_test_substring_spaces"
      "snakeStringSubstring(\"what is good everyone\", 1, 10)"
      ""
      "hat is go"
  ; t
      "string_test_to_upper"
      "snakeStringToUpper(\"whats up this is now uppercase\")"
      ""
      "WHATS UP THIS IS NOW UPPERCASE"
  ; t
      "string_test_to_upper_some_already"
      "snakeStringToUpper(\"whats up THIS is NOW uppercasE\")"
      ""
      "WHATS UP THIS IS NOW UPPERCASE"
  ; t
      "string_test_to_lower"
      "snakeStringToLower(\"WHATS UP THIS IS NOW LOWERCASE\")"
      ""
      "whats up this is now lowercase"
  ; t
      "string_test_to_lower_some_already"
      "snakeStringToLower(\"wHATS Up THIS IS now LOWERCASE\")"
      ""
      "whats up this is now lowercase"
  ; t
      "string_test_trim"
      "snakeStringTrim(\"       lots of padding on either side    \")"
      ""
      "lots of padding on either side"
  ; t
      "string_test_trim_no_whitespace"
      "snakeStringTrim(\"no padding on either side\")"
      ""
      "no padding on either side"
  ; t
      "string_test_trim_lots_of_different_whitespace"
      "snakeStringTrim(\"  \n \t   \rmixed padding on either side \")"
      ""
      "mixed padding on either side"
  ; t "string_test_trim_all_whitespace" "snakeStringTrim(\"  \n \t     \r    \")" "" ""
  ; t
      "string_test_trim_left"
      "snakeStringTrim(\"  \n \t     \r    hello\tthere\")"
      ""
      "hello\tthere"
  ; t
      "string_test_trim_right"
      "snakeStringTrim(\"hello \n hello  \n   test  \n \t     \r    \")"
      ""
      "hello \n hello  \n   test"
  ; t "string_test_idxof" "snakeStringIdxOf(\"hello\", \"e\")" "" "1"
  ; t "string_idxof_gets_first" "snakeStringIdxOf(\"hello\", \"l\")" "" "2"
  ; t "string_idxof_test" "snakeStringIdxOf(\"hello\", \"h\")" "" "0"
  ; t "string_idxof_gets_last" "snakeStringIdxOf(\"hello\", \"o\")" "" "4"
  ; t
      "string_idxof_multiword"
      "snakeStringIdxOf(\"hello there hella there\", \"a\")"
      ""
      "16"
  ; t "idxof_empty" "snakeStringIdxOf(\"hello there hella there\", \"\")" "" "0"
  ; t
      "idxof_multichar_whole_thing"
      "snakeStringIdxOf(\"hello there hella there\", \"hello there hella there\")"
      ""
      "0"
  ; t "idxof_multichar" "snakeStringIdxOf(\"hello there hella there\", \"hella\")" "" "12"
  ; t
      "idxof_multichar_with_space"
      "snakeStringIdxOf(\"hello there hella there x\", \" x\")"
      ""
      "23"
  ; t
      "idxof_multichar_exists_twice"
      "snakeStringIdxOf(\"hello there hella there x\", \"there\")"
      ""
      "6"
  ; t "string_contains_test" "snakeStringContains(\"hello\", \"e\")" "" "true"
  ; t "string_contains_false_test" "snakeStringContains(\"hello\", \"r\")" "" "false"
  ; t "string_contains_multichar_test" "snakeStringContains(\"hello\", \"ell\")" "" "true"
  ; t
      "string_contains_multichar_false_test"
      "snakeStringContains(\"hello\", \"ellop\")"
      ""
      "false"
  ; t "string_contains_empty" "snakeStringContains(\"hello there\", \"\")" "" "true"
  ; t
      "string_replace_first"
      "snakeStringReplace(\"hello there\", \"h\", \"t\")"
      ""
      "tello there"
  ; t
      "string_replace_middle"
      "snakeStringReplace(\"hello there\", \"lo th\", \"mn\")"
      ""
      "helmnere"
  ; t
      "string_replace_end"
      "snakeStringReplace(\"hello there\", \"re\", \"are\")"
      ""
      "hello theare"
  ; t
      "string_replace_empty_to_replace"
      "snakeStringReplace(\"hello there\", \"\", \"wow\")"
      ""
      "wowhello there"
  ; t
      "string_replace_empty_replace_with"
      "snakeStringReplace(\"hello there\", \"ll\", \"\")"
      ""
      "heo there"
  ; t "builtin_equals_string_true_diff_ptrs" "equal(\"hello\", \"hello\")" "" "true"
  ; t "builtin_equals_string_true_same_ptr" "let a = \"hello\" in equal(a,a)" "" "true"
  ; t "builtin_equals_string_false" "let a = \"hello\" in equal(a,\"sup\")" "" "false"
  ; t
      "builtin_equals_string_false_one_nil"
      "let a = \"hello\" in equal(nil,\"sup\")"
      ""
      "false"
  ]
;;

let input = [ t "input1" "let x = input() in x + 2" "123" "125" ]

let () =
  run_test_tt_main
    ("all_tests"
    >::: string_tests_passing
         @ string_tests_error
         @ old_suite
         @ pair_tests
         (* @ oom *)
         @ gc
         @ input
         @ interference
         @ graph_coloring
         @ reg_alloc_tests
         @ [ input_file_test_suite () ])
;;
