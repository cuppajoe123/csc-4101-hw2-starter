open OUnit2
open Basics

let test_sanity _ =
  assert_equal 1 1 ~msg:"Custom error message"

let test_rev_tup _ =
  assert_equal ("b", "a") (rev_tup ("a", "b")) ~msg:"rev_tup: strings";
  assert_equal (true, false) (rev_tup (false, true)) ~msg:"rev_tup: booleans";
  assert_equal (1.5, -3.14) (rev_tup (-3.14, 1.5)) ~msg:"rev_tup: floats";
  assert_equal ("hello", 42) (rev_tup (42, "hello")) ~msg:"rev_tup: mixed int and string";
  assert_equal ((3, 4), (1, 2)) (rev_tup ((1, 2), (3, 4))) ~msg:"rev_tup: nested tuples";
  assert_equal (["c"; "d"], [1; 2]) (rev_tup ([1; 2], ["c"; "d"])) ~msg:"rev_tup: lists";
  assert_equal ([], []) (rev_tup ([], [])) ~msg:"rev_tup: empty lists"

let test_rev_triple _ =
  assert_equal ("c", "b", "a") (rev_triple ("a", "b", "c")) ~msg:"rev_triple: strings";
  assert_equal (-1.0, 0.0, 3.14) (rev_triple (3.14, 0.0, -1.0)) ~msg:"rev_triple: floats";
  assert_equal (true, 42, "hello") (rev_triple ("hello", 42, true)) ~msg:"rev_triple: mixed types";
  assert_equal (["c"], (3, 4), 1) (rev_triple (1, (3, 4), ["c"])) ~msg:"rev_triple: nested tuple and list";
  assert_equal (None, Some "cat", []) (rev_triple ([], Some "cat", None)) ~msg:"rev_triple: empty list and options"

let test_is_odd _ =
  assert_equal true (is_odd (-1)) ~msg:"is_odd: negative one";
  assert_equal true (is_odd (-7)) ~msg:"is_odd: negative odd";
  assert_equal false (is_odd (-10)) ~msg:"is_odd: negative even"

let test_is_older _ =
  assert_equal false (is_older (2022, 5, 10) (2022, 5, 10)) 
    ~msg:"is_older: identical dates";
  assert_equal true (is_older (2022, 3, 30) (2022, 4, 1)) 
    ~msg:"is_older: month is less";
  assert_equal false (is_older (2022, 10, 5) (2022, 9, 6)) 
    ~msg:"is_older: month is greater";
  assert_equal true (is_older (2023, 1, 31) (2023, 2, 1)) 
    ~msg:"is_older: end of month";
  assert_equal true (is_older (2023, 12, 31) (2024, 1, 1)) 
    ~msg:"is_older: end of year";
  assert_equal true (is_older (2024, 2, 29) (2024, 3, 1)) 
    ~msg:"is_older: leap day"

let test_to_us_format _ =
  assert_equal (1, 1, 2025) (to_us_format (2025, 1, 1)) ~msg:"to_us_format: first day of year";
  assert_equal (12, 31, 2024) (to_us_format (2024, 12, 31)) ~msg:"to_us_format: last day of year";
  assert_equal (2, 29, 2024) (to_us_format (2024, 2, 29)) ~msg:"to_us_format: leap day";
  assert_equal (5, 5, 2025) (to_us_format (2025, 5, 5)) ~msg:"to_us_format: same month and day"

let test_pow _ =
  assert_equal 1 (pow 10 0) ~msg:"pow: exponent is zero";
  assert_equal 1 (pow (-5) 0) ~msg:"pow: negative base, exponent is zero";
  assert_equal 1 (pow 0 0) ~msg:"pow: zero to the power of zero";
  assert_equal 2 (pow 2 1) ~msg:"pow: exponent is one";
  assert_equal 0 (pow 0 5) ~msg:"pow: base is zero";
  assert_equal 1 (pow 1 100) ~msg:"pow: base is one";
  assert_equal (-1) (pow (-1) 13) ~msg:"pow: base is -1, odd exponent";
  assert_equal 1 (pow (-1) 12) ~msg:"pow: base is -1, even exponent"

let test_fac _ =
  assert_equal 1 (fac 0);
  assert_equal 1 (fac 1)
  
let test_get_nth _ =
  assert_equal 42 (get_nth(0, [42])) ~msg:"get_nth: single element list";
  assert_equal "hello" (get_nth(0, ["hello"; "world"])) ~msg:"get_nth: string list";
  assert_equal false (get_nth(1, [true; false; true])) ~msg:"get_nth: bool list"


let test_larger _ =
  assert_equal [] (larger [1; 2; 3] [4; 5; 6]) ~msg:"larger: non-empty lists of equal length";
  assert_equal [] (larger ["a"] ["b"]) ~msg:"larger: single-element lists of equal length";
  assert_equal [] (larger [] []) ~msg:"larger: both lists are empty";
  assert_equal ["a"; "b"] (larger ["a"; "b"] ["c"]) ~msg:"larger: string lists"

let test_sum _ =
  assert_equal 10 (sum [1; 0; 4] [5; 0]) ~msg:"sum: lists containing zero";
  assert_equal (-15) (sum [-1; -2; -3] [-4; -5]) ~msg:"sum: all negative numbers";
  assert_equal (-2) (sum [1; 2] [-3; -2]) ~msg:"sum: negative result";
  assert_equal 0 (sum [10; -20] [5; 5]) ~msg:"sum: elements cancel to zero"



let suite =
  "student" >::: [
    "sanity" >:: test_sanity;
    "rev_tup" >:: test_rev_tup;
    "rev_triple" >:: test_rev_triple;
    "is_odd" >:: test_is_odd;
    "is_older" >:: test_is_older;
    "to_us_format" >:: test_to_us_format;
    "pow" >:: test_pow;
    "fac" >:: test_fac;
    "get_nth" >:: test_get_nth;
    "larger" >:: test_larger;
    "sum" >:: test_sum
  ]

let _ = run_test_tt_main suite
