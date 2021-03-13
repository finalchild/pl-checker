(* Exercise 1. merge *)
open Ex1
open Testlib

module TestEx1: TestEx =
  struct
    type testcase =
        MERGE of int list * int list * int list

    let testcases: testcase list =
      [ MERGE ([], [], [])
      ; MERGE ([], [-1;-300], [-1;-300])
      ; MERGE ([-1;-300], [], [-1;-300])
      ; MERGE ([130;5;4;2], [3;1;-1], [130;5;4;3;2;1;-1])
      ; MERGE ([130;5;4;2], [3;1;-1;-3000], [130;5;4;3;2;1;-1;-3000])
      ]

    let runner (tc: testcase): bool =
      match tc with
      | MERGE (a, b, ans) -> merge(a,b) = ans

    let string_of_int_list: int list -> string = string_of_list string_of_int

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | MERGE (a, b, ans) ->
          ( Printf.sprintf "MERGE(%s, %s, %s)" (string_of_int_list a) (string_of_int_list b) (string_of_int_list ans)
          , string_of_int_list ans
          , string_of_int_list (merge(a,b))
          )
  end

open TestEx1
let _ = wrapper testcases runner string_of_tc
