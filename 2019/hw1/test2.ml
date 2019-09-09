(* Exercise 2. iterator *)
open Ex2
open Testlib

module TestEx2: TestEx =
  struct
    type testcase =
      | ITERATOR of int * (int -> int) * string * (int -> int) * string * int * int
      | ITERATOR_S of int * (string -> string) * string * (string -> string) * string * string * string

    let testcases: testcase list =
      [ ITERATOR (3, (fun x -> 2 + x), "x -> 2 + x", (fun x -> 6 + x) , "x -> 6 + x", 0, 6)
      ; ITERATOR (4, (fun x -> 2 * x), "x -> 2 * x", (fun x -> 16 * x), "x -> 16 * x", 1, 16)
      ; ITERATOR (4, (fun x -> 10 * x), "x -> 10 * x", (fun x -> 10000 * x), "x -> 10000 * x", 2, 20000)
      ; ITERATOR (0, (fun x -> 10 * x), "x -> 10 * x", (fun x -> x), "x -> x", 0, 0) (* 0 일 때는 항등함수를 반환하라고 했음 *)
      ; ITERATOR_S (3, (fun x -> x ^ "o"), "x -> x ^ \"x\"", (fun x -> x ^ "ooo"), "x -> x ^ \"ooo\"", "I", "Iooo") (* integer 라고 안 했음 *)
      ; ITERATOR_S (6, (fun x -> "0" ^ x), "x -> \"0\" ^ x", (fun x -> "000000" ^ x), "x -> \"000000\" ^ x", "2", "0000002")
      ]

    let runner (tc: testcase): bool =
      match tc with
      | ITERATOR (a, f, fs, ans, anss, example, result) -> (iter(a,f) example) = (ans example)
      | ITERATOR_S (a, f, fs, ans, anss, example, result) -> (iter(a,f) example) = (ans example)

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | ITERATOR (a, f, fs, ans, anss, example, result) ->
          ( Printf.sprintf "iter(%d, %s) %d" a fs example
          , string_of_int result
          , string_of_int (iter(a,f) example)
          )
      | ITERATOR_S (a, f, fs, ans, anss, example, result) ->
          ( Printf.sprintf "iter(%d, %s) %s" a fs example
          , result
          , (iter(a,f) example)
          )
  end

open TestEx2
let _ = wrapper testcases runner string_of_tc

