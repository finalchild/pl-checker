(* Exercise 6. IntListQ *)
open Ex6
open Testlib

open IntListQ

module ValidIntListQ = (IntListQ: Queue)

module TestEx6: TestEx =
  struct
    let exnum = 6

    type testcase =
      | SEQ of seq list
    and seq =
      | ENQ of int list
      | DEQ of int list

    let runner tc =
      let rec runner_ l q =
        match l with
        | [] -> true
        | (h::tc') ->
            match h with
            | ENQ l -> runner_ tc' (enQ (q, l))
            | DEQ l ->
                let (l', q') = deQ q in
                if l' = l then runner_ tc' q'
                else false
      in
      match tc with
      | SEQ l -> runner_ l emptyQ

    let string_of_tc tc =
      let rec string_of_seqs seqs q =
        match seqs with
        | [] -> ("", "", "")
        | (h::seqs') ->
            let string_of_int_list = string_of_list string_of_int in
            match h with
            | ENQ l ->
                let (s, ans, out) = string_of_seqs seqs' (enQ (q, l)) in
                ("\n  enQ (q, " ^ (string_of_int_list l) ^ ")" ^ s, ans, out)
            | DEQ l ->
                let (l', q') = deQ q in
                if l' = l then
                  let (s, ans, out) = string_of_seqs seqs' q' in
                  ("\n  " ^ correct_symbol ^ " deQ (q) = " ^ (string_of_int_list l) ^ s, ans, out)
                else ("\n  " ^ wrong_symbol ^ " deQ (q)", string_of_int_list l, string_of_int_list l')

      in
      match tc with
      | SEQ seqs -> string_of_seqs seqs emptyQ

    let testcases =
      [ SEQ
        [ ENQ [1;2;3]
        ; DEQ [1;2;3]
        ]
      ; SEQ
        [ ENQ []
        ; ENQ [1;2;3]
        ; ENQ [4;5;6]
        ; ENQ [1;2;3]
        ; DEQ []
        ; DEQ [1;2;3]
        ; ENQ [1]
        ; ENQ [-10;1;-9]
        ; DEQ [4;5;6]
        ; DEQ [1;2;3]
        ; DEQ [1]
        ; DEQ [-10;1;-9]
        ; ENQ [222;333]
        ; DEQ [222;333]
        ]
      ]
  end

open TestEx6
let _ = wrapper exnum testcases runner string_of_tc
