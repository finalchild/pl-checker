(* Exercise 1. eval formula *)
open Ex1
open Testlib
open Printf

module TestEx1: TestEx =
  struct
    type testcase =
      | CALCULATE of exp * string * float

    let testcases =
      [ 
        CALCULATE(
          SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)),
          "SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))",
          375.0
        );
        CALCULATE(
          INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1)),
          "INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1))",
          319.065
        );
        CALCULATE(
          INTEGRAL(REAL 1.0, REAL 0.0, SUB(MUL(X, X), INT 0)),
          "INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1))",
          -0.285
        );
        (* Nested SIGMA *)
        CALCULATE(
          SIGMA(REAL 1.0, REAL 5.0, MUL(X, SIGMA(REAL 1.0, REAL 3.0, X)) ),
          "SIGMA(REAL 1.0, REAL 5.0, MUL(X, SIGMA(REAL 1.0, REAL 3.0, X)) )",
          90.0
        );
        (* Nested INTEGRAL & flipped INTEGRAL *)
        CALCULATE(
          INTEGRAL(REAL 2.05, REAL 1.0, MUL( MUL(X,X), INTEGRAL(REAL 0.0, REAL 1.05, X)) ),
          "INTEGRAL(REAL 2.05, REAL 1.0, MUL( MUL(X,X), INTEGRAL(REAL 0.0, REAL 1.05, X)) )",
          -0.98325
        );
        (* SOME ZERO SIGMA *)
        CALCULATE(
          SIGMA(REAL 100.0, REAL 5.0, MUL(X, SIGMA(REAL 1.0, REAL 3.0, X)) ),
          "SIGMA(REAL 1.0, REAL 5.0, MUL(X, SIGMA(REAL 1.0, REAL 3.0, X)) )",
          0.0
        );
        CALCULATE(
          SIGMA(REAL 1.0, REAL 5.0, MUL(X, SIGMA(REAL 4.0, REAL 3.0, X)) ),
          "SIGMA(REAL 1.0, REAL 5.0, MUL(X, SIGMA(REAL 1.0, REAL 3.0, X)) )",
          0.0
        );
        CALCULATE(
          SIGMA(REAL 5.0, REAL 1.0, MUL(X, X)),
          "SIGMA(REAL 1.0, REAL 5.0, MUL(X, SIGMA(REAL 1.0, REAL 3.0, X)) )",
          0.0
        );
        (* SOME SIGMA *)
        CALCULATE(
          SIGMA(REAL 1.0, REAL 3.0, MUL(X, MUL(X,X))),
          "SIGMA(REAL 1.0, REAL 3.0, MUL(X, MUL(X,X))",
          36.0
        );
        CALCULATE(
          SIGMA(REAL 1.0, SIGMA(REAL 1.0, REAL 3.0, X) , SUB(X, MUL(X,X))),
          "SIGMA(REAL 1.0, SIGMA(REAL 1.0, REAL 3.0, X) , SUB(X, MUL(X,X)))",
          -70.0
        );
        CALCULATE(
          SIGMA(SIGMA(REAL 3.0, REAL 100.0, SUB(X,X)), SIGMA(REAL 1.0, REAL 3.0, X) , SUB(X, MUL(X,X))),
          "SIGMA(SIGMA(REAL 3.0, REAL 100.0, SUB(X,X)), SIGMA(REAL 1.0, REAL 3.0, X) , SUB(X, MUL(X,X)))",
          -70.0
        );
        
      ]

    let runner tc =
      match tc with
      | CALCULATE (f, fs, ans) -> abs_float ((calculate f) -. ans) < 0.00000001

    let string_of_tc tc =
      match tc with
      | CALCULATE (f, fs, ans) -> 
          ( fs  
          , Printf.sprintf "%.10f" ans
          , Printf.sprintf "%.10f" (calculate f)
          )
  end

open TestEx1
let _ = wrapper testcases runner string_of_tc
