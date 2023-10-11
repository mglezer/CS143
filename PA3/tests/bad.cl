
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no class-level error *)
class A {
    x: Int <- ; --Missing expression
    z: Int <- 2; --valid
    y <- 5; --Missing type
    l: Int <- LET 
        x: Int, y: Int 
            <- 4, z <- "abc" IN 42;
    b: Int <- {
        class;
        5 + 6;
        Random stuff
        7;
    }; --last: Int <-
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;

