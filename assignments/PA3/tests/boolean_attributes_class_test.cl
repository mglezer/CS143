class AttrOnly {
    args: Int <- f(5);
    let_ambi: Int <- let x: Int <- 5, y: Int <- 10 IN x + y * 3;
    let_ambi: Int <- (let x: Int <- 5, y: Int <- 10 IN x) + y * 3;
    oth: Int <- ~x.getNum();
    tri: Int <- 8 - 1 * ~ 6 * ~x . getNum () + 2 + 3 / 2;
    x: Int <- 5;
    y: String <- "Mickey";
    z: Boolean <- false;
    a: Int <- x;
    parens: Int <- (5);
    comp: Boolean <- not z;
    eq: Boolean <- z = comp;
    lte: Boolean <- 5 <= 6;
    lt: Boolean <- 4 < 3;
    neg: Int <- ~5;
    div: Int <- 10 / 3;
    mul: Int <- 3 * 98;
    plus: Int <- 8 + 7;
    minus: Int <- 9 - 4;
    isv: Boolean <- isvoid 4;
    neu: String <- new String;
    cs: String <-
            case x of
                int: Int => "Integer";
                str: String => "String";
                obj: Object => "Object";
            esac;
    lt: String <-
            let x: Integer <- 5,

                y: Integer <- 6, z: Integer <- 2 in

                    { 42; };
    wl: Int <- WHILE x < 100 LOOP x <- x + 1 POOL;
    iff: Int <- IF x < a THEN x ELSE a FI;
    call: Int <- f(x);
    static_dispatch: Int <- ~x@Object.method("hello");
    --prec: Int <- 4 + x . y + 5;

    add(a: Int, b: Int): Int {
        { 
            ClaSS; 
            x +;
            a + b;
        }
    };

    last: Int <-
        {{ 
            x +;
            y; 
            LET l1: Int <-5, l2: Int <- 
                    , 
                l4: String <- "testing", 
                    l3 <- 
                        "abc" IN 42;
            question -;
            a + b
        };}
    ;
};
