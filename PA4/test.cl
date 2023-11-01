
class Main inherits Parent {
    x: String <- "abc";
    y: Int <- 5;
    z: Object <- y;
    zz: Int <- z;
    zzz: Main <- par;
    m: Main;
    obj: Object <- y;
    obj2: Object <- par;
    par: Parent <- m;
    pp: Int <- p;
    main(): Int { 0 };
};

class Parent {
    p: Int <- 10;
};

class Z {
    p: String <- "foo";
};

