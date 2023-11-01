
class Main inherits Parent {
    x: String <- "abc";
    y: Int <- 5;
    z: Int <- y;
    pp: Int <- p;
    main(): Int { 0 };
};

class Parent {
    p: Int <- 10;
};

class Z {
    p: String <- "foo";
};

