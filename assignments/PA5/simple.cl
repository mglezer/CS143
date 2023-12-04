class A {
    a: Int <- 5 * 4;
    aa: String <- "abc";
    meth(): Object { new Object };
};

class B inherits A {
    b: Bool <- true;
    meth(): Object { 5 };
    meth2(a: Int, b: Int): Int { a + b };
};

class Main {
    num: Int <- 7 * 3;
    b: B <- new B;
    main(): Int {{
        num;
        2 * 3;
    }};
};
