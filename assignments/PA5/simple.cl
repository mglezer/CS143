class A {
    a: Int <- 5;
    aa: String <- "abc";
    meth(): Object { new Object };
};

class B inherits A {
    b: Bool <- true;
    meth(): Object { 5 };
    meth2(a: Int, b: Int): Int { a + b };
};

class Main {
    b: B <- new B;
    main(): Int { 2 + 3 };
};
