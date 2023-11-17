class A {
    a: Int <- 5;
    aa: String <- "abc";
    meth(): Object { new Object };
};

class B inherits A {
    b: Bool <- true;
    meth(): Object { 5 };
    meth2(): Int { 4 };
};

class Main {
    main(): Int { 0 };
};
