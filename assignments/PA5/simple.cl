class A {
    a: Int <- 5 * 4;
    aa: String <- "abc";
    makeSound(): String { "Ahhhh" };
    meth(): Object { new Object };
};

class B inherits A {
    b: Bool <- true;
    meth(): Object { 5 };
    meth2(a: Int, b: Int): Int { a + b };
    makeSound(): String { "Bahhhhh" };
};

class Main {
    num: Int <- 7 * 3;
    b: B <- new B;
    a: A <- new A;
    utterNoise(): String {
        "It's main!"
    };
    main(): Int {{
        (new IO).out_string(self.utterNoise());
        (new IO).out_string("\n");
        (new IO).out_string(utterNoise());
        (new IO).out_string("\n");
        (new IO).out_int(b.meth2(b.meth2(2, 2), b.meth2(5, 400)));
        (new IO).out_string("\n");
        (new IO).out_string(b.makeSound());
        (new IO).out_string("\n");
        (new IO).out_string(a.makeSound());
        (new IO).out_string("\n");
        4 * 5;
    }};
};
