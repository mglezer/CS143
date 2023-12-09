class A {
    a: Int <- 5 * 4;
    aa: String <- "abc";
    makeSound(): String { "Ahhhh" };
    meth(): Object { new Object };
};

class B inherits A {
    b: Bool <- true;
    get_num(): Int { 101 };
    meth(): Object { 5 };
    meth2(a: Int, b: Int): Int { a + b };
    makeSound(): String { "Bahhhhh" };
};

class Main {
    num: Int <- 7 * 3;
    b: B <- new B;
    a: A <- new A;
    io: IO <- new IO;
    boo: Bool <- true;
    utterNoise(): String {
        "It's main!"
    };
    main(): Int {{
        num <- case b of
            m: Main => 8;
            bb: B => bb.get_num();
            o: Object => 42;
        esac;
        io.out_int(num);
        io.out_string("\n");
        while boo loop {
            io.out_string("Inside loop!\n");
            boo <- false;
        } pool;
        let tmp1: Int <- 5, tmp2: String <- "Hi" in {
            io.out_int(tmp1 + 1);
            io.out_string("\n");
            io.out_string(tmp2.concat("\n"));
        };
        io.out_string(self@Main.utterNoise());
        io.out_string("\n");
        io.out_string(utterNoise());
        io.out_string("\n");
        io.out_int(b.meth2(b.meth2(2, 2), b.meth2(5, 400)));
        io.out_string("\n");
        io.out_string(b.makeSound());
        io.out_string("\n");
        io.out_string(b@A.makeSound());
        io.out_string("\n");
        io.out_string(a.makeSound());
        io.out_string("\n");
        4 * 5;
    }};
};
