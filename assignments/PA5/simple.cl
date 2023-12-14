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
    str1: String <- "abc";
    str2: String <- "abc";
    str3: String <- "abdefg";
    uninit: B;
    io: IO <- new IO;
    boo: Bool <- true;
    utterNoise(): String {
        "It's main!"
    };
    out_bool(b: Bool): Object {
        io.out_int(if b then 1 else 0 fi)
    };
    new_line(): Object {
        io.out_string("\n")
    };
    main(): Int {{
        io.out_string("~false? ");
        out_bool(not false);
        new_line();
        io.out_string("~true? ");
        out_bool(not true);
        new_line();
        io.out_string("str1 = str2? ");
        io.out_int(if str1 = str2 then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("str1 = str3? ");
        io.out_int(if str1 = str3 then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("b = a? ");
        io.out_int(if b = a then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("b = uninit? ");
        io.out_int(if b = uninit then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("b = b? ");
        io.out_int(if b = b then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("num = 21? ");
        io.out_int(if num = 21 then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("num < 21? ");
        io.out_int(if num < 21 then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("42 < 21? ");
        io.out_int(if 42 < 21 then 1 else 0 fi);
        io.out_string("\n");
        io.out_string("21 < 42? ");
        io.out_int(if 21 < 42 then 1 else 0 fi);
        io.out_string("\n");
        let tmp: Int in {
            tmp <- if isvoid(uninit) then 1 else 0 fi; --should be true
            io.out_string("Value of tmp: ");
            io.out_int(tmp);
            io.out_string("\n");
        };
        b.get_num();
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
