
class Main inherits Parent {
    x: String <- "abc";
    y: Int <- 5;
    z: Object <- y;
    zz: Int <- z;
    zzz: Main <- par;
    m: Main;
    me3: Main <- me;
    obj: Object <- y;
    obj2: Object <- par;
    par: Parent <- m;
    pp: Int <- p;
    main(): Int { 0 };
    selfie(): Main { new Main };
};

class Parent inherits Grandparent {
    num: String;
    me: SELF_TYPE <- new Parent;
    me2: Parent <- me;
    p: Int <- 10;
    meth(): Int { 1 };
    meth2(b: String): Object { 0 };
    attr: Int <- 5;
    attr: String <- "abc";
    obj: Object <- self.meth2("abc");
    selfie(): SELF_TYPE { new SELF_TYPE };
    copy(): SELF_TYPE { self };
};

class Grandparent {
    num: Int;
    meth(): Object { 0 };
    meth2(a: Int): Object { 0 };
};


class Z {
    p: String <- "foo";
};

