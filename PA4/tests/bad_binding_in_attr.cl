
class A {};
class B inherits A {};
class Aa inherits A {};
class Main {
    xx: XX <- 5;
    yy: YY <- 5;
    bad_cond: XX <- if 1 < 0 then xx else yy fi;
    bad_cond2: B <- if 1 < 0 then xx else new A fi;
    a: A <- xx;
    a: A <- xx + 5;
    main(): Int {0};
};
