
class A {};
class B inherits A {};
class Aa inherits A {};
class Main {
    xx: XX <- 5;
    yy: YY <- 5;
    bad_cond: XX <- if 1 < 0 then xx else yy fi;  --ok
    bad_cond2: B <- if 1 < 0 then xx else new A fi; --not ok
    bad_cond3: A <- if 1 < 0 then xx else xx fi; --ok
    a: A <- xx;
    a: A <- xx + 5;
    main(): Int {0};
};
