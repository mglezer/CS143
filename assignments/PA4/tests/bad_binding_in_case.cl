class A {};
class B inherits A {};
class Aa inherits A {};
class Main {
    attr: B <- case 5 of
        a: A => a;
        b: B => b;
        aa: Aa => aa;
        xx: XX => xx;
        esac;
    main(): Int {0};
};
