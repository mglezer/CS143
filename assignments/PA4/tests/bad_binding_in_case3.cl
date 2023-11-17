class A {};
class B inherits A {};
class Aa inherits A {};
class Main {
    attr: A <- case 5 of
        xx: XX => xx;
        yy: YY => yy;
        esac;
    main(): Int {0};
};
