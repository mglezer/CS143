class C {
	a : Int;
	b : Bool;
    d : D <- new D;
    bool: Bool;
    eq: Bool <- 4 = 5;
    obj: Object <- d;
    cs: Object <- case d of
        c: C => c;
        d: D => d;
        e: E => e;
        o: Object => o;
    esac;
    letty: Int <- let neu: Int <- neu, d: Int <- 8 in neu + d;
    loopy: Object <- while false loop 5 pool;
    cond: Object <- if b then 5 else "abc" fi;
    neu: SELF_TYPE <- new SELF_TYPE;
    c : C <- if 1 < 0 then new D else new E fi;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

class D inherits C {};
class E inherits D {};

Class Main {
	main():C {
	  new C
	  --(new C).init(1,true)
	};
};
