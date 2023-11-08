class C {
	a : Int;
	b : Bool;
    d : D <- new D;
    bool: Bool;
    str: String <- "hello world";
    len: Int <- str.length();
    conc: String <- str.concat(" you beautiful thing");
    substr: String <- str.substr(0, 5);
    ab: Object <- str.abort();
    cp: String <- str.copy();
    type_name: String <- a.type_name();
    eq: Bool <- 4 = 5;
    obj: Object <- d;
    cs: C <- case d of
        c: SELF_TYPE => c;
        d: D => d;
        e: E => e;
        o: Object => o;
    esac;
    other_c : D <- cs;
    letty: Int <- let neu: Int <- 5, d: Int <- 8 in neu + d;
    loopy: Object <- while false loop 5 pool;
    cond: Object <- if b then self else self fi;
    neu: SELF_TYPE <- new SELF_TYPE;
    c : C <- if 1 < 0 then new D else new E fi;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
        c <- d.init(3, true);
		self;
           }
	};
};

class D inherits C {
    get_copy(n: Int) : D { new D };
};

class E inherits D {};

Class Main {
	main():C {
	  new C
	  --(new C).init(1,true)
	};
};
