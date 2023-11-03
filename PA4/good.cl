class C {
	a : Int;
	b : Bool;
    bool: Bool;
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
