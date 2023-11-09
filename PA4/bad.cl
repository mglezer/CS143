class A {
    inherited_attr: Bool <- false;
};
class B inherits A {
    overridden_attr: Int <- 5;
    inherited_method(o: Object): Int { 7 };
    inherited_method2(o: Object): Int { 5 };
};

class BB inherits B {};

class C inherits B {
	a : Int <- "abc";
    a: String <- "xyz";
    overridden_attr: Int <- 6;
	b : Bool;
    wrong_type: String <- new Object;
    dup(): Object { new Object };
    dup(i: Int): Object { new Object };
    inherited_method(a: Object): Int { 8 };
    inherited_method2(a: String): Int { 8 };
    inherited_method2(a: String, b: String): Int { 8 };
    inherited_method2(a: Object): String { 8 };
    wrong_return(): C { new B };
    bad_static(o: Object, b: B, c: C): Object {{
        o@B.inherited_method(5);
        b@B.xyz(2);
        c@B.inherited_method(4);
    }};
    bad_dispatch(o: Object, b: B): Object {{
        o.inherited_method(5);
        b.xyz(2);
        c.inherited_method(4);
        c.inherited_method("xyz");
        c.inherited_method(4, 5);
    }};
    cs: B <- case a of 
        bb: BB => bb;
        c: C => c;
    esac;
    cs_dup: B <- case a of 
        bb: BB => bb;
        c: C => c;
        c: C => c;
        slf: SELF_TYPE => slf;
    esac;
    eq1: Bool <- new Int = new Bool;
    eq2: Bool <- 5 = 6;
    eq3: Bool <- new BB = new C;
    eq4: Bool <- b = overridden_attr;
    bad_loop: Bool <- while 5 loop 4 pool;
    good_loop: Object <- while true loop 4 pool;
    bad_cond: C <- if inherited_attr then new B else new C fi;
    good_cond: B <- if true then new B else new C fi;
    compl: Bool <- not 5;
    lt: Bool <- 5 < "abc";
    lte: Bool <- 5 <= "abc";
    lt2: Bool <- 5 / "abc";
    lt2: Bool <- 5 * "abc";
    lt3: Bool <- 5 + "abc";
    bad_ref: String <- xyz.copy();
    asn(o: Object): Object {{
        o <- 5;
        o <- new Object;
    }};
    lt: B <- let o: Object, b: B <- new B in b;
    isv: Bool <- isvoid a;
    cmp: Int <- ~5;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};
