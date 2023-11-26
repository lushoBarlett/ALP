circuit First (0 -> a, 0 -> b, 0 -> c, 0 -> d) {

	gate a,b -> Hfirst {
		b,a -> |H;
	};

	gate c,a,b -> Q {
		a,b -> Hfirst;
		c -> X;
	};

	c,a,b -> Q;
}
