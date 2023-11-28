circuit First (0 -> a, 0 -> b, 0 -> c, 0 -> d) {

	gate a,b -> Hfirst {
		a,b -> |H;
	};

	gate c,a,b -> Q {
		a,b -> Hfirst;
		c -> H -> Y;
	};

	c,a,b -> Q;

	if a {
		c -> X;
	};
}
