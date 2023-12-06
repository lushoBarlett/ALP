circuit First (0 -> a, 0 -> b) {

	gate x,y -> CNOT {
		if x {
			y -> X;
		};
	}

	a -> X -> H;
	b -> H;

	b,a -> CNOT;

	a,b -> H H;
}
