prepare (0 -> a, 0 -> b) {

	circuit x,y -> CNOT {
		ctrl x {
			y -> X;
		};
	};

	a -> X -> H;
	b -> H;

	b,a -> CNOT;

	a,b -> H H;
}
