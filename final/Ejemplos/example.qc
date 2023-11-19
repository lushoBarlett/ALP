circuit First (0 -> a, 0 -> b, 0 -> c) {
	gate x,y,z -> P {
		x -> X;
		y -> Y;
		z -> Z;
	};
	a -> X -> Y -> Z;
}