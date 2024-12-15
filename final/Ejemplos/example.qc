Rx(pi) | skip;

(Ry(π / 2); Rx(π)) | skip;

skip | (Ry(π / 2); Rx(π));

swap;

ctrl {
	Rx(π)
};

Ry(π / 2) | Ry(π / 2);
Rx(π) | Rx(π)