struct A {
	int x;
};

int operator()(A x,float y) { // MUST be a member function
	return 1;
}

main() {
	A x;
	x(1.0);
}
