// Build don't link:

struct foo {};
foo& x() { return foo(); }	// WARNING - 
