// Build don't link:

class string {
public:
  string(const char*) { }	// ERROR - fn ref in err msg
  explicit string(int size) { }	// ERROR - fn ref in err msg
};				// ERROR - fn ref in err msg

void foo(string) { }

string bar() {
  foo("hello");		// ok
  foo(string(2));	// ok
  foo(2);		// ERROR - no implicit conversion from int to string
  string x = 2;		// ERROR - no implicit conversion from int to string
  string y(2);		// ok
  foo((string)2);	// ERROR - no conversion from int to string
  return 2;		// ERROR - no implicit conversion from int to string
}

class A : string {
public:
  A() : string(2) { }	// ok
};
