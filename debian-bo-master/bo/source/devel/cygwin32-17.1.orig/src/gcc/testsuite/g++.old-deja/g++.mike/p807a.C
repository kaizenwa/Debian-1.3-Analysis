// Build don't link:
// prms-id: 807

// See ARM page 275 Section 12.3.2

extern "C" void printf (char *, ...);
extern "C" void exit(int);

class B;

class A {
public:
	A(B&);
};

class B {
public:
	operator A();
};

B b;
A a = b;  // ERROR - should fail as it is ambigious.
