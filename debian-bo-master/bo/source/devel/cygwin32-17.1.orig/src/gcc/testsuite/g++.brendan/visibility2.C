class X {
  void g (int);
public:
  void g (double);
};
	
class Y : public X { void f() { g (1); } }; // ERROR

