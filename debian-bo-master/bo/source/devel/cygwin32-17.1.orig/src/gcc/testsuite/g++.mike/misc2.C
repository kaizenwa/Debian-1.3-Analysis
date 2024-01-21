/* -ansi -pedantic-errors should catch this. */

class C {
 public:
  extern inline int A() {
	return 1;
  }
};
