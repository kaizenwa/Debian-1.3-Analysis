class a {
public:
  int operator++(int) { return operator()++ ; }		// ERROR - 
};
