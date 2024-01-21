class foo {
public:
  operator ++ ();
  operator ++ (int);
  operator ++ (char);		// illegal
  operator ++ (short);		// illegal
  operator ++ (long);		// illegal
};
