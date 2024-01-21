// Bug: # line directive in template definition interferes with growing obstack
template <class T> class A
{
public:

# 200 "lineno3.C"
  int foo () { undef1(); }
};

template class A<int>;
