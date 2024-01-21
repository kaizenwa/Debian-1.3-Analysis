// potential bug: # line directive does not get reproduced in template
// expansion
template <class T> class A
{
public:
# 200 "lineno3.C"
  int foo () { undef1(); }
};

template class A<int>;
