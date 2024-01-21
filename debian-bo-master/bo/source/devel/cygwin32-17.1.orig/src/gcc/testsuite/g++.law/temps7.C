
template <class B >
class A {
public:
  class C {};
};

template class A<int>::C;

