// Bug: incomplete instantiation messes with lineno
template <class T> class A;

main()
{
  A<int> *p;
  undef1();
}
