class A {
  char *p;
public:
  operator const char *() const { return p; }
};

int foo(const A &a)
{
  return (a != 0);
}

