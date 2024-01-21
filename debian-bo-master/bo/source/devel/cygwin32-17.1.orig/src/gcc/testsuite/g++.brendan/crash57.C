class foo {
private:
  char buffer[1024];
public:
  foo();
};

main()
{
  static foo& a = *(new foo);
}
