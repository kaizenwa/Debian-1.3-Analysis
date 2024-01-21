// ARM $5.7, it's illegal to do math on a `void*'.

main()
{
  void *p;
  ++p;
}
