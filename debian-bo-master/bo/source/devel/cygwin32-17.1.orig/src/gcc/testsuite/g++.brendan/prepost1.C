class foo {
public:
  operator ++ ();
};

main()
{
  foo x;

  // This should fall back to calling operator++(), and be an error with
  // the -pedantic flag.
  x++;
}
