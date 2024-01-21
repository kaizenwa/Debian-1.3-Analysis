blah()
{
  return '\n';
}

foo()
{
  char s[] = "abcedfg01234";
  char *sp = s + 12;

  switch (blah ())
    {
      case '\n':
        break;
    }

  while(*--sp == '0')
    ;
  sprintf(sp+1, "X");

  if (s[12] != 'X')
    abort();
}

main()
{
  foo();
  exit (0);
}
