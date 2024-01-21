void
foo(char c)
{
}

main()
{
 char *c;
 
 c = (char*)0x1000000;
 foo(*c);
}

 