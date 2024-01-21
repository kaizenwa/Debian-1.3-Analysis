main()
{
 char *m;
 void (*func)(int);
 
 m = malloc (10);
 func = m;
 (*func)(4);
}
