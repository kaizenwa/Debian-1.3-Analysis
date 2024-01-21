int val[] =
{ 4, 9, 36, 15, 12, 40, 27, 0};

main()
{
 int i;
 char *p;
 for (i = 0; val[i]; i++)
   {
     p = alloca(val[i]);
     printf ("alloca(%d) = %p\n", val[i], p);
     __chkr_disp_right (p, val[i] + 2);
     p[val[i] - 1] = 0;
   }
}

