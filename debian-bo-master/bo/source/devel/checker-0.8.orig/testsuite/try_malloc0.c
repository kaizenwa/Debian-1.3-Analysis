main()
{
 char *v=malloc(0);
 v=realloc(v,0);
 printf("v=0x%x\n", v);
 free(v);
}
