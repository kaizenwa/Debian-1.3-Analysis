main()
{
 int *ptr;
 printf("Using printf...\n");
 ptr = sbrk(1525);
 *ptr = 5;
 printf("val: %d\n", *ptr);
 ptr = sbrk(-1525);
 printf("ptr: %p\n", ptr);
}
