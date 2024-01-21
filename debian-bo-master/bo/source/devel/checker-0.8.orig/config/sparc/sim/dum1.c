extern int flag_verbose;

main(int argc, char *argv[])
{
 char *str = "Hello\n";
 unsigned int sp;
 if (argc > 2)
   flag_verbose = 0;
 alarm(0);
 if (argc > 1 && argv[1][0] == 'r')
   run();
 asm ("mov %%sp, %0\n\t" : "=r" (sp));
 printf("Sp : 0x%08x\n", sp);
 write(1, str, strlen(str));
 printf("hello %d\n", 12);
}
