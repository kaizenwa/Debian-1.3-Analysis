unsigned long sigma(unsigned long n)
{
 if (n == 1 || n == 0)
   return 1;
 return n + sigma(n-1);
}

do_child()
{
 printf("sigma 2000 %u\n", sigma(2000));
 printf("EOC\n");
}

main()
{
 int pid;
 int c,d;
 
 do_child();
 pid = fork();
 
 if (pid != 0)
   {
     printf("Parent\n");
     do_child();
     wait(0);
     c = d;
   }
 else
   {
     int a,b;
     a = 10+b;
     b = 500;
     printf("Child: %d\n", getpid());
     sleep(14);
     do_child();
   }
}
