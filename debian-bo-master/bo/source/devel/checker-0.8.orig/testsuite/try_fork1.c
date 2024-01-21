unsigned long sigma(unsigned long n)
{
 if (n == 1 || n == 0)
   return 1;
 return n + sigma(n-1);
}

void
do_child()
{
 printf("sigma 2000 %u\n", sigma(2000));
 printf("EOC\n");
}

int
do_a_fault()
{
 int a;
 return a; 
}

main()
{
 int pid;
 int c,d;
 
 do_child();
 do_a_fault();
 pid = fork();
 
 if (pid != 0)
   {
     printf("Parent\n");
     do_child();
     do_a_fault();
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
     do_a_fault();
   }
}
