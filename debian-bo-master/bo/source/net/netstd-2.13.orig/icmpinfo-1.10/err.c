#include <stdio.h>

extern char *pname;

int err_quit(str)
	char *str;
{
  fprintf(stderr,"%s: %s\n",pname,str);
  exit(1);
}
int err_sys(str)
        char *str;
{
  perror(pname);
  fprintf(stderr,"\t%s\n",str);
  exit(2);
}
int err_ret(str)
	char *str;
{
  fprintf(stderr,"%s: %s\n",pname,str);
  return 0;
}
