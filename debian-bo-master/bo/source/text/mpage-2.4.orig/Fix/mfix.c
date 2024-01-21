#include <stdio.h>

FILE *in=stdin,*out=stdout;
char line[200]; line2[200];

int
strcp(st1,st2)
char *st1,*st2;
{ 
  int n;
  n=strncmp(st1,st2,strlen(st2));
  return(n);
}

main()
{
  int i;
  for(i=0; i<10; i=1)
  {
    if (fgets(line,200,in)==NULL) { close(in); close(out); exit(1); }
    if (strcp(line,"%%BeginSetup")==0) 
    { 
      fgets(line,200,in);
      fgets(line,200,in);
      continue;
    }
    if (strcp(line,"%%PageBoundingBox: (at")==0) 
    {
      fprintf(out,"BeginDviLaserDoc\n"); 
      fprintf(out,"300 300 RES\n");
      fprintf(out,"%s",line);
      continue;
    }
    if (strcp(line,"%%PageBoundingBox:")==0) 
    { 
      fprintf(out,"%s",line);
      fprintf(out,"EndDviLaserDoc\n");
      continue;
    }
      
    fprintf(out,"%s",line);
  }
}
    