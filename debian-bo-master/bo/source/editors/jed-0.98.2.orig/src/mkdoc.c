#include "config.h"
#include "jed-feat.h"

#include <stdio.h>
FILE *Fpin = stdin;
FILE *Fpout = stdout;

#define SKIP_TO(x) while (c = getc(Fpin), (c != EOF) && (c != (x)))
#define SKIP_WHITE while (c = getc(Fpin), (c == ' ') || (c == '\t'))

void do_sl_file()
{
   int c = '\n';
   int type, n;

   /* looking for this format:  ) function ;%% doc... */
   while (c != EOF)
     {
	while ((c != '\n') && (c != EOF)) c = getc(Fpin);
	n = 0;
	while (1)
	  {
	     SKIP_WHITE;
	     if (c != ';') break;
	     if (getc(Fpin) != '%') break;
	     if (getc(Fpin) != '%') break;
	     SKIP_WHITE;
	     n = 1;
	     if (c == EOF) break;
	     do
	       {
		  putc(c, Fpout);
	       }
	     SKIP_TO('\n');
	     putc(1, Fpout);
	  }
	if (n) putc('\n', Fpout);
     }
}

	     
	
	

void do_c_file()
{
   int c = '\n';
   int type; 
   
   while (c != EOF)
     {
	while ((c != '\n') && (c != EOF)) c = getc(Fpin);
	
	SKIP_WHITE;
	
	if (c != 'a') continue;	c = getc(Fpin);
	if (c != 'd') continue;	c = getc(Fpin);
	if (c != 'd') continue;	c = getc(Fpin);
	if (c != '_') continue;	c = getc(Fpin);	
	if (c == 'i')
	  {
	     c = getc(Fpin);
	     if (c != 'n') continue;	c = getc(Fpin);
	     if (c != 't') continue;	c = getc(Fpin);
	     if (c != 'r') continue;	c = getc(Fpin);
	     if (c != 'i') continue;	c = getc(Fpin);
	     if (c != 'n') continue;	c = getc(Fpin);
	     if (c != 's') continue;	c = getc(Fpin);
	     if (c != 'i') continue;	c = getc(Fpin);
	     if (c != 'c') continue;
	     type = 'f';
	     putc('F', Fpout);
	  }
	else if (c != 'v') continue;
	else
	  {
	     c = getc(Fpin);
	     if (c != 'a') continue;	c = getc(Fpin);
	     if (c != 'r') continue;	c = getc(Fpin);
	     if (c != 'i') continue;	c = getc(Fpin);
	     if (c != 'a') continue;	c = getc(Fpin);
	     if (c != 'b') continue;	c = getc(Fpin);
	     if (c != 'l') continue;	c = getc(Fpin);
	     if (c != 'e') continue;
	     type = 'v';
	     putc('V', Fpout);
	  }
   
	SKIP_WHITE;
	if (c != '(') continue;
	SKIP_WHITE;
	if (c != '"') continue;
	
	SKIP_TO('"')
	  {
	     if (c == '\\')
	       {
		  c = getc(Fpin);
		  if ((c != '"') && (c != '\\')) putc('\\', Fpout);
	       }
	     putc(c, Fpout);
	  }
   
	fflush(Fpout);
	if (type == 'f')  /* get function, parameter number and return type */
	  {
	     /* example:
	     add_intrinsic("coreleft", (long) show_memory,VOID_TYPE, 0); */
	     
	     SKIP_TO(')');  /* (long) */
	     SKIP_WHITE;
	     putc(':', Fpout);
	     putc(c, Fpout);
	     SKIP_TO(',')
	       {
		  putc(c, Fpout);
	       }
	     putc(':', Fpout);
	     SKIP_WHITE;
	     putc(c, Fpout);  /* type: V,S,I (void, string , integer ) */
	     SKIP_TO(',');
	     putc(':', Fpout);
	     SKIP_WHITE;
	     if (c != EOF) putc(c, Fpout);  /* number of parameters */
	  }   
	putc(1, Fpout);
	/* get doc string */
	SKIP_TO('\n');
	SKIP_WHITE;
	
	/* doc follows / * ;; syntax */
   
	if (c != '/')
	  {
	     ungetc(c, Fpin); c = '\n';
	     putc('\n', Fpout);
	     continue;
	  }
	c = getc(Fpin);
	if (c != '*') 
	  {
	     putc('\n', Fpout);
	     continue;
	  }
	
	while (1)
	  {
	     SKIP_WHITE;
	     if (c != ';') break;
	     c = getc(Fpin);
	     if (c != ';') break;
	     SKIP_TO('\n') putc(c, Fpout);
	     putc(1, Fpout);  /* literal ^A to separate lines */
	  }
	ungetc(c, Fpin);
	c = '\n';
	putc('\n', Fpout);
     }
}

int process_file(char *file)
{
   if ((Fpin = fopen(file,"r")) == NULL) return(0);
/*   do_sl_file(); */
   do_c_file(); 
   fclose(Fpin);
}

void do_filelist(char *file)
{
   char line[512], *chp, *linep;
   FILE *fp;
   
   if (NULL == (fp = fopen(file, "r")))
     {
	fprintf(stderr, "unable to open input file: %s\n", file);
	exit(1);
     }
   
   while(NULL != fgets(line, 511, fp))
     {
	if (*line == '#') continue;
	linep = line;
	while(*linep && (*linep <= ' ')) linep++;
	chp = linep;
	while (*chp > ' ') chp++;
	*chp = 0;
	if (! *linep) continue; 
	if (!process_file(linep))
	  {
	     fprintf(stderr, "Unable to open %s\n", linep);
	  }
     }
   fclose(fp);
}

void usage(void)
{
   fprintf(stderr, "mkdoc [-o docfile] [-i filelist] [file1 ...]\n");
   exit(1);
}

int main(int argc, char **argv)
{
   int i;
   char *arg;
   
   if (argc < 2) usage();
   
   for (i = 1; i < argc; )
     {
	arg = argv[i++];
	if ((i != argc) && !strcmp(arg, "-i"))
	  {
	     do_filelist(argv[i++]);
	  }
	else if ((i != argc) && !strcmp(arg, "-o"))
	  {
	     if ((Fpout != NULL) && (Fpout != stdout)) fclose(Fpout);
	     if ((Fpout = fopen(argv[i++], "w")) == NULL)
	       {
		  fprintf(stderr, "Unable to open output file %s\n", argv[i - 1]);
		  exit(1);
	       }
	  } 
	else process_file(arg);
     }
   exit(0);
   return(0);
}
