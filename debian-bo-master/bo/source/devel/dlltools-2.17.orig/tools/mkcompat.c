/* 
 * Can we really build a jump.* files to be compatable with a older
 * DLL library without knowing previously what values to allocate for
 * each global ?? (m.dsouza@mrc-apu.cam.ac.uk)	2-3-1993
 * Compile as `gcc -O6 -s -N -o mkcompat mkcompat.c'
 * If invoked as `libinfo' then jump.params can be determined.
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include "utils.h"
#include "mkcompat.h"

#ifdef linux
#include <getopt.h>
#else
#ifdef __svr4__
#include <stdlib.h>
#define bzero(x,y) memset(x,0,y)
#else
extern int optind;
extern char *optarg;
extern int getopt (int __argc, char **__argv, char *__optstring);
#endif

#endif

#ifndef GREP
#define GREP "/usr/bin/grep"
#endif

#define TRUE 1
#define FALSE (!TRUE)
#define MAX_LINE_LEN 1024
#define JUMP_TMP	"/tmp/jump.tmp"
#define JUMP_VARS_NEW	"jump.vars.new"
#define JUMP_VARS_EXP	"jump.vars.exp"
#define JUMP_FUNCS_NEW	"jump.funcs.new"
#define JUMP_FUNCS_EXP	"jump.funcs.exp"
#define die(x,y) fprintf(stderr,x,y),exit(1)
#define progname(foo) (!strcmp(name,foo))
#define HEX_AND_DEC(num) " 0x%x (%d)\n",(num),(num)

char find_failed, new_lib=FALSE;
int undef = 0, plt_count = 0, got_count = 0;
unsigned int getaddr (FILE *, char[], long int), first_plt = 0L, first_got = 0L,
	current_got_end = 0L, current_got_size = 0L, jump_table_size = 0L, start_addr= 0L;
void mkcompat_usage (char[]), jump_vars (FILE *, FILE *), jump_funcs (FILE *, FILE *),
	output_new(FILE *, char *);

char *
getpath (char *path, char *file)
{

  char *ret;
  char *f=file;

  if (!strncmp(f,"-l",2) && strlen(f)>2)
	{
		f=malloc(strlen(file)+8);
		sprintf(f,"lib%s.sa",file+2);
	}
  ret = malloc ((path ? strlen (path) : 0) + 1 + strlen (f) + 1 + 1);
  strcpy(ret,f);
  if (path)
    {
      if (*(path + strlen (path) - 1) != '/')
	sprintf (ret, "%s/%s", path, f);
      else
	sprintf (ret, "%s%s", path, f);
    }

  if (!access (ret, R_OK))
    return ret;
  else if (!access (f, R_OK))
    sprintf (ret, "%s", f);

    if (f!=file) free(f);
    return ret;

}

void mkcompat_usage (char name[])
{
     fprintf (stderr,"***[Error!! Wrong number/type of arguments.!!]***\n");
     if progname("libinfo") {
	fprintf (stderr, "Get DLL library parameters from stub\n");
	fprintf (stderr, "usage: %s [-L lib_path] -- [-l lib] stub_files ...\n", name);
	fprintf (stderr, "example: %s", name);
     } else {
	fprintf (stderr, "Build compatable jump.vars/funcs files from a stub library\n");
        fprintf (stderr, "usage: %s [-v] [-f] [-L library_path] -- [stub_file] [-l lib]\n", name);
	fprintf (stderr, "example: %s -v", name);
     }
     die (" %s\n","-L/usr/X386/lib -- -lXaw");
}

int main (int argc, char **argv)
{
     FILE *stub=NULL, *jumpvars=NULL, *jumpfuncs=NULL;
     char tmp[MAX_LINE_LEN], found_dependencies = FALSE,
	ver[15], *p, *name, *path=NULL, *fullpath=NULL;
     char *jump_read=xmalloc(MAX_LINE_LEN);
     char *jump_write=xmalloc(MAX_LINE_LEN);
     char *filedir=NULL, extract_only=FALSE;
     unsigned int x;
     void (*call)()=NULL;
     int c;
     int i=0;


     optind = 1;      /* My SunOS manual says I shouldn't have to do this ?? */

     name = (name = strrchr (argv[0], '/')) ? ++name : argv[0];
     fprintf(stderr,"%s v%s\n",name,version_string);
     if progname("getvars") {
	argc=3; argv[1]="-x";argv[2]="-v"; /* Nasty but nice hack !! */
     }
     if progname("getfuncs") {
	argc=3;	argv[1]="-x";argv[2]="-f"; /* Nasty but nice hack !! */
     }
     while ((c = getopt (argc, argv, "xvfL:")) != EOF)
	switch (c) {
		case 'v': call=jump_vars;  ++i; break;
		case 'f': call=jump_funcs; ++i; break;
		case 'x': extract_only=TRUE;	break;
		case 'L': path=optarg;		break;
		default:  die ("Unknown option `%c' encountered\n", c);
	}
	if progname("libinfo") {
		if (argc < 2) mkcompat_usage (name);
		if (i) fprintf (stderr, "%s: `-%c' option unsupported\n",name,
			call==jump_funcs?'f':'v');
	} else {
	if (!extract_only && argc < 3) mkcompat_usage (name);
	if (!i)	die ("You must specify one of either `-v' or `-f'\n%c", 0);
	if (i>1)	die ("Only one option may be specified\n%c", 0);
	filedir = getenv("JUMP_DIR");
	if (filedir)
		fprintf (stderr, "Reading jump.log file from %s\n",filedir);
	else
		fprintf (stderr, "Warning: JUMP_DIR environment variable not set - checking cwd\n");

	sprintf(jump_read,"%s%sjump.log",filedir?filedir:"",filedir?"/":"");

	if (access(jump_read, R_OK))
	  {
	    fprintf(stderr,"Warning - cannot open `%s' for reading\n", jump_read);
	    if (call==jump_funcs)
	      sprintf(jump_write,"%s%sjump.funcs",filedir?filedir:"",filedir?"/":"");
	    else
	      sprintf(jump_write,"%s%sjump.vars",filedir?filedir:"",filedir?"/":"");
	    xsystem("rm -f %s;touch %s", jump_write, jump_write);
	  } else {
	    if (call==jump_funcs) {
	      sprintf(jump_write,"%s%sjump.funcs",filedir?filedir:"",filedir?"/":"");
	      xsystem (GREP " ' T ' %s>%s;true",jump_read,jump_write);
	      if (!extract_only && !(jumpfuncs = fopen (jump_write, "r")))
		die ("Can't open `%s' file for reading\n", jump_write);
	    } else {
	      sprintf(jump_write,"%s%sjump.vars",filedir?filedir:"",filedir?"/":"");
	      xsystem (GREP " -v ' T ' %s>%s;true",jump_read,jump_write);
	      if (!extract_only && !(jumpvars = fopen (jump_write, "r")))
		die ("Can't open `%s' file for reading\n", jump_write);
	    }
	  }
      }
     if (extract_only) return 0;

     if (jump_read) xfree(jump_read);
     for (; optind < argc; optind++) {

      new_lib=FALSE;
      undef = plt_count = got_count = 0;
      first_plt = first_got = current_got_end = current_got_size =
          jump_table_size =  start_addr= 0L;
      found_dependencies = FALSE;
      x=0;

     bzero(&tmp,MAX_LINE_LEN);
     bzero(&ver,15);

     *jump_write=(char)NULL;

     if (fullpath) free(fullpath);
     
     if (access(fullpath=getpath(path,argv[optind]), R_OK))
	  die ("Cannot open stub `%s' for reading\n", fullpath);
     else
	  fprintf(stderr,"Reading %s:\n",fullpath);

     xsystem (PREFIX "nm " NO_CPLUS " %s|sort>%s", fullpath,JUMP_TMP);


     if (!progname("libinfo") && optind < argc) {
	for (++optind; optind < argc; optind++) {

		if (fullpath) free(fullpath);
		fullpath=getpath(fullpath,argv[optind]);

		if (access(fullpath=getpath(path,argv[optind]), R_OK))
			die ("Cannot open stub `%s' for reading\n", fullpath);
		else
			fprintf(stderr,"Reading %s:\n",fullpath);

		xsystem (PREFIX "nm " NO_CPLUS " %s|sort>>%s", fullpath,JUMP_TMP);

		}
	xsystem ("sort %s | uniq > %s.new", JUMP_TMP,JUMP_TMP);
	xsystem ("mv %s.new %s", JUMP_TMP,JUMP_TMP);
     }

     if (!(stub = fopen (JUMP_TMP, "r")))
	  die ("Can't open `%s' file of library symbols for reading\n", JUMP_TMP);
     while (!feof (stub)) {
	    fgets (tmp, MAX_LINE_LEN - 1, stub);
	    if (!found_dependencies) {
		   if (strstr (tmp, " U __NEEDS_")&&progname("libinfo")) {
			if (*jump_write && strcmp(tmp,jump_write)==0) continue;
			strcpy(jump_write,tmp);
			 for (p = tmp + 26; *p=='_'?*p='.':*p; p++);
			 *(p=strchr (tmp + 26, '.'))=(char)NULL;
			 fprintf (stderr,"\tLibrary requires %s.so.%s", tmp + 26,++p);
		   } else if (strstr (tmp, " D ___lib")) {
			if ((p=strchr(tmp+14,'_'))!=NULL) {
				i=atoi(p+1);
				p=strrchr(p+1,'_');
				sprintf (ver, "%d.%cpl%d",i,
					 atoi(p+1)>99 ? *(p+1) : '0',
					 atoi(p+(atoi(p+1)>99 ? 2 : 1)));
			} else  sprintf (ver, "unknown");
			 found_dependencies = TRUE;
		   }
	    } else if (!first_got && strlen(tmp) >10 && (*(tmp+9)=='A' || *(tmp+9)=='a')) {
		   if (*(tmp+9)=='a') {
			if (strstr (tmp, " a __GOT_SIZE")) {
			 new_lib=TRUE;
			 current_got_size=strtoul (tmp, (char **) NULL, 16);
			} else if (strstr (tmp, " a __PLT_SIZE")) {
			 jump_table_size=strtoul (tmp, (char **) NULL, 16);
			}
			continue;
		   }
		   if (!strstr(tmp," A __PLT_")) ++plt_count;
		   if (!first_plt)
			first_plt = first_plt ? first_plt : ftell (stub) - strlen (tmp);
	    }
	    if (strstr (tmp, " A __GOT_")) {
		   if (!first_got)
			first_got = first_got ? first_got : ftell (stub) - strlen (tmp);
	    } else if (first_got) {
 		   if (strlen(tmp) >10 && *(tmp+9)=='A') ++got_count;
 		   if (!current_got_end)
			current_got_end = ftell (stub) - strlen (tmp);
		   if (strstr (tmp, " a __LAST_DATA_ADDRESS")) break;
	    }
     }
	if (!first_plt) {
	 fprintf (stderr,"Is `%s' really the stub of a DLL library ??\n", fullpath);
	 continue;
	}
	  if (!new_lib) {
	    fseek (stub, current_got_end, SEEK_SET);
	    fgets (tmp, MAX_LINE_LEN - 1, stub);
	    current_got_size = strtoul (tmp, (char **) NULL, 16);
#ifdef DEBUG
	    fprintf (stderr, "\tLast GOT %s (%x)\n", tmp, current_got_size);
#endif
	    fseek (stub, first_got, SEEK_SET);
	    fgets (tmp, MAX_LINE_LEN - 1, stub);
	    jump_table_size = strtoul (tmp, (char **) NULL, 16);
#ifdef DEBUG
	    fprintf (stderr, "\tFirst GOT %s (%x)\n", tmp, jump_table_size);
#endif
	  }
	    fseek (stub, first_plt, SEEK_SET);
	    fgets (tmp, MAX_LINE_LEN - 1, stub);
	    x = strtoul (tmp, (char **) NULL, 16);
	    start_addr=x-8L;
#ifdef DEBUG
	    fprintf (stderr, "\tFirst PLT %s (%x)\n", tmp, x);
#endif
	if progname("libinfo") {
	    fprintf (stderr,"\tLibrary version is %s\n\tLibrary resides in ", ver);
	    for (i=sizeof(tb)/sizeof(struct tab)-1;i>=0;i--)
		if (x-8L>=tb[i].start_addr) {
		if (*tb[i].name)
			fprintf (stderr, "registered area occupied by\n\t\t %s\n",tb[i].name);
		else	fprintf (stderr, "un-registered area\n");
        	break;
		}
	    fprintf (stderr,"\tStart address of library is 0x%x\n",start_addr);
	if (!new_lib) {
		fprintf (stderr, "WARNING: Old style library. Following values may be incorrect\n");
	    if (current_got_size)
		 fprintf (stderr,"\tGlobal Offset Table size is"
			  HEX_AND_DEC(current_got_size - jump_table_size + 8));
	    else
		 fprintf (stderr,"\tUnable to determine GOT size\n");
		 fprintf (stderr,"\tJump Table size is"
			  HEX_AND_DEC(jump_table_size -x));
	} else {
	    fprintf (stderr,"\tGlobal Offset Table size is"
		     HEX_AND_DEC(current_got_size));
	    fprintf (stderr,"\tJump Table size is"
		     HEX_AND_DEC(jump_table_size));
	}
	    fprintf (stderr,"\tEncountered %d function(s) ", --plt_count);
	    if (got_count)
		fprintf (stderr,"and %d global var(s) in stub\n", got_count);
	    else putc('\n',stderr);
	} else {
		call(call==jump_funcs?jumpfuncs:jumpvars,stub);
		fclose (call==jump_funcs?jumpfuncs:jumpvars);
	}
	fclose (stub); unlink (JUMP_TMP);
	if (fullpath) free(fullpath);
	if (!progname("libinfo")) exit(1);
     }
	if (jump_write) xfree(jump_write);
     exit (0);
}

void jump_vars (FILE * jumpvars, FILE * stub)
{
     FILE *new_vars,
      *expect_vars;
     char libname[20],
       line_1[MAX_LINE_LEN],
       line_2[MAX_LINE_LEN];
     unsigned int size=0,
       x=0,i=0,j=0;
     int count = 0;
     unsigned long start_fp;

     if (!(new_vars = fopen (JUMP_VARS_NEW, "w")))
	  die ("Can't open `%s' file for writing\n", JUMP_VARS_NEW);
     if (!(expect_vars = fopen (JUMP_VARS_EXP, "w")))
	  die ("Can't open `%s' file for writing\n", JUMP_VARS_EXP);
/*
 *	Get the libname for undefined symbols
 */

     fgets (line_1, MAX_LINE_LEN - 1, jumpvars);
     sscanf(line_1,"%*s%*s%*s%s",libname);

/*
 *	Re-arrange all existing symbols and place __DUMMY__
 *	definitions for lost globals
 */

#ifdef DEBUG
	printf("Debug: Table start = 0x%x\n", start_addr);
	printf("Debug: PLT size    = 0x%x\n",jump_table_size);
	printf("Debug: GOT start = 0x%x\n",start_addr+jump_table_size);
#endif

      if (new_lib) {
 	rewind(stub);
 	while (fgets (line_1, MAX_LINE_LEN - 1, stub) &&
	       strtoul(line_1, (char **)NULL, 16) < start_addr+jump_table_size);
#if DEBUG
	printf("Debug: First GOT = %s",line_1);
#endif

	fseek (stub, ftell(stub)-strlen(line_1), SEEK_SET);
     } else
	fseek (stub, first_got, SEEK_SET);

     start_fp=ftell(stub);
     while (fgets (line_1, MAX_LINE_LEN - 1, stub) && !feof (stub))
       {
	if (!strstr(line_1,"A __GOT_")) continue;
	if (strstr(line_1,"_LAST_DATA_ADDRESS")) break;
	if ((strlen(line_1)<17)) continue;
	    size=strtoul(line_1, (char **)NULL, 16);
	    strcpy(line_2,line_1);
	    sprintf(line_1+11,"%s",strstr(line_2,"__GOT_")+strlen("__GOT_"));
	    x = getaddr (jumpvars, line_1, 0);
	    if (find_failed) {
		 fprintf (stderr, "Lost symbol %s", line_1+12);
		 strcpy(line_2,line_1);
		 x=getaddr(stub,line_1,start_fp);
		 fprintf (expect_vars, "%.08x D %-20s %s\n",x,"__DUMMY__",libname);
	    }
	    else {
		strcpy(line_2,line_1);
		x=getaddr(stub,line_1,start_fp);
		fprintf (expect_vars, "%08x %s", x, line_2 + 9);
	    }
	    if (i) {
		j=size;
		   for (count=j-i-4;count>0;count-=4)
			fprintf(expect_vars,"%.08x D %-20s %s\n", 0, "__DUMMY__", libname);
		i=j;
	    } else i=size;
       }
     if (undef)
	fprintf (stderr, "Used %d placeholder(s) for lost global data(s)\n", undef);

/*
 *	Reopen the file of symbols we expect to see, count any new symbols
 *	and move them to the file jump.new
 */

     undef = count = 0;
     rewind (jumpvars);
     expect_vars = freopen (JUMP_VARS_EXP, "r", expect_vars);
     while (fgets (line_1, MAX_LINE_LEN - 1, jumpvars) && !feof (jumpvars)) {
	    if (!strstr (line_1, " __DUMMY__ ")) {
		   getaddr (expect_vars, line_1,0);
		   if (find_failed)
			fprintf (new_vars, "%s", line_1);
	    }
     }
     if (undef)
	  fprintf (stderr, "Found %d new global(s)\n", undef);

/*
 *	Reopen the file of symbols we expect to see and do the offeset
 *	calculations and print to stdout
 */
     count = 0;
     rewind (expect_vars);
     while (fgets (line_1, MAX_LINE_LEN - 1, expect_vars) && !feof (expect_vars))
       {
	    if (count++) {
		   x = strtoul (line_1, (char **) NULL, 16);
		   if (!x) {
			/* These are placeholders (seen in libc) */
			printf("%s",line_1);
			continue;
		   }
		   printf ("%08x %s", x - size, line_2 + 9);
	    }
	    size = strtoul (line_1, (char **) NULL, 16);
	    strcpy (line_2, line_1);
       }
     if (new_lib) {
		   /* Get the line with the __LAST_DATA_ADDRESS symbol */
		   strcpy(line_1,"blah blah __LAST_DATA_ADDRESS");
		   x = getaddr(stub,line_1,start_fp);
		   printf ("%08x %s", x - size, line_2 + 9);
     } else {
	    fprintf (stderr, "WARNING: Old style library. Unable to determine size of last symbol.\n");
	    printf ("# The following symbol was found in the old stub but its size\n");
	    printf ("# could not be determined as it is the last symbol in the table.\n");
	    printf ("# Please use getsize to determine its size.\n");
	    printf ("%08x %s", 0, line_2 + 9);
     }
     if (undef)	output_new(new_vars, JUMP_VARS_NEW);

     fclose (new_vars);    unlink (JUMP_VARS_NEW);
     fclose (expect_vars); unlink (JUMP_VARS_EXP);
}

void jump_funcs (FILE * jumpfuncs, FILE * stub)
{
     FILE *new_funcs,
      *expect_funcs;
     char libname[20],
       line_1[MAX_LINE_LEN],
       line_2[MAX_LINE_LEN];
     unsigned int size=0,
       x, i;
     int count = 0;
     unsigned long start_fp;

     if (!(new_funcs = fopen (JUMP_FUNCS_NEW, "w")))
	  die ("Can't open `%s' file for writing\n", JUMP_FUNCS_NEW);
     if (!(expect_funcs = fopen (JUMP_FUNCS_EXP, "w")))
	  die ("Can't open `%s' file for writing\n", JUMP_FUNCS_EXP);
/*
 *	Get the libname for undefined symbols
 */

     fgets (line_1, MAX_LINE_LEN - 1, jumpfuncs);
     sscanf(line_1,"%*s%*s%*s%s",libname);

/*
 *	Re-arrange all existing symbols and provide __DUMMY__
 *	definitions for lost globals.
 */
     fseek (stub, first_plt, SEEK_SET);

     start_fp=ftell(stub);
     while (ftell(stub) < first_got && fgets (line_1, MAX_LINE_LEN - 1, stub) && !feof (stub))
       {
	if (!strstr(line_1,"A __PLT_") ||
	    strstr(line_1,"___CTOR_LIST__")) continue;
	if ((strlen(line_1)<17)) continue;
	    strcpy(line_2,line_1);
	    sprintf(line_1+11,"%s",strstr(line_2,"__PLT_")+strlen("__PLT_"));
	    x = getaddr (jumpfuncs, line_1, 0);
	    if (find_failed) {
		 count++;
		 fprintf (expect_funcs, "%.8s U %-20s %s\n", line_1, "__DUMMY__", libname);
		 fprintf (stderr, "Lost function %s", line_1+12);
	    }
	    else {
		strcpy(line_2,line_1);
		x=getaddr(stub,line_1,start_fp);
/*		if (!x) {
 *			fprintf(stderr,"No library with function %s found\n",line_2+12);
 *			fprintf(stderr,"You must include ALL libraries required to build a compatable DLL\n");
 *			fprintf(stderr,"Clearing up and exiting....\n");
 *			goto funcs_exit;
 *
 *		} else
 */		fprintf (expect_funcs, "%08x %s", x, line_2 + 9);
	    }
       }
     if (count)
	  fprintf (stderr, "Used %d placeholder(s) for lost function(s)\n", count);

/*
 *	Reopen the file of symbols we expect to see, count any new symbols
 *	and move them to the file jump.new
 */

     undef = count = 0;
     rewind (jumpfuncs);
     expect_funcs = freopen (JUMP_FUNCS_EXP, "r", expect_funcs);
     while (fgets (line_1, MAX_LINE_LEN - 1, jumpfuncs) && !feof (jumpfuncs))
       {
	    if (!strstr (line_1, " U ")) {
		   getaddr (expect_funcs, line_1,0);
		   if (find_failed) fprintf (new_funcs,"%s", line_1);
	    }
       }
     if (undef)	fprintf (stderr, "Found %d new function(s)\n", undef);

/*
 *	Add more __DUMMY__'s for lost funcs seen in the stub.
 */
     rewind(expect_funcs);
     count = 0;
     while (fgets (line_1, MAX_LINE_LEN - 1, expect_funcs) && !feof (expect_funcs))
       {
	    if (count++)
	      {
		   x = strtoul (line_1, (char **) NULL, 16);

		   if (!x) {
			/* libc has these because of libcompat */
#if 0
			printf("%s",line_1);
#endif
			continue;
		   }
		   printf ("%08x %s", 0, line_2 + 9);
		   for (i=x-size-8L;i>0;i-=8L)
			printf("%08x U %-20s %s\n", 0, "__DUMMY__", libname);
	      }
	    size = strtoul (line_1, (char **) NULL, 16);
	    strcpy (line_2, line_1);
       }
     if (*line_2)	printf ("%08x %s", 0, line_2 + 9);
     if (undef)		output_new (new_funcs, JUMP_FUNCS_NEW);

     fclose (new_funcs);      unlink (JUMP_FUNCS_NEW);
     fclose (expect_funcs);   unlink (JUMP_FUNCS_EXP);
}

void output_new (FILE * f, char *name)
{
  char c;

  printf ("# Anything below this line is new to the old library\n");
  printf ("# Note: It is possible that some/all of these could be moved to jump.ignore\n");

/*
 *  This has to succeed if it suceeded before so I won't test it :-)
 */

  f = freopen (name, "r", f);
  while ((c = getc (f)) != EOF) putc (c, stdout);
}

unsigned int getaddr (FILE * f, char *p, long int pos)
{
  char str1[MAX_LINE_LEN],
	str2[MAX_LINE_LEN],
	line[MAX_LINE_LEN];
  unsigned int addr;
  long int saved_pos;

  bzero(&str1,MAX_LINE_LEN);
  bzero(&str2,MAX_LINE_LEN);
  bzero(&line,MAX_LINE_LEN);

  saved_pos=ftell(f);
  fseek(f,pos,SEEK_SET);
#if 0
  rewind (f);
#endif
  find_failed = FALSE;
  sscanf (p, "%*s%*s%s", str1);		/* We are looking for the 3rd field */
  while (fgets (line, MAX_LINE_LEN - 1, f) && !feof (f)) {
    if (strlen(line)<14) break;
    sscanf (line, "%*s%*s%s", str2);	/* Compare with the 3rd field */
    if (!strcmp (str1, str2)) {
      fseek (f, ftell (f) - strlen (line), SEEK_SET);
      addr = strtoul (line, (char **) NULL, 16);
      strcpy (p, line);
      fseek(f,saved_pos,SEEK_SET);
      return (addr);
    }
  }

/*
 * Should never get here if symbol exists
 */

  find_failed = TRUE;
  undef++;
  fseek(f,saved_pos,SEEK_SET);
  return 0;
}
