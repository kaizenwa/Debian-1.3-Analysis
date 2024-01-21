    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "client_def.h"
#include "c_extern.h"
#include "bsd_extern.h"
#include <signal.h>

int grab_clobbertype=CLOBBERONFIND;
char *fname;
int optletter;
int suffix;
char *tname;
unsigned long start_from;
extern int optind;

static void fsp_cleanup PROTO1(int, signum)
{
  char filename[20];
#ifndef VMS
  sprintf(filename,".fsp.%d",getpid());
#else /* no more than one dot allowed in filenames */
  sprintf(filename,"fsp_%d",getpid());
#endif
  unlink(filename);
  exit(1);
}

static int grab_file PROTO1(char *, path)
{
  char *name, *t2;
#ifdef VMS
  char filename[255];
#endif
  FILE *fp;
  struct stat statbuf;
  
  for(name = t2 = path; *t2; t2++)
    if(*t2 == '/') name = t2 + 1;
  
#ifdef VMS /* convert more than one dot to underscores */
  strcpy(filename,name); name=filename;
  convdots(name, filename);
#endif /* VMS */
  
  if (grab_clobbertype==NOCLOBBER) {
    if (fp=fopen(name,"r")) {
      fclose(fp);
      fprintf(stderr,"Will not overwrite existing file %s\n",name);
      return;
    }
  }
  
  if (grab_clobbertype==UNIQUE) {
    fname=name;
    name=(char *)malloc(strlen(fname)+5);
    strcpy(name,fname);
    for (suffix=0 ; (fp=fopen(name,"r")) ; suffix++) {
      fclose(fp);
      sprintf(name,"%s-%d",fname,suffix);
    }
  }
  
  if (grab_clobbertype==CLOBBERONFIND) {
    fname=name;
    name=(char*)malloc(20);
#ifdef VMS /* no more than one dot allowed in filenames */
    sprintf(name,"fsp_%d",getpid());
#else
    sprintf(name,".fsp.%d",getpid());
#endif
  }
  
  if(grab_clobbertype == APPEND) {
    if(stat(name, &statbuf) == 0) {
      start_from = statbuf.st_size;
      if((fp = fopen(name,"a")) == NULL) perror(name);
    } else start_from = -1;
  } else start_from = -1;
  
  if(start_from == -1) {
    fp = fopen(name, "w");
    start_from = 0;
  }
  
  if(fp) {
    if(util_grab_file(path,fp,start_from) == -1) {
      fclose(fp); unlink(name);
    } else
      fclose(fp);
  } else fprintf(stderr,"Cannot write %s\n",name);
  
  if (grab_clobbertype==CLOBBERONFIND) {
    rename(name,fname);
    free(name);
  }
  
  if (grab_clobbertype==UNIQUE) {
    free(name);
  }
}

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  char **av, *av2[2], n[1024];
  int prompt;
  
  signal(SIGHUP,fsp_cleanup);
  signal(SIGINT,fsp_cleanup);
  signal(SIGQUIT,fsp_cleanup);
  signal(SIGILL,fsp_cleanup);
  signal(SIGTRAP,fsp_cleanup);
  signal(SIGFPE,fsp_cleanup);
#ifndef __linux__
  signal(SIGBUS,fsp_cleanup);
  signal(SIGEMT,fsp_cleanup);
  signal(SIGSYS,fsp_cleanup);
#endif
  signal(SIGSEGV,fsp_cleanup);
  signal(SIGPIPE,fsp_cleanup);
  signal(SIGTERM,fsp_cleanup);
  
  env_client();
  if (strcmp(env_local_dir,".") && chdir(env_local_dir)) {
    perror("chdir");
    exit(1);
  }
  
  /* Parse options
   * -f forces overwrite
   * -u forces unique names
   * -t uses temporary file to download
   * -n forces noclobber
   * -a append to files if they exist
   */
  while ((optletter=getopt(argc, argv,"futna")) != EOF)
    switch (optletter) {
      case 'f':
        grab_clobbertype=CLOBBER;
	break;
      case 'u':
	grab_clobbertype=UNIQUE;
	break;
      case 't':
	grab_clobbertype=CLOBBERONFIND;
	break;
      case 'n':
	grab_clobbertype=NOCLOBBER;
	break;
      case 'a':
	grab_clobbertype = APPEND;
    }
  
  if(argc > optind) {
    for(; argc > optind; optind++) {
      if(!(av = glob(argv[optind]))) {
	av = av2;
	av2[0] = argv[optind];
	av2[1] = 0;
      }
      while(*av) grab_file(*av++);
    }
  } else {
    prompt = isatty(0);
    while(1) {
      if(prompt) {
	fputs("fgrab: ",stdout);
	fflush(stdout);
      }
      if(!gets(n)) break;
      if(!*n) continue;
      if(!(av = glob(n))) {
	av = av2;
	av2[0] = n;
	av2[1] = 0;
      }
      while(*av) grab_file(*av++);
    }
  }
  
  client_done();
  
  exit(0);
}
