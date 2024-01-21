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

static int put_file PROTO1(char *, path)
{
  struct stat sb;
  char *name, *t2;
  FILE *fp;
  
  if(stat(path,&sb) != 0) {
    perror(path);
    return;
  }
  if(!(S_ISREG(sb.st_mode))) {
    fprintf(stderr,"%s: not a file\n",path);
    return;
  }
  
  for(name = t2 = path; *t2; t2++)
    if(*t2 == '/') name = t2 + 1;
  
  if(fp = fopen(path,"r")) {
    util_upload(name,fp);
    fclose(fp);
  } else fprintf(stderr,"Cannot read %s\n",path);
}

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  char n[1024];
  int prompt;
  
  env_client();
  if (strcmp(env_local_dir,".") && chdir(env_local_dir)) {
    perror("chdir");
    exit(1);
  }
  
  if(argc > 1)
    while(*++argv) put_file(*argv);
  else {
    prompt = isatty(0);
    while(1) {
      if(prompt) {
	fputs("fput: ",stdout);
	fflush(stdout);
      }
      if(!gets(n)) break;
      if(!*n) continue;
      put_file(n);
    }
  }
  
  client_done();
  
  exit(0);
}
