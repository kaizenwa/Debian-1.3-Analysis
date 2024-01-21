    /*********************************************************************\
    *  Copyright (c) 1993 by Michael Meskes                               *
    *  (meskes@ulysses.informatik.rwth-aachen.de)                         *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "client_def.h"
#include "c_extern.h"
#include <stdio.h>
#include "my-string.h"
#ifndef VMS
#include <pwd.h>
#else
#include "pwd.h"
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#ifdef HOST_LOOKUP
#include <netdb.h>
#endif

#ifdef VMS
#define FSP_STAT vms_stat
#else
#define FSP_STAT stat
#endif

#define NUMBER 1
#define NAME 2

int delay = -1, local_port = -1, timeout = -1, trace = -1, port = -1;
char *local_dir = NULL, *filename = NULL, *password = NULL;
char *hostname = NULL, *hostaddr = NULL, *dir = NULL;

extern char *optarg;
extern int optind;

char *buffer=0L,*home="/";

void host_usage PROTO0((void)) /* print usage message */
{
  fprintf(stderr,"Usage: fhost [-d delay] [-p local port] [-l local dir]\n");
  fprintf(stderr,"             [-o timeout] [-t trace] [-w password]\n");
  fprintf(stderr,"             [-f filename] [-h [number|name]]\n");
  fprintf(stderr,"             [host port [directory] | abbreviation]\n");
  exit(0);
}

char *load_file PROTO1(char *, filename) /* load resource file */
{
  FILE *stream;
  long i,length;
  struct stat sf;
  char *f1=".fsp_prof", *f2=0L, *f3=FSPRC;
  
  if (!filename) {
    if (!FSP_STAT(f1,&sf)) filename=f1;
    else {
      f2=(char *)malloc (strlen(home) + 7);
      if (!f2) {
	perror("malloc");
	return(0L);
      }
      sprintf (f2,"%s/.fsp_prof",home);
      if (!FSP_STAT(f2,&sf)) filename=f2;
      else filename=f3;
    }
  }
  stream=fopen(filename,"r");
  if (!stream) {
    perror(filename);
    if (f2) free(f2); return(0L);
  }
  i=fseek(stream,0L,2);
  if (i) {
    perror("fseek");
    fclose(stream);
    if (f2) free(f2);
    return(0L);
  }
  length=ftell(stream);
  if (!length) {
    perror("ftell");
    fclose(stream);
    if (f2) free(f2);
    return(0L);
  }
  buffer=(char *)calloc(length,sizeof(char));
  if (!buffer) {
    perror("malloc");
    fclose(stream);
    if (f2) free(f2);
    return(0L);
  }
  i=fseek(stream,0L,0);
  if (i) {
    perror("fseek");
    free(buffer);
    fclose(stream);
    if (f2) free(f2);
    return(0L);
  }
  fread(buffer,(long)sizeof(char),length,stream);
  if (ferror(stream)) {
    perror("fread");
    free(buffer);
    fclose(stream);
    if (f2) free(f2);
    return(0L);
  }
  i=fclose(stream);
  if (i) {
    perror("fclose");
    free(buffer);
    if (f2) free(f2);
    return(0L);
  }
  if (f2) free(f2);
  return(buffer);
}

/* get data out of resource file */
void parse_file PROTO2(char *, filename, char *, abb)
{
  char *ptrh,*ptrp,*ptra,*ptre;
  int found=0;
  
  ptrh=buffer=load_file(filename);
  if (!buffer) exit(1);
  while ((ptrh=(char *)strstr(ptrh,"host ")) && !found) {
    ptrh+=5;
    for (ptre=ptrh;*ptre!='\n';ptre++);
    ptra=(char *)strstr(ptrh,abb);
    if (ptra && ptra<ptre) {
      found=1;
      /* get host name or number */
      for (ptrp=ptrh;!hostname && *ptrp!=' ' && *ptrp!='\n';ptrp++)
	if (!isdigit(*ptrp) && *ptrp!='.') hostname=ptrh;
      if (!hostname) hostaddr=ptrh;
      /* does the othe one exist, too? */
      for (;*ptrp!=' ' && *ptrp!='\n';ptrp++);
      for (;*ptrp==' ';ptrp++); 
      if (*ptrp!='\n') {
	if (hostaddr) hostname=ptrp;
	else if (hostname) {
	  hostaddr=ptrp;
	  for (;*ptrp!=' ' && *ptrp!='\n';ptrp++)
	    if (!isdigit(*ptrp) && *ptrp!='.') {
	      fprintf(stderr, "fhost: syntax error in setup file\n");
	      free(buffer);
	      exit(1);
	    }
	}
      }
      for (ptrh++;*ptrh!='\n';ptrh++);
      ptrp=(char *)strstr(ptrh,"fsp port ");
      if (!ptrp) {
	fprintf(stderr,"fhost: syntax error in setup file\n");
	free(buffer);
	exit(1);
      }
      port=atol(ptrp+9);
      ptre=(char *)strstr(ptrh,"host "); /* begin next host */
      /* search for environment values */
      if (delay==-1) {
	ptra=(char *)strstr(ptrh,"delay ");
	if (ptra && (!ptre || ptra<ptre)) delay=atol(ptra+6);
      }
      if (timeout==-1) {
	ptra=(char *)strstr(ptrh,"timeout ");
	if (ptra && (!ptre || ptra<ptre)) timeout=atol(ptra+8);
      }
      if (trace==-1) {
	ptra=(char *)strstr(ptrh,"trace ");
	if (ptra && (!ptre || ptra<ptre))	{
	  for (ptra+6;*ptra!='o' && *ptra!='\n';ptra++);
	  if (*ptra=='o') trace=(ptra[1]=='n') ? 1 : 0;
	  else {
	    fprintf(stderr, "fhost: syntax error in setup file\n");
	    free(buffer);
	    exit(1);
	  }
	}
      }
      if (local_port==-1) {
	ptra=(char *)strstr(ptrh,"local port ");
	if (ptra && (!ptre || ptra<ptre)) local_port=atol(ptra+11);
      }
      if (local_dir==0L) {
	ptra=(char *)strstr(ptrh,"local directory ");
	if (ptra && (!ptre || ptra<ptre)) local_dir=ptra+16;
      }
      if (dir==0L) {
	ptra=(char *)strstr(ptrh,"fsp directory ");
	if (ptra && (!ptre || ptra<ptre)) dir=ptra+14;
      }
      if (password==0L) {
	ptra=(char *)strstr(ptrh,"password ");
	if (ptra && (!ptre || ptra<ptre)) password=ptra+16;
      }
    }
  }
  if (!found) fprintf(stderr,"host %s not found\n",abb);
  return;
}

void list_file PROTO1(char *, filename) /* list resource file */
{
  char *ptrh,*ptrp,*ptre;
  
  ptrh=buffer=load_file(filename);
  if (!buffer) exit(1);
  while (ptrh=(char *)strstr(ptrh,"host ")) {
    ptrh+=5;
    fprintf(stderr,"host: ");
    for (;*ptrh!='\n';ptrh++) fprintf(stderr,"%c",*ptrh);
    ptrp=(char *)strstr(ptrh,"fsp port ");
    ptre=(char *)strstr(ptrh,"host ");
    if (!ptrp || (ptre && ptrp>=ptre)) {
      fprintf(stderr,"fhost: syntax error in setup file\n");
      free(buffer);
      exit(1);
    }
    fprintf(stderr," port: ");
    for (ptrp+=9;*ptrp!='\n';ptrp++) fprintf(stderr,"%c",*ptrp);
    fprintf(stderr,"\n\n");
  }
  return;
}

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  int optletter,csh,host=0;
  register char *p;
  char *log,*homedir="/";
  struct passwd *pw=0L;
  struct hostent *hp;
  long addr;

#ifndef VMS  
  log = (char *)getlogin();
  if (log) pw = getpwnam(log);
  if (!pw) pw = getpwuid(getuid());
  if (pw) {
    /*
     * Figure out what shell we're using.  A hack, we look for a shell
     * ending in "csh".
     */
    csh = !strcmp(pw->pw_shell + strlen(pw->pw_shell) - 3, "csh");
    home = pw->pw_dir;   /* for default search for file .fsp_prof*/
  }
#endif

  while ((optletter=getopt(argc, argv,"d:p:l:t:o:f:h:w:")) != EOF)
    switch (optletter) {
      case 'd':
        delay=atol(optarg); /* FSP_DELAY */
	break;
      case 'p':
	local_port=atol(optarg); /* FSP_LOCAL_PORT */
	break;
      case 'w':
	password=optarg; /* FSP_PASSWORD */
	break;
      case 'l':
	local_dir=optarg; /* FSP_LOCAL_DIR */
	break;
      case 'o':
	timeout=atol(optarg); /* FSP_TIMEOUT */
	break;
      case 't':
	if (!strcmp(optarg,"on")) trace=1;  /* FSP_TRACE */
	else if (!strcmp(optarg,"off")) trace=0;
	else host_usage();
	break;
      case 'f':
	filename=optarg; /* file name */
	break;
      case 'h':
	if (!strcmp(optarg,"number")) host=NUMBER; /* host mode */
	else if (!strcmp(optarg,"name")) host=NAME;
	else host_usage();
	break;
      default:
	host_usage();
	break;
    }

  if(argc > optind+1 && !filename) { /* host and port and no filename given */
    for (p=argv[optind];!hostname && *p && *p!='\n';p++)
      if (!isdigit(*p) && *p!='.') hostname=argv[optind];
    if (!hostname) hostaddr=argv[optind];
    port=atol(argv[optind+1]);
    if (argc > optind + 1) dir=argv[optind+2]; /* directory given, too */
  } else if (argc > optind) { /* abbreviation given */
    parse_file(filename, argv[optind]);
  } else { /* list or set command-line options */
    if (filename || argc==1) {  /* list only */
      list_file(filename);
      exit(0);
    }
  }
  if (delay>=0) {
#ifdef VMS
    printf("$define/nolog FSP_DELAY %d\n",delay);
#else
    if (csh) printf("setenv FSP_DELAY %d;\n",delay);
    else printf("FSP_DELAY=%d;\nexport FSP_DELAY;\n",delay);
#endif
  }
  if (local_port>=0) {
#ifdef VMS
    printf("$define/nolog FSP_LOCALPORT %d\n",local_port);
#else
    if (csh) printf("setenv FSP_LOCALPORT %d;\n",local_port);
    else printf("FSP_LOCALPORT=%d;\nexport FSP_LOCALPORT;\n",local_port);
#endif
  }
  if (trace>=0) {
    if (csh) {
#ifdef VMS
      printf("$define/nolog FSP_TRACE \"1\"\n");
#else
      if (trace) printf("setenv FSP_TRACE;\n");
      else printf("unsetenv FSP_TRACE;\n");
#endif
    } else {
#ifdef VMS
      printf("$deassign FSP_TRACE\n");
#else
      if (trace) printf("FSP_TRACE;\nexport FSP_TRACE;\n");
      else printf("unset FSP_TRACE;\n");
#endif
    }
  }
  if (timeout>=0) {
#ifdef VMS
    printf("$define/nolog FSP_TIMEOUT %d\n",timeout);
#else
    if (csh) printf("setenv FSP_TIMEOUT %d;\n",timeout);
    else printf("FSP_TIMEOUT=%d;\nexport FSP_TIMEOUT;\n",timeout);
#endif
  }
  if (port>=0) {
#ifdef VMS
    printf("$define/nolog FSP_PORT %d\n",port);
#else
    if (csh) printf("setenv FSP_PORT %d;\n",port);
    else printf("FSP_PORT=%d;\nexport FSP_PORT;\n",port);
#endif
  }
  if (local_dir) {
#ifdef VMS
    printf("$define/nolog FSP_LOCAL_DIR \"");
#else
    if (csh) printf("setenv FSP_LOCAL_DIR ");
    else printf("FSP_LOCAL_DIR=");
#endif
    for (p=local_dir;*p && *p!='\n' && *p!=' ';p++) printf("%c",*p);
#ifdef VMS
    printf("\"\n");
#else
    if (csh) printf(";\n");
    else printf(";\nexport FSP_LOCAL_DIR;\n");
#endif
  }
  if (password) {
#ifdef VMS
    printf("$define/nolog FSP_PASSWORD \"");
#else
    if (csh) printf("setenv FSP_PASSWORD ");
    else printf("FSP_PASSWORD=");
#endif
    for (p=password;*p && *p!='\n' && *p!=' ';p++) printf("%c",*p);
#ifdef VMS
    printf("\"\n");
#else
    if (csh) printf(";\n");
    else printf(";\nexport FSP_PASSWORD;\n");
#endif
  }
  if (hostname || hostaddr) {
#ifdef VMS
    printf("$define/nolog FSP_HOST \"");
#else
    if (csh) printf("setenv FSP_HOST ");
    else printf("FSP_HOST=");
#endif
    if(hostname) {
      for(p = hostname; *p && *p!='\n' && *p!= ' '; p++);
      *p = 0;
    }
    if(hostaddr) {
      for(p=hostaddr;*p && *p !='\n' && *p!=' ';p++);
      *p = 0;
    }
    if(host==NAME && !hostname) {
#if HOST_LOOKUP
      addr=inet_addr(hostaddr);
      if (hp=gethostbyaddr((char *) &addr, sizeof(addr), AF_INET))
	hostname= (char *)hp->h_name;
#endif
      if (!hostname) host=NUMBER;
    }
    if (host==NUMBER && !hostaddr) { /* look for number */
#if HOST_LOOKUP
      if (hp=gethostbyname(hostname))
	hostaddr=(char *)inet_ntoa(*(struct in_addr *) * hp->h_addr_list);
#endif
      if (!hostaddr) host=NAME;
    }
    if (!host) {
      if (hostaddr) host=NUMBER;
      else if (hostname) host=NAME;
      else {
	fprintf(stderr,"fhost: No host given!");
	if (buffer) free(buffer);
	exit(1);
      }
    }
    printf("%s", (host==NAME)? hostname : hostaddr);
#ifdef VMS
    printf("\"\n");
#else
    if (csh) printf(";\n");
    else printf(";\nexport FSP_HOST;\n");
#endif
    if (!dir) dir=homedir; /* if host is set we need this */
  }
  if (dir) {
#ifdef VMS
    printf("$define/nolog FSP_DIR \"");
#else
    if (csh) printf("setenv FSP_DIR ");
    else printf("FSP_DIR=");
#endif
    for (p=dir;*p && *p!='\n' && *p!=' ';p++) printf("%c",*p);
#ifdef VMS
    printf("\"\n");
#else
    if (csh) printf(";\n");
    else printf(";\nexport FSP_DIR;\n");
#endif
  }

#ifdef VMS
  if (hostname) printf("$define/nolog FSP_NAME \"");
  else printf("$deassign FSP_NAME\n");
#else
  if (csh) printf("setenv FSP_NAME \"");
  else printf("FSP_NAME=\"");
#endif
  if (hostname)
    for (p=hostname;*p && *p!='\n' && *p!=' ';p++) printf("%c",*p);
#ifdef VMS
  if (hostname) printf("\"\n");
#else
  if (csh) printf("\";\n");
  else printf("\";\nexport FSP_NAME;\n");
#endif
  if (buffer) free(buffer);
  exit(0);
}
