/*
** Copyright (c) 1996,1997 Thorsten Kukuk 
**
** This file is part of the NYS YP Server.
**
** The NYS YP Server is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS YP Server is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
** 
** You should have received a copy of the GNU General Public
** License along with the NYS YP Server; see the file COPYING.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Thorsten Kukuk <kukuk@uni-paderborn.de>
*/

static char rcsid[] = "$Id: makedbm.c,v 1.3 1997/01/03 19:02:32 kukuk Exp $";

#if defined(HAVE_CONFIG_H)
#include "config.h"
#endif 

#include "system.h"
#include "version.h"
#include "yp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <netdb.h>
#include <sys/param.h>
#include <sys/time.h>
#if defined(HAVE_GETOPT_H) && defined(HAVE_GETOPT_LONG)
#include <getopt.h>
#else
#include <compat/getopt.h>
#endif
#include <gdbm.h>

static GDBM_FILE dbm;
static int lower = 0;

#ifndef HAVE_GETOPT_LONG
#include <compat/getopt.c>
#include <compat/getopt1.c>
#endif

static inline void write_data(datum key, datum data)
{
  if (gdbm_store(dbm, key, data, GDBM_REPLACE) < 0)
    {
      perror("makedbm: dbm_store");
      gdbm_close(dbm);
      exit(1);
    }
}


static void create_file(char *fileName, char *dbmName, char *masterName,
			char *domainName, char *inputName, 
			char *outputName, int aliases, int shortlines,
			int b_flag, int s_flag)
{
  datum  kdat, vdat;
  char   key[8192];
  char   filename[1024];
  FILE   *input;
  char   orderNum[12];
  struct timeval tv;
  struct timezone tz;
   
  input=strcmp(fileName, "-")?fopen(fileName, "r"):stdin;
  if (input==NULL) 
    {
      fprintf(stderr, "makedbm: Cannot open %s\n", fileName);
      exit(1);
    }

  sprintf(filename, "%s~", dbmName);
  if ((dbm=gdbm_open(filename,0,GDBM_NEWDB|GDBM_FAST,0600,NULL))==NULL) 
    {
      fprintf(stderr, "makedbm: Cannot open %s\n", filename);
      exit(1);
    }


  if (masterName && *masterName) 
    {
      kdat.dptr="YP_MASTER_NAME"; kdat.dsize=strlen(kdat.dptr);
      vdat.dptr=masterName; vdat.dsize=strlen(vdat.dptr);
      write_data(kdat,vdat);
    }
  
  if (domainName && *domainName) 
    {
      kdat.dptr="YP_DOMAIN_NAME"; kdat.dsize=strlen(kdat.dptr);
      vdat.dptr=domainName; vdat.dsize=strlen(vdat.dptr);
      write_data(kdat,vdat);
    }
  
  if (inputName && *inputName) 
    {
      kdat.dptr="YP_INPUT_NAME"; kdat.dsize=strlen(kdat.dptr);
      vdat.dptr=inputName; vdat.dsize=strlen(vdat.dptr);
      write_data(kdat, vdat);
    }
  
  if (outputName && *outputName) 
    {
      kdat.dptr="YP_OUTPUT_NAME"; kdat.dsize=strlen(kdat.dptr);
      vdat.dptr=outputName; vdat.dsize=strlen(vdat.dptr);
      write_data(kdat,vdat);
    }

  if (b_flag)
    {
      kdat.dptr="YP_INTERDOMAIN"; kdat.dsize=strlen(kdat.dptr);
      vdat.dptr=""; vdat.dsize=strlen(vdat.dptr);
      write_data(kdat,vdat);
    }

  if (s_flag)
    {
      kdat.dptr="YP_SECURE"; kdat.dsize=strlen(kdat.dptr);
      vdat.dptr=""; vdat.dsize=strlen(vdat.dptr);
      write_data(kdat,vdat);
    }

  if (aliases)
    {
      kdat.dptr="@"; kdat.dsize=strlen(kdat.dptr);
      vdat.dptr="@"; vdat.dsize=strlen(vdat.dptr);
      write_data(kdat, vdat);
    }
  
  gettimeofday(&tv, &tz);
  sprintf(orderNum, "%ld",tv.tv_sec);
  kdat.dptr="YP_LAST_MODIFIED"; kdat.dsize=strlen(kdat.dptr);
  vdat.dptr=orderNum; vdat.dsize=strlen(vdat.dptr);
  write_data(kdat, vdat);

  while (fgets(key, sizeof(key)-1, input) != NULL)
    {
      char *cptr;
      
      /* Replace first '\n' with '\0' */
      if((cptr = strchr(key,'\n'))!=NULL)
	*cptr = '\0';

      if (aliases)
	{
	  int len;
	  
	  len = strlen(key);
	  while (key[len-1] == ' ' || key[len-1] == '\t')
	    {
	      key[len-1] = '\0';
	      len--;
	    }

	  while (key[len -1] == ',')
	    {
	      char buf[2049];

	      fgets(buf, sizeof(buf)-1, input);
	      cptr = buf;
	      while((*cptr == ' ') || (*cptr == '\t')) 
		++cptr;
	      if(strlen(key)+strlen(cptr) < sizeof(key))
		strcat(key,cptr);
	      if((cptr = strchr(key,'\n'))!=NULL)
		*cptr = '\0';
	      len = strlen(key);
	      while (key[len-1] == ' ' || key[len-1] == '\t')
		{
		  key[len-1] = '\0';
		  len--;
		}
	    }
	  if((cptr = strchr(key,':')) != NULL)
	    *cptr = ' ';
	}
      else
	while (key[strlen(key)-1] == '\\')
	  {
	    if(shortlines)
	      {
		char buf[2049];
		int len;

		len = strlen(key);
		key[len-1] = '\0';
		len--;
		if((key[len-1] != ' ') && (key[len-1] != '\t'))
		  strcat(key," ");
		fgets(buf, sizeof(buf)-1, input);
		cptr = buf;
		while((*cptr == ' ') || (*cptr == '\t')) 
		  ++cptr;
		if(len+1+strlen(cptr) < sizeof(key))
		  strcat(key,cptr);
	      }
	    else
	      fgets(&key[strlen(key) -1], sizeof(key) - strlen(key), input);
	    
	    if((cptr = strchr(key,'\n'))!=NULL)
	      *cptr = '\0';
	  }
      
      cptr = key;

      while (*cptr && *cptr != '\t' && *cptr != ' ')
	++cptr;
      
      if (strlen(key) == 0)
	fprintf(stderr, "makedbm: warning: malformed input data (ignored)\n");
      else
	{
	  int i;
	  
	  *cptr++ = '\0';

	  while(*cptr == '\t' || *cptr == ' ')
	    ++cptr;

	  if(strlen(key) > YPMAXRECORD)
	    {
	      fprintf(stderr,"makedbm: warning: key too long: %s\n",key);
	      continue;
	    }
	  kdat.dsize = strlen(key);
	  kdat.dptr = key;
	  
	  if(strlen(cptr) > YPMAXRECORD)
	    {
	      fprintf(stderr,"makedbm: warning: data too long: %s\n",cptr);
	      continue;
	    }
	  vdat.dsize = strlen(cptr);
	  vdat.dptr = cptr;
	  
	  if(lower)
	    for(i = 0; i < kdat.dsize; i++)
	      kdat.dptr[i] = tolower(kdat.dptr[i]);
	  
	  write_data(kdat, vdat);
	}
    }

  gdbm_close(dbm);
  unlink(dbmName);
  rename(filename, dbmName);
  
}

static void dump_file(char *dbmName)
{
  datum key, data;
  if ((dbm=gdbm_open(dbmName,0,GDBM_READER, 0600, NULL))==NULL) 
    {
      fprintf(stderr, "makedbm: Cannot open %s\n", dbmName);
      exit(1);
    }
  for (key=gdbm_firstkey(dbm); key.dptr; key=gdbm_nextkey(dbm,key)) 
    {
      data=gdbm_fetch(dbm,key);
      if (!data.dptr) 
	{
	  fprintf(stderr, "Error:\n");
	  perror(dbmName);
	  exit (1);
	}
      printf("%.*s %.*s\n",
	     key.dsize, key.dptr,
	     data.dsize, data.dptr);
      free(data.dptr);
    }
  gdbm_close(dbm);
}

static void send_clear(void)
{
  char in = 0;
  char *out = NULL;
  int stat;
  if ((stat = callrpc("localhost",YPPROG,YPVERS,YPPROC_CLEAR,
		      (xdrproc_t)xdr_void, &in,
		      (xdrproc_t)xdr_void, out)) != RPC_SUCCESS) 
    {
      fprintf(stderr,"failed to send 'clear' to local ypserv: %s",
	    clnt_sperrno((enum clnt_stat) stat));
    }
}

static void Usage( int exit_code)
{
  fprintf(stderr, "usage: makedbm -u dbname\n");
  fprintf(stderr, "       makedbm [-a|-r] [-b] [-s ] [-l] [-i YP_INPUT_NAME]\n");
  fprintf(stderr, "               [-o YP_OUTPUT_NAME] [-m YP_MASTER_NAME] inputfile dbname\n");
  fprintf(stderr, "       makedbm -c\n");
  fprintf(stderr, "       makedbm --version\n");
  exit(exit_code);
}

int main(int argc, char *argv[])
{
  char *domainName=NULL;
  char *inputName=NULL;
  char *outputName=NULL;
  char masterName[MAXHOSTNAMELEN+1] = "";
  int dump=0;
  int aliases=0;
  int shortline=0;
  int clear=0;
  int b_flag=0;
  int s_flag=0;

  while(1) 
    {
      int c;
      int option_index = 0;
      static struct option long_options[] =
      {
	{"version", no_argument, NULL, '\255'},
	{"dump", no_argument, NULL, 'u'},
	{"help", no_argument, NULL, 'h'},
	{"usage", no_argument, NULL, 'h'},
	{"secure", no_argument, NULL, 's'},
	{"aliases", no_argument, NULL, 'a'},
	{"send_clear", no_argument, NULL, 'c'},
	{"remove_spaces", no_argument, NULL, 'r'},
	{NULL, 0, NULL, '\0'}
      };

      c=getopt_long(argc, argv, "abcd:hi:lm:o:rsu",long_options, &option_index);
      if (c==EOF) break;
      switch (c) 
	{
	case 'a':
	  aliases++;
	  shortline++;
	  break;
	case 'b':
	  b_flag++;
	  break;
	case 'c':
	  clear++;
	  break;
	case 'l':
	  lower++;
	  break;
	case 'u': 
	  dump++;
	  break;
	case 'r':
	  shortline++;
	  break;
	case 's':
	  s_flag++;
	  break;
	case 'd':
	  domainName=optarg;
	  break;
	case 'i':
	  inputName=optarg;
	  break;
	case 'o':
	  outputName=optarg;
	  break;
	case 'm':
	  if(strlen(optarg) <= MAXHOSTNAMELEN)
	    strcpy(masterName,optarg);
	  else
	    fprintf(stderr,"hostname to long: %s\n",optarg);
	  break;
	case '\255':
	  fprintf(stderr,"makedbm - NYS YP Server version %s\n",version);
	  exit(0);
	case 'h':
	  Usage(0);
	  break;
	case '?':
	  Usage(1);
	  break;
	}
    }

  argc-=optind;
  argv+=optind;

  if (dump)
    {
      if(argc<1)
	Usage(1);
      else
	dump_file(argv[0]);
    }
  else
    {
      if(clear && argc == 0)
	{
	  send_clear();
	  return 0;
	}

      if (argc<2) 
	Usage(1);
      else
	{
	  if (strlen(masterName) == 0) 
	    if (gethostname(masterName, sizeof(masterName))<0) 
	      perror("gethostname");
	  
	  create_file(argv[0], argv[1], masterName, domainName, 
		      inputName, outputName, aliases, shortline, 
		      b_flag, s_flag);

	  /* 
	  ** XXX What does SunOS, if they update a map? Send
	  ** they always a YPPROC_CLEAR to the local ypserv ?
	  ** Or do they nothing ?
	  */
	  if(clear)
	    send_clear();
	}
    }

  return 0;
}
