/*  VER 128   TAB P   $Id: main.c,v 1.23 1996/11/22 15:45:42 src Exp $
 *
 *  an NNTP news exchange client
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  Husebybakken 14A, N-0379 Oslo, Norway	  
 *
 *  this program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  this program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  see the
 *  GNU General Public License for more details.
 *
 *  you should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "common.h"
#include "proto.h"

#include <getopt.h>

static void pickargs(int argc,char *argv[],char *envp[]);
static void stringarg(char *arg,char **where);

/*
 *  main 
 */
int main(int argc, char *argv[], char *envp[])
{
    char *p,*q;
    int ok;

    /* initialize and set the name of the program */
    for (q=argv[0]; (p=strchr(q,'/')); ) q = p+1;
    stringarg(q,&pname);
    begintitle(argc,argv,envp);

    /* plus some command line defaults */
    spooldir = NEWSSPOOL;
    nopull_opt = 0;
    inn_flag = INN_MODE;
    minspool = MINSPOOL;
    hostport = "nntp";

    /* there may be an environment variable */
    spoolname = getenv("NNTPSERVER");

    /* parse the args */
    pickargs(argc,argv,envp);

    /* do what we were told */
    ok = doit();

    /* remove locks */
    history_done();
    unlock();

    return ok;
}

/*
 *  pick arguments from command line
 *  NOTE: the actual command line will be cleared after this
 */
static void pickargs(int argc,char *argv[],char *envp[])
{
    int c;
    char *p;
    char name[512];
	
    while ((c = getopt(argc,argv,"a:b:cde:f:ghikl:mnoprs:t:w:vx:y:z")) != EOF) {
	switch (c) {
	case 'a':			/* do an authinfo */
	    if (ai_username) goto usage;
	    get_authinfo(optarg);
	    break;
	case 'b':			/* min spool size in bytes */
	    minspool = atoi(optarg);		       
	    break;
	case 'c':			/* Cnews mode */
	    inn_flag=0;
	    break;
	case 'd':			/* debugging on */
	    debug_flag++;
	    break;
	case 'e':			/* end at tag */
	    if (end_tag) goto usage;
	    end_tag = strcpy(malloc(sizeof(optarg)+1),optarg);
	    break;
	case 'f':			/* folder */
	    stringarg(optarg,&folder);
	    break;
	case 'h':			/* no history lookup */
	    ++nohist_opt;
	    break;
	case 'g':			/* don't get news */
	    ++nopull_opt;
	    break;
	case 'i':			/* inn mode */
	    inn_flag=1;
	    break;
	case 'k':			/* keep "Path" */
	    keep_path_flag++;
	    break;
	case 'l':			/* logfile */
	    stringarg(optarg,&logfile);
	    break;
	case 'n':			/* do nothing */
	    noaction_flag++;
	    break;
	case 'm':			/* skip message ID */
	    nomsgid_flag++;
	    break;
	case 'o':			/* keep .old */
	    keep_old_flag++;
	    break;
	case 'p':			/* no posting */
	    nopost_opt++;
	    break;
	case 'r':			/* do a 'MODE READER' */
	    mode_reader_flag++;
	    break;
	case 's':			/* spool */
	    stringarg(optarg,&spooldir);
	    break;
	case 't':			/* timeout in seconds */
	    timeout = atoi(optarg);		       
	    break;
	case 'u':			/* no unlock by force */
	    noforce_flag++;
	    break;
	case 'w':			/* -w chat */
	    stringarg(optarg,&chat_file);
	    break;
	case 'v':			/* version */
	    printf("%s version %s\n", pname, VERSION);
	    unlock_exit(0); /* that's ok */
	    break;
	case 'x':			/* -x connect script */
	    stringarg(optarg,&connect_exec);
	    break;
	case 'y':			/* -y connect via */
	    stringarg(optarg,&via_exec);
	    break;
	case 'z':			/* zero incoming */
	    zap_flag++;
	    break;
	default:
	  usage:
	    /* BUG: have a more complete list! */
	    fprintf(stderr,"usage: %s spool [[server] port]\n", pname);
	    unlock_exit(2);
	}
    }
    
    /* get spool name */
    if (optind < argc) {
	if (argv[optind][0]) {
	    spoolname = 0;
	    stringarg(argv[optind],&spoolname);
	}
	++optind;
    } 
    if (!spoolname || !spoolname[0]) {
	fprintf(stderr, "spool name missing\n");
	goto usage;
    }

    /* get server name */
    if (optind < argc) {
	if (argv[optind][0]) {
	    hostname = 0;
	    stringarg(argv[optind],&hostname);
	}
	++optind;
    }
    if (!hostname || !hostname[0]) {
	hostname = spoolname;
    }
    
    /* get port name */
    if (optind < argc) {
	if (argv[optind][0]) {
	    hostport = 0;
	    stringarg(argv[optind],&hostport);
	}
	++optind;
    } 

    if (optind < argc) {
	fprintf(stderr, "excess arguments\n");
	goto usage;
    }

    /* set the program title for "ps" */
    sprintf(name,"%s(%s)",pname,spoolname);
    settitle(name);
}

/*
 *  handle string argument
 *  move to heap
 */
static void stringarg(char *arg,char **where)
{
    char *p;

    if (*where) {
	fprintf(stderr,"%s: option specified twice\n",pname);
	unlock_exit(2);
    }
    if (!(p = malloc(strlen(arg)+1))) {
	fprintf(stderr,"%s: out of memory\n",pname);
	unlock_exit(2);
    }
    *where = strcpy(p,arg);
}
