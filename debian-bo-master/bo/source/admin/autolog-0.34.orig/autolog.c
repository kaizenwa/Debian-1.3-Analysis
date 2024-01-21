/*  "@(#) autolog.c      0.34
    Originally ported by David Dickson" 
    Modified by Michael C. Mitchell, 15Oct94
    Rewritten by Kyle Bateman, Nov94, Aug95

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include    <stdio.h>
#include    <signal.h>
#include    <string.h>
#include    <sys/types.h>
#include    <sys/stat.h>
#include    <utmp.h>
#include    <time.h>
#include    <pwd.h>
#include    <grp.h>
#include	<regex.h>
#include	<malloc.h>

#define     D_IDLE      30  /* maximum idle time (minutes) */
#define     D_GRACE     30  /* grace time (sec) for user reply */
#define     D_HARD      0   /* consider connect time only */
#define     D_MAIL      1   /* notify user by mail */
#define     D_CLEAR     0   /* clear user terminal before warning */
#define     D_WARN      1   /* warn user before killing */
#define     D_LOG       1   /* log activity in logfile */

#define     WARNING     1   /* a warning message */
#define     LOGOFF      2   /* a log-off message */
#define     NOLOGOFF    3   /* a log-off failure message */

#define     KWAIT       20  /* time to wait after kill (in sec.) */
#define     STRLEN      64
#define     LINELEN     256
#define     MAXCONF     512 /* maximum lines in config file */

struct utmp *utmpp;         /* pointer to utmp file entry */
char delims[] = "\n\t ";    /* valid delimiters in config file */
char anystrg[] = ".+";      /* matches anything */

int debug = 0;
int nokill = 0;
int listall = 0;
char *confname = "/etc/autolog.conf";
char *logfname = "/var/log/autolog.log";
int g_idle = D_IDLE;
int g_grace = D_GRACE;
int g_hard = D_HARD;
int g_mail = D_MAIL;
int g_clear = D_CLEAR;
int g_warn = D_WARN;
int g_log = D_LOG;

typedef struct conf
    {
    char    *name;          /* user name */
    char    *group;         /* group name */
    char    *line;          /* tty line */
    int     idle;           /* minutes of idle time */
    int     grace;          /* minutes of grace time */
    int     hard;           /* consider connect time only */
    int     mail;           /* notify by mail */
    int     clear;          /* do screen clears before warn */
    int     warn;           /* warn before kill */
    int     log;            /* log actions to logfile */
    } conf_el;
conf_el c_arr[MAXCONF];
int c_idx = 0;

main(int argc, char *argv[])
    {
    int i;
    for (i = 1; i < argc; i++)
        if (argv[i][0] == '-')
            switch(argv[i][1])
                {
                case 'a':

                    listall = 1;
                    break;
                case 'd':
                    debug = 1;
                    break;
                case 'n':
                    nokill = 1;
                    break;
                case 'h':
                    g_hard = 1;
                    break;
                case 'l':
                    logfname = argv[++i];
                    break;
                case 'f':
                    confname = argv[++i];
                    break;
                case 't':
                    g_idle = atoi(argv[++i]);
                    break;
                case 'g':
                    g_grace = atoi(argv[++i]);
                    break;
                case 'm':
                    g_mail = (argv[++i][0] == 'y');
                    break;
                case 'c':
                    g_clear = (argv[++i][0] == 'y');
                    break;
                case 'w':
                    g_warn = (argv[++i][0] == 'y');
                    break;
                case 'L':
                    g_log = (argv[++i][0] == 'y');
                    break;
                default:
                    fprintf(stderr,"autologout: illegal switch: %s\n", argv[i]);
                }
        else
            fprintf(stderr,"autologout: illegal parameter: %s\n", argv[i]);
    eat_confile();              /* read config file */
    if (!debug)                 /* if not in debug mode, */
        if (fork())             /* the parent process */
            exit(0);            /* exits */

    /* the child processes all utmp file entries: */
    while ((utmpp = getutent()) != (struct utmp *) NULL)
        check_idle();
    exit(0);                    /* done, so bye */
    }

set_defs(int i)
    {
    c_arr[i].name = anystrg;
    c_arr[i].group = anystrg;
    c_arr[i].line = anystrg;
    c_arr[i].idle = g_idle;
    c_arr[i].grace = g_grace;
    c_arr[i].hard = g_hard;
    c_arr[i].mail = g_mail;
    c_arr[i].clear = g_clear;
    c_arr[i].warn = g_warn;
    c_arr[i].log = g_log;
    }

eat_confile()
    {
    FILE *f;
    char *s, iline[LINELEN];
    int i, lev;
    
    if (!(f=fopen(confname, "r")) )
        {
        if (debug)
            printf("Can't find file: %s\n", confname);
        }
    else
        {
        while (fgets(iline, LINELEN, f))
            {
            if (*iline == '#' || strlen(iline) <= 1)
                continue;
            set_defs(c_idx);
            s=strtok(iline,delims);
            do
                {
                lev = 1;
                if (!strncmp(s,"name=",5) && *(s+=5))
                    c_arr[c_idx].name=strcpy((char *)malloc(strlen(s)+1),s);
                else if (!strncmp(s,"group=",6) && *(s+=6))
                    c_arr[c_idx].group=strcpy((char *)malloc(strlen(s)+1),s);
                else if (!strncmp(s,"line=",5) && *(s+=5))
                    c_arr[c_idx].line=strcpy((char *)malloc(strlen(s)+1),s);
                else if (!strncmp(s,"idle=",5) && *(s+=5))
                    c_arr[c_idx].idle=atoi(s);
                else if (!strncmp(s,"grace=",6) && *(s+=6))
                    c_arr[c_idx].grace=atoi(s);
                else
                    {
                    if (!strncmp(s,"no",2))
                        {
                        lev=0;
                        s+=2;
                        }
                    if (!strcmp(s,"hard"))
                        c_arr[c_idx].hard=lev;
                    else if (!strcmp(s,"clear"))
                        c_arr[c_idx].clear=lev;
                    else if (!strcmp(s,"mail"))
                        c_arr[c_idx].mail=lev;
                    else if (!strcmp(s,"warn"))
                        c_arr[c_idx].warn=lev;
                    else if (!strcmp(s,"log"))
                        c_arr[c_idx].log=lev;
                    else if (debug)
                        printf("Unknown token in file: %s: %s\n",confname,s);
                    }
                }
            while(s=strtok(0,delims));
            c_idx++;
            }
        fclose(f);
        }
    if (!c_idx)                 /* if no entries made yet */
        set_defs(c_idx++);      /* make one */
    if (debug)
        for(i=0;i < c_idx; i++)
            printf("name=%s group=%s line=%s idle=%d grace=%d mail=%d warn=%d log=%d\n",
                c_arr[i].name,c_arr[i].group,c_arr[i].line,c_arr[i].idle,
                c_arr[i].grace,c_arr[i].mail,c_arr[i].warn,c_arr[i].log);
    }

/* I've apparently found some bug in certain LINUX versions of the */
/* regular expressions library routines.  One of these following two */
/* versions of the routine should work for you. */
#define AVOID_REGEX_BUG
#ifdef AVOID_REGEX_BUG				/* some strange bug in re_exec */
pat_match(char *patt, char *strg)
    {
    struct re_pattern_buffer rpb;
    int len, retval = 0;
	
/*    if (debug)
        printf("pat_match:%s:%s:\n",patt,strg); */
    len = strlen(patt+256);
    rpb.buffer = malloc(len);
    rpb.allocated = len;
    rpb.fastmap = 0;
    rpb.translate = 0;
    if (!re_compile_pattern(patt,strlen(patt),&rpb))
        {
        if (re_match(&rpb,strg,strlen(strg),0,0) == strlen(strg))
            retval = 1;
        }
    free(rpb.buffer);
    return(retval);
    }

#else
/* return true if strg matches the regex in pattern */
pat_match(char *pattern,char *strg)
    {
    re_comp(pattern);
    return(re_exec(strg));
    }
#endif

check_idle()            /* select utmp entries needing killing */
    {
    char dev[STRLEN], name[STRLEN], prname[STRLEN], *gn = "";
    struct stat status;
    time_t idle, pres_time, start, time(), stime, atime;
    struct passwd *passwd_entry;
    struct group *group_entry;
    conf_el *ce;
    int i;

    if (utmpp->ut_type != USER_PROCESS)     /* if not a user process */
        {
        if (listall)
            printf("Non-user process: N:%-8s P:%5d Login:%s",utmpp->ut_user,utmpp->ut_pid,ctime(&utmpp->ut_time));
        return(0);                          /* skip the utmp entry */
        }
    sprintf(prname,"/proc/%d",utmpp->ut_pid);   /* make filename to check in /proc */
    if (stat(prname, &status))                  /* is this a current process */
        {
        if (listall)
            printf("Dead process: N:%-8s P:%5d Login:%s",utmpp->ut_user,utmpp->ut_pid,ctime(&utmpp->ut_time));
        return(0);
        }
        
    sprintf(dev,"/dev/%s",utmpp->ut_line);  /* append /dev/ to base name */
    if (stat(dev, &status))                 /* if can't get status for port */
        bailout("Can't get status of user's terminal", 1);

    /* idle time = current time less last access time: */
    time(&pres_time);                           /* get current time */
    atime = (status.st_atime > utmpp->ut_time) ? status.st_atime : utmpp->ut_time; /* get last access time */
    idle = (pres_time - atime) / 60;			/* total idle minutes */
    stime = (pres_time - utmpp->ut_time) / 60;  /* total session minutes */
    strncpy(name, utmpp->ut_user, UT_NAMESIZE); /* get user name */
    name[UT_NAMESIZE] = '\0';           /* null terminate user name string */

    if (debug)
        printf("\nChecking: %-11s on %-12s I:%-4d T:%d Login: %s",name,dev,idle,utmpp->ut_type,ctime(&utmpp->ut_time));

    /* now try to find the group of this person */
    /* if usernames in utmp are limited to 8 chars, we will may fail on */
    /* names that are longer than this, so we'll try to find it by uid */
    if (!(passwd_entry = getpwnam(name)))       /* If can't find by name */
        passwd_entry = getpwuid(status.st_uid); /* try by uid */
    if (passwd_entry)
        {
        strcpy(name,passwd_entry->pw_name);
        if(group_entry = getgrgid( passwd_entry->pw_gid ))
            gn = group_entry->gr_name;
        else if (debug)
            printf("Can't find group entry for user: %s\n",name);
        }
    else if (debug)
        printf("Can't find password entry for user: %s\n",name);
    
    for(i = 0; i < c_idx; i++)
        if (pat_match(c_arr[i].name,name) && 
            pat_match(c_arr[i].group,gn) &&
            pat_match(c_arr[i].line,utmpp->ut_line))
            {
            if (debug)
                printf("Match #%2d: U:%-12s Grp:%-8s Line:%-9s Pid:%-6d Sess:%3d:%02d\n",
                    i+1,name,gn,utmpp->ut_line,utmpp->ut_pid,stime/60,stime%60);
            if (!c_arr[i].idle)     /* if user exempt (idle=0) */
                {
                if (debug)
                    printf("User exempt\n");
                return(0);          /* then don't kill him */
                }
            ce = &c_arr[i];         /* get address of matched record */
            break;
            }
    if (i >= c_idx)
        {
        if (debug)
            printf("No match for process\n");
        return(0);
        }
        
    if (!c_arr[i].hard)                 /* if considering idle time */
        {
        if (debug)
            printf("Subject to logout   Idle time: %4d (%2d allowed)\n",idle,ce->idle);
        if (idle < ce->idle)    /* if user was recently active */
            return(0);                  /* let it live */
        }
    else
        {
        if (debug)
            printf("Subject to logout   Total time: %4d (%2d allowed)\n",stime,ce->idle);
        if (stime < ce->idle)   /* if user still under limit */
            return(0);                  /* let it live */
        }
    if (nokill)
        {
        if (debug)
            printf("Would kill this process\n");
        return(1);
        }
    if (debug)
        printf("Warning user:%s Line:%s  Sleep %d sec\n",name,utmpp->ut_line,ce->grace);
    mesg(WARNING, name, dev, stime, idle, ce); /* send warning to user */
    if (stat(dev, &status))
        bailout("Can't get status of user's terminal", 2);
    start = status.st_atime;                /* start time for countdown */
    sleep(ce->grace);
    if (stat(dev, &status))
        bailout("Can't get status of user's terminal", 3);
    if (ce->hard || start >= status.st_atime)   /* user still idle */
        {
        if (debug)
            printf("Killing user:%s Line:%s  Sleep %d sec\n",name,utmpp->ut_line,KWAIT);
        if (killit(utmpp->ut_pid))
            {
            mesg(LOGOFF, name, dev, stime, idle, ce); /* send warning to user */
            return(1);
            }
        mesg(NOLOGOFF, name, dev, stime, idle, ce); /* couldn't kill */
        }
    return(0);
    }

mesg(int flag, char *name, char *dev, int stime, int idle, conf_el *ce)
    {
    char    mbuf[LINELEN];          /* message buffer */
    time_t  tvec;
    FILE    *fp, *log = 0, *mprog;
    struct stat status;

    time(&tvec);                /* tvec = current time */
    if (stat(logfname, &status) >= 0)           /* if logfile exists */
        log = fopen(logfname, "a"); /* append to it */
    if (ce->clear) 
        {
        sprintf (mbuf,"clear >%s",dev);
        system (mbuf);
        }
    if (flag == WARNING && ce->warn)
        {                       /* process warning message */
        if (!(fp = fopen(dev, "w")) )
            bailout("Can't open user's terminal", 5);
        if (ce->hard)
            {
            fprintf(fp,"%s: You've been on for %d min.\07\n", name, stime);
            fprintf(fp,"You'll be logged off in %d sec. so finish up:",ce->grace);
            }
        else
            {
            fprintf(fp,"%s: You've been idle for %d min.\07\n", name, idle);
            fprintf(fp,"You'll be logged off in %d sec unless you hit a key",ce->grace);
            }
        fclose(fp);
        if (log && ce->log)
            fprintf(log, "** NOTIFIED ** %s %s idle:%d sess:%d %s",name, dev+5, idle, stime, ctime(&tvec)+3);
        }
    if (flag == LOGOFF) 
        {                       /* process log-off message */
        if (log && ce->log)
            fprintf(log, "** LOGOFF ** %s %s idle:%d sess:%d %s",name, dev+5, idle, stime, ctime(&tvec)+3);
        if (ce->mail)
            {
            sprintf(mbuf, "/bin/mail %s", name);
            /* open pipe to mail program for writing */
            if (!(mprog = popen(mbuf, "w")) )
                bailout("Can't use /bin/mail program", 6);
            fprintf(mprog, "Subject: Excess Idle Time\nLogged off - excess idle time - %s %s\ntty = %s\n",name, ctime(&tvec), dev+5);
            fclose(mprog);
            }
        }
    if (flag == NOLOGOFF) 
        {                       /* send mail to root if can't kill */
        if (log && ce->log)
            fprintf(log, "** LOGOFF-FAIL ** %s (pid = %d) %s (%d min idle time) %s",name, utmpp->ut_pid, dev+5, idle, ctime(&tvec)+3);
        if (ce->mail)
            {
            sprintf(mbuf, "/bin/mail root");
            if ((mprog = popen(mbuf, "w")) == (FILE *) NULL)
                bailout("Can't use /bin/mail program", 7);
            fprintf(mprog, "Subject: Can't logoff %s\nCan't Log off - %s %s\ntty = %s\n", name, name, ctime(&tvec), dev+5);
            fclose(mprog);
            }
        }
    fclose(log);
    return(0);
    }

killit(int pid)     /* terminate process using SIGHUP, then SIGKILL */
    {
    kill(pid, SIGHUP);          /* first send "hangup" signal */
    sleep(KWAIT);
    if (!kill(pid, 0)) 
        {                       /* SIGHUP might be ignored */
        kill(pid, SIGKILL);     /* then send sure "kill" signal */
        sleep(KWAIT);
        if (!kill(pid, 0))
            return(0);          /* failure--refuses to die! */
        else
            return(1);          /* successful kill with SIGKILL */
        }
    else
        return(1);              /* successful kill with SIGHUP */
    }

bailout(char *message, int status)  /* display error message and exit */
    {
    fprintf(stderr,"autologout: %s\n", message);
    exit(status);
    }
