/*
 * ps.c                - show process status
 *
 * Copyright (c) 1992 Branko Lankester
 *
 * Snarfed and HEAVILY modified for procps by Michael K. Johnson,
 * (johnsonm@sunsite.unc.edu).  What is used is what is required to have a
 *  common interface.
 *
 * Massive modifications by Charles Blake (cblake@bbn.com).  Rewrite
 * of the system process table code, multilevel sorting, device number
 * database, forest feature (contributed by ...), environment variables, GNU
 * style long options, pid list filtering (contributed by Michael Shields).
 *
 * massive changes to support SVR4/POSIX style ps options as well as
 * configurable styles, colour, generic display routines
 * by Helmut Geyer <Helmut.Geyer@iwr.uni-heidelberg.de>
 *
 * Changes Copyright (C) 1993, 1994 Michael K. Johnson,
 *   and   Copyright (C) 1995, 1996 Charles Blake,
 *   and   Copyright (C) 1996, 1997 Helmut Geyer
 * See file COPYING for copyright details.
 */
#include "proc/version.h"
#include "proc/readproc.h"
#include "proc/ps.h"
#include "proc/psdata.h"
#include "proc/devname.h"
#include "proc/tree.h"
#include "proc/sysinfo.h"
#include "proc/showtask.h"
#include "proc/output.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <time.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <getopt.h>
#include <errno.h>


#define BSD_PERSON 1
#define POSIX_PERSON 2

/* Linux used to have BSD style ps, so that's the default for now */
#ifndef DEFAULT_PERSONALITY
# define DEFAULT_PERSONALITY BSD_PERSON
#endif

void get_pcpu (proc_t*);

char Format[2048] = "";
int person=DEFAULT_PERSONALITY;

/* this struct replaces the previously parallel fmt_fnc and hdrs */
struct {
    const char*	default_sort;
    const char* format;
} mode[] = {
    { "Up", "pid,tty,state,time,comm" },
    { "Pp", "flags,uid,pid,ppid,pri,nice,vsz,rss,wchan,state,tty,time,comm" },
    { "up", "user,pid,pcpu,pmem,vsz,rss,tty,state,start_time,time,comm" },
    { "gPp", "ppid,pid,pgid,sid,tty,tpgid,state,uid,time,comm" },
    { "p", "uid,pid,sig_pend,sig_block,sig_ignore,sig_catch,state,tty,time,comm" },
    /* no more rss_lim, as the kernel defines it but doesn't honor it */
    { "r", "pid,tty,state,time,maj_flt,m_trs,m_drs,rss,pmem,comm" }, 
    { "r", "pid,tty,maj_flt,min_flt,m_trs,m_drs,m_size,m_swap,m_res,m_share,vm_lib,m_dt,comm" },
    { "p",  "pid,start_stack,esp,eip,timeout,alarm,state,tty,time,comm" },
    { "Up", "pid,tty,time,comm"},
  /* below fields C and ADDR are missing as they are not (yet) supported by
     the kernel */
    { "p",  "user,pid,ppid,start_time,tty,time,comm"},
    { "p", "flags,state,uid,pid,ppid,pri,nice,vsz,wchan,tty,time,comm" },
    { NULL, NULL }
};

/* some convenient integer constants corresponding to the above rows. */
enum PS_MODALITY {
    /* BSD Modes  */  PS_D = 0, PS_L, PS_U, PS_J, PS_S, PS_V, PS_M, PS_X,
    /* SYSV Modes */  PS_SD, PS_SF, PS_SL};

extern int   sort_depth;
extern int   sort_direction[];
extern int (*sort_function[])(/* void* a, void* b */);

int    parse_sort_opt (const char*);
int    parse_long_sort(const char*);
char * status         (proc_t*);
char * prtime         (char *s, unsigned long t, unsigned long rel);
void   usage          (char* context);
void   set_cmdspc     (int w_opts);
void   show_procs     (int do_header, int pflags, void*,int);
void   show_cmd_env   (struct tree_node tsk, unsigned maxch);
void   show_time      (char *s, proc_t * p);
void   add_node       (proc_t *task);
int    node_cmp       (const void *s1, const void *s2);
void   show_tree      (int n, int depth, char *continued);
void   show_forest    (void);

int CL_fmt       = 0;

/* process list filtering command line options */
dopts  disp_options;

int CL_all,
    CL_kern_comm =0,
    CL_no_glead,
    CL_no_slead,
    CL_run_only;
char  * CL_ctty;
pid_t * CL_pids= (pid_t *) NULL;/* zero-terminated list, dynamically allocated */
uid_t * CL_uids = (uid_t *)NULL, * CL_gids = (uid_t *)NULL;
int CL_npids=0,  CL_nuids=0,  CL_ngids=0, CL_ntty=0;
char * CL_stats;

/* process display modification command line options */

int CL_show_env,
    CL_num_outp,          /* numeric fields for user, wchan, tty */
    CL_pcpu     = 0,
    CL_sort     = 1,
    CL_deselect = 0,      /* deselect uids and gids given on command line */
    CL_forest,
    CL_Sum;

/* Globals */

unsigned cmdspc = 80;     /* space left for cmd+env after table per row */
int      GL_current_time; /* some global system parameters */
unsigned GL_main_mem;
long     GL_time_now;
int      GL_wchan_nout = 0; /* this is can also be set on the command-line  */

int      GL_width = 0,
         GL_pflags = 0,
         GL_no_color = 0,
         GL_cols = 0,
         CL_user_ord = 0;

int parse_pidlist (char* list, pid_t** pidlist, int length) {
    
    char *c, *tmp;
    int i;
    
    tmp=strdup(list);
    c = strchr(tmp, ' ');
    if (c != NULL) *c ='\0';
    i=strtol(strtok(tmp,","), (char **)NULL, 0);
    if (errno == ERANGE) {
        fprintf(stderr, "not a valid PID list: %s\n", list);
        exit(1);
    }
    length++;
    *pidlist=xrealloc(*pidlist, (length+1)*sizeof(pid_t));
    (*pidlist)[length-1] = (pid_t) i;
    (*pidlist)[length]   = (pid_t) 0;
    while ((c=strtok(NULL,","))!= NULL) {
        i=strtol(c, (char **)NULL, 0);
        if (errno == ERANGE) {
            fprintf(stderr, "not a valid PID list: %s\n", list);
            exit(1);
        }
        length++;
        *pidlist=xrealloc(*pidlist, (length+1)*sizeof(pid_t));
        (*pidlist)[length-1] = (pid_t) i;
        (*pidlist)[length]   = (pid_t) 0;
    }
    free (tmp);
    return (length);
}

int parse_grouplist (char* list, gid_t** gidlist, int length) {

    char *c, *tmp;
    gid_t i;
    struct group *grp_p;

    tmp=strdup(list);
    c = strchr(tmp, ' ');
    if (c != NULL) *c ='\0';
    c = strtok(tmp, ",");
    while (c != NULL) {
        if (isdigit(c[0])) {
            i=(gid_t)strtol(c, (char **)NULL, 0);
            if (errno == ERANGE) {
                fprintf(stderr, "not a valid GID: %s\n", c);
                exit(1);
            }
        } else {
            if((grp_p=getgrnam(c))==NULL) {
                fprintf(stderr, "not a valid group name: %s\n", c);
                exit(1);
            } else {
                i = grp_p->gr_gid;
            }
        }
        length++;
        *gidlist=xrealloc(*gidlist, (length+1)*sizeof(gid_t));
        (*gidlist)[length-1] = i;
        c = strtok (NULL, ",");
    }
    free(tmp);
    return (length);    
}

int parse_userlist (char* list, uid_t** uidlist, int length) {
    
    char *c, *tmp;
    int i;
    struct  passwd *user_pw;
    
    tmp=strdup(list);
    c = strchr(tmp, ' ');
    if (c != NULL) *c ='\0';
    c = strtok(tmp, ",");
    while (c != NULL) {
        if (isdigit(c[0])) {
            i=(uid_t)strtol(c, (char **)NULL, 0);
            if (errno == ERANGE) {
                fprintf(stderr, "not a valid UID: %s\n", c);
                exit(1);
            }
        } else {
            if((user_pw=getpwnam(c))==NULL) {
                fprintf(stderr, "not a valid user name: %s\n", c);
                exit(1);
            } else {
                i = user_pw->pw_uid;
            }
        }
        length++;
        *uidlist=xrealloc(*uidlist, (length+1)*sizeof(uid_t));
        (*uidlist)[length-1] = i;
        c = strtok (NULL, ",");
    }
    free (tmp);
    return (length);    
}

int parse_bsd_options (int argc, char **argv){
    char *p;
    int next_arg = 0,
        new_format = 0,
        do_header = 1,
	toppid = 0;
    

    do {
        --argc;		/* shift to next arg. */
        ++argv;
        for (p = *argv; p && *p; ++p) {
            switch (*p) {
            case '-':               /* "--" ==> long name options */
		if (*(p+1) == '-') {
                    if (strncmp(p+2,"sort",4)==0) {
                        if (*(p+6)!='\0'){
                            if (parse_long_sort(p+7) == -1)
                                usage("unrecognized long sort option\n");
                        } else {
                            argc--;argv++;
                            if (argc==0) usage ("missing argument to long sort option\n");
                            if (parse_long_sort(*argv) == -1)
                                usage("unrecognized long sort option\n");
                        }
			CL_user_ord = 1;
			next_arg = 1;
			break;
                    } else if (strncmp(p+2, "cols", 4) ==0) {
                        argc--;argv++;
                        GL_cols=atoi(*argv);
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "format", 6) == 0) {
                        argc--;argv++;
                        if(argc==0) usage("long option misses argument\n");
                        if (!new_format) {
                            new_format=1;
                            if (Format[0]!='\0') memset(Format, 0, 2048);
                        }
                        if (strlen(Format)+strlen(*argv)<2048) {
                            if (strlen(Format)>0) strcat (Format, ",");
                            strcat (Format, *argv);
                        } else {
                            fprintf(stderr, "Too long supplied format\n");
                            exit(1);
                        }
                        next_arg = 1;
                        break;
                    } else if (strncmp(p+3, "ser", 3) == 0) {
                        argv++;argc--;
                        if(argc==0) usage("long option misses argument\n");
                        if (*(p+2) == 'u') {
                            CL_nuids = parse_userlist(*argv, &CL_uids, CL_nuids);
                        } else if (*(p+2) == 'U') {
                             GL_pflags |= PROC_REAL;
                             CL_nuids = parse_userlist(*argv, &CL_uids, CL_nuids);
                        } else {
                            usage("ps: unknown long option\n");
                        }
                        next_arg=1;
                        break;
                    } else if (strncmp(p+3, "roup", 4) == 0) {
                        argv++;argc--;
                        if(argc==0) usage("long option misses argument\n");
                        if (*(p+2) == 'g') {
                            CL_ngids = parse_grouplist(*argv, &CL_gids, CL_ngids);
                        } else if (*(p+2) == 'G') {
                            GL_pflags |= PROC_REAL;
                            CL_ngids = parse_grouplist(*argv, &CL_gids, CL_ngids);
                        } else {
                            usage("ps: unknown long option\n");
                        }
                        next_arg=1;
                        break;
                    } else if (strncmp(p+3, "id", 2) == 0) {
                        argv++;argc--;
                        if(argc==0) usage("long option misses argument\n");
                        if (*(p+2) == 'p') {
                            CL_npids = parse_pidlist(*argv, &CL_pids, CL_npids);
                        } else if (*(p+2) == 's') {
                            CL_npids = parse_pidlist(*argv, &CL_pids, CL_npids);
                        } else {
                            usage("ps: unknown long option\n");
                        }
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "tty", 3) == 0) {
                        argv++;argc--;
                        if(argc==0) usage("long option misses argument\n");
                        CL_ctty = *argv;
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "color", 5) == 0) {
                        GL_no_color=0;
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "nocolor", 7) == 0) {
                        GL_no_color=1;
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "cumulative", 10) == 0) {
                        CL_Sum=1;
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "deselect", 8) == 0) {
                        CL_deselect=1;
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "forest", 6) == 0) {
                        CL_forest=1;
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "alttime", 7) == 0){
                        disp_options.flags ^= DISP_NEWTIMFORM;
                        next_arg=1;
                        break;
                    } else if (strncmp(p+2, "help", 4) == 0) {
			usage(NULL);
			dump_keys();
			exit(0);
                    } else if (strncmp(p+2, "version", 6) == 0) {
			display_version();
			exit(0);
                    } else if (*(p+2) != '\0')	/* just '-- '; not an error */
			usage("ps: unknown long option\n");
		}
		break;
            case 'l': CL_fmt = PS_L;   /* replaceable by a */	break;
            case 'u': CL_fmt = PS_U;   /* loop over mode[] */	break;
            case 'j': CL_fmt = PS_J;				break;
            case 's': CL_fmt = PS_S;				break;
            case 'v': CL_fmt = PS_V;				break;
            case 'm': CL_fmt = PS_M;				break;

            case 'N': CL_deselect = 1;                          break;
            case 'X': CL_fmt = PS_X;   /* regs */		break;
                
            case 'r': CL_run_only = 1; /* list filters */	break;
            case 'g':   /* SunOS  session leaders ignored*/     break;
                
            case 'a': CL_all = 1;GL_pflags &= ~PROC_UID; break;
            case 'x': GL_pflags &= ~PROC_ANYTTY;		break;
            case 't': CL_ctty = p + 1;
		next_arg = 1;   				break;
                
            case 'e': CL_show_env = 1; /* output modifiers */	break;
            case 'f': CL_forest = 1;
                disp_options.flags |= DISP_FOREST;
		CL_kern_comm = 0;			        break;
            case 'c': CL_kern_comm = 1;			        break;
            case 'w': ++GL_width;				break;
            case 'h': do_header = 0;				break;
            case 'n': CL_num_outp = 1;
		GL_wchan_nout = 1;                              break;
            case 'S': CL_Sum = 1;
                disp_options.flags |= DISP_CUMULATIVE;          break;
            case 'p': disp_options.pg_shift = 0;                break;
            case 'o': CL_sort = !CL_sort;			break;
            case 'O':
		if (parse_sort_opt(p+1) == -1)
		    usage("short form sort flag parse error\n");
		CL_user_ord = 1;
		next_arg = 1;
		break;
            case 'V': display_version(); exit(0);
            default:
                /* Step through, reading+alloc space for comma-delim pids */
		if (isdigit(*p)) {
		    while (isdigit(*p)) {
			CL_pids = xrealloc(CL_pids, (toppid + 2)*sizeof(pid_t));
			CL_pids[toppid++] = atoi(p);
                        CL_npids++;
			while (isdigit(*p))
			    p++;
			if (*p == ',')
			    p++;
		    }
		    CL_pids[toppid] = 0;
		    next_arg = 1;
		}
		if (*p)
		    usage("unrecognized option or trailing garbage\n");
            }
            if (next_arg) {
                next_arg = 0;
                break;       /* end loop over chars in this argument */
            }
        }
    } while (argc > 1);
    if (CL_fmt != 0) strcpy (Format,mode[CL_fmt].format);
    return do_header;
}

/* 
 * SYSV/POSIX options do not allow omission of leading -, so we can use
 * getopt for command line options.
 */
void parse_sysv_options (int argc, char **argv){
    char *p, c;
    int option_index = 0,
        add_jobinfo = 0,
        new_format = 0;
    
    static struct option long_options[]={
        { "version", 0, 0, 'V' },
        { "help", 0, 0, 0 },
        { "sort",1, 0, 0 },
        { "cols", 1, 0, 0 },
        { "width", 1, 0, 0 },
        { "alttime", 0, 0, 0 },
        { "format", 1, 0, 'o' },
        { "user", 1, 0, 'u' },
        { "User", 1, 0, 'U' },
        { "groups", 1, 0, 'g' },
        { "Groups", 1, 0, 'G' },
        { "sid", 1, 0, 's' },
        { "tty", 1, 0, 't' },
        { "pid", 1, 0, 'p' },
        { "deselect", 0, 0, 'N'},
        { "forest", 0, &CL_forest, 1 },
        { "cumulative", 0, &CL_Sum, 1},
        { "color", 0, &GL_no_color, 0},
        { "nocolor", 0, &GL_no_color, 1},
        { NULL, 0, 0, 0 } };

    while ((c = getopt_long (argc, argv, "aAcdefg:G:jlNo:p:rs:t:u:U:Vw", long_options, &option_index))!= -1) {
        switch (c) {
        case 0:      /* long option in action */
            if (long_options[option_index].flag != 0) break;
            switch (option_index){
            case 1:
                usage(NULL);
                dump_keys();
                 exit(0);
                break;
            case 2:
                if (parse_long_sort(optarg) == -1)
                    usage("unrecognized long sort option\n");
                CL_user_ord = 1;
                break;
            case 3:
                GL_cols=atoi(optarg);
                break;
            case 4:
                GL_width=atoi(optarg);
                break;
            case 5:
                disp_options.flags ^= DISP_NEWTIMFORM;
                break;
            default:
                usage("Unknown long option\n");
                exit(1);
                break;
            }
            break;
            
        case 'a': GL_pflags = (GL_pflags|PROC_NOGLEAD|PROC_ANYTTY) 
                      & ~PROC_UID;                                  break;
   /* -a really isn't much use, if used as defined in the SOLARIS manpage.*/
        case 'c': /* NOP Option for now */                          break;
        case 'd': GL_pflags = (GL_pflags|PROC_ANYTTY|PROC_NOSLEAD) 
                      & ~PROC_UID;                                  break;
        case 'e': /* e and A are the same */
        case 'A': CL_all =1; GL_pflags &= ~(PROC_ANYTTY|PROC_UID);  break;
        case 'f': 
            new_format = 1;
            strcpy(Format, "uid,pid,ppid,start_time,tty,time,args");
            break;
        case 'G': GL_pflags |= PROC_REAL; /* rest as in 'g' */
        case 'g': CL_ngids = parse_grouplist (optarg, &CL_gids, CL_ngids);
            break;
        case 'j': add_jobinfo = 1;                                  break;
        case 'l':
            new_format = 1;
            strcpy(Format, "flags,state,uid,pid,ppid,pri,nice,wchan,tty,time,comm");
            break;
        case 'N': CL_deselect = 1;                                  break;
        case 'o':
            if (!new_format) {
                new_format=1;
                if (Format[0]!='\0') memset(Format, 0, 2048);
            }
            if (strlen(Format)+strlen(optarg)<2048) {
                if (strlen(Format)>0) strcat (Format, ",");
                strcat (Format, optarg);
            } else {
                fprintf(stderr, "Too long supplied format\n");
                exit(1);
            }
            break;
        case 'p':
        case 's': CL_npids = parse_pidlist(optarg, &CL_pids, CL_npids); break;
        case 'r': GL_pflags |= PROC_REAL;                              break;
        case 't': CL_ctty = optarg;                                    break;
        case 'U': GL_pflags |= PROC_REAL;
        case 'u': CL_nuids = parse_userlist(optarg, &CL_uids, CL_nuids);break;
        case 'V':
                display_version();
                exit(0);
                break;
        case 'w': GL_width++;                                           break;
        case '?':
            break;
        default:
            fprintf (stderr," getopt error -- this should never happen\n");
            exit (1);
        }
    }
    if (add_jobinfo) {
        p = strstr(Format, "comm");
        if (p!=NULL) {
            if (strlen(Format)< 2039) {
                *p='\0';
                strcat (Format, "pgid,sid,comm");
            } else {
                fprintf(stderr, "Too long supplied format\n");
                exit(1);
            }
        } else if ((p=strstr(Format, "args"))!= NULL) {
            if (strlen(Format)< 2039) {
                *p='\0';
                strcat (Format, "pgid,sid,args");
            } else {
                fprintf(stderr, "Too long supplied format\n");
                exit(1);
            }
        } else {
            if (strlen(Format)< 2039) {
                strcat (Format, ",pid,sid");
            } else {
                fprintf(stderr, "Too long supplied format\n");
                exit(1);
            }
        }
    }
}
    
int main(int argc, char **argv) {
    char *p;
    int i,
        do_header = 1,
	psdbsucc = 0,
        N = 1;
    void* args = NULL;
    dev_t tty[2] = { 0 };
    uid_t uid[1];

    disp_options.pg_shift=(PAGE_SHIFT - 10); /* default: show k instead of pages */
#ifdef NEW_TIME_DEFAULT
    disp_options.flags = DISP_NEWTIMFORM | DISP_COLOR;
#else
    disp_options.flags = DISP_COLOR;
#endif
    GL_pflags = PROC_ANYTTY|PROC_UID;
    args = uid;
    uid[0]=getuid();
    
    set_linux_version();
                 /* set up environment variables */
    p=getenv("PS_COLORS");
    setup_colors(&disp_options,p);
    p=getenv("PS_PERSONALITY");
    if(p){
        if(!strcasecmp("bsd", p)) person=BSD_PERSON;
        else if(!strcasecmp("posix", p)) person=POSIX_PERSON;
    }
    p=getenv ("PS_FORMAT");
    if (p) {
        strcpy (Format, p);
    } else {
        strcpy (Format, mode[0].format);
    }

    if (person == BSD_PERSON) {
        do_header = parse_bsd_options(argc, argv);
    } else {
        if (argc == 1) {
            CL_ctty = strrchr(ttyname(0),'/');
            CL_ctty++;
            GL_pflags=PROC_TTY;
        }
        CL_kern_comm = 1;
        parse_sysv_options(argc,argv);
    }
    
    disp_options.Fieldstr = Format;
    
    if (!CL_sort)	/* since the unsorted mode is intended to be speedy */
	CL_forest = 0;	/* turn off the expensive forest option as well. */
    
    if (GL_no_color) disp_options.flags &= ~DISP_COLOR;
    
    if ( CL_Sum ) disp_options.flags |= DISP_CUMULATIVE;

    set_cmdspc(GL_width);
/* 
 * clean up Format string for use of forest option. We implicitly assume, 
 * that multiple mentionings of comm or args fields really are intended.
 */
    if (CL_forest) { 
        p = strstr (Format, ",args");
        if (p!=NULL) {
            GL_pflags = PROC_FILLCMD;
        } else {
            p=strstr (Format, ",comm");
        }
        if (p!=NULL) {
            *p='\0';
            p = strchr(&(p[1]), ',');
            if (p!=NULL) {
                char c_tmp[2048];
                strcpy (c_tmp, p);
                strcat (Format, c_tmp);  
            }    
        }
        
    }
    
    GL_pflags |= make_header (&disp_options);

    for (i=0;i< disp_options.Numfields;i++)
        if (disp_options.p[i] == P_PCPU) {
            CL_pcpu = 1;
            break;
        } else if (disp_options.p[i] == P_WCHAN) {
            if (GL_wchan_nout || open_psdb())
                GL_wchan_nout = 1;
            else {
                disp_options.flags |= DISP_WCHAN;
                psdbsucc = 1;
            }
            break;
        }
    
    if (!(GL_main_mem = read_total_main()) ||
	!(GL_current_time = uptime(0,0)))
	return 1;
    GL_time_now = time(0L);

    if (CL_sort && !CL_user_ord)
        parse_sort_opt(mode[CL_fmt].default_sort);

    /* NOTE:  all but option parsing has really been done to enable
     * multiple uid/tty/state filtering as well as multiple pid filtering
     * We have TTY > PID > {NOT,}[GU]ID > STAT
     */

    if (!CL_kern_comm && GL_pflags & PROC_FILLSTAT) {  	 /* verbosity flags */
	GL_pflags |= PROC_FILLCMD;
        disp_options.flags |= DISP_CMD;
    }
    if (CL_show_env)  GL_pflags |= PROC_FILLENV;

    if (CL_run_only){ PROC_SET_MASK(GL_pflags, STAT);   args = "RD"; }
    if (CL_stats)   { PROC_SET_MASK(GL_pflags, STAT);   args = CL_stats; }
    if (CL_deselect) {
        if (CL_nuids)   
            { PROC_SET_MASK(GL_pflags, NOTUID); args = CL_uids; N = CL_nuids; }
        if (CL_ngids)  
            { PROC_SET_MASK(GL_pflags, NOTGID); args = CL_gids; N = CL_ngids; }
        if (CL_npids) 
            { PROC_SET_MASK(GL_pflags, NOTPID); args = CL_pids; N = CL_npids; }
    } else {
        if (CL_nuids)
            { PROC_SET_MASK(GL_pflags, UID); args = CL_uids; N = CL_nuids; }
        if (CL_ngids)
            { PROC_SET_MASK(GL_pflags, GID); args = CL_gids; N = CL_ngids; }
        if (CL_npids) 
            { PROC_SET_MASK(GL_pflags, PID); args = CL_pids; N = CL_npids; }
    }
    
    if (CL_ctty) {
	if ((tty[0] = tty_to_dev(CL_ctty)) == (dev_t)-1) {
	    fprintf(stderr, "the name `%s' is not a tty\n", CL_ctty);
	    exit(1);
	}
        PROC_SET_MASK(GL_pflags, TTY);
        args = tty;
    }
    show_procs(do_header, GL_pflags, args, N);
    if (psdbsucc)
	close_psdb();
    return 0;
}

/* print a context dependent usage message and maybe exit
 */
void usage(char* context) {
    if (person==BSD_PERSON) {
        fprintf(stderr,
                "%s"
                "usage:  ps -acehjlnrsSuvwx{t<tty>|#|O[-]u[-]U..} \\\n",
                context ? context : "");
    } else {
        fprintf(stderr,
                "%s"
                "usage:  ps -aAcdefjlNV -[gGopstuU] arg\\\n",
                context ? context : "");
    }
    fprintf(stderr,
            "           --color       turn on colorisation\n"
            "           --cumulative  show cumulative times\n"
            "           --alttime     use alternative time format\n"
            "           --deselect    deselects users, groups or processes\n"
            "                         using --gruop,  --user and --pid\n"
            "           --forest      show process trees\n"
            "           --format field1,field2,..\n"
            "           --group {group1|gid1},...\n"
            "           --Group as --group, but use real gid for matching\n"
            "           --help        gives you this message\n"
            "           --nocolor     turn off colorisation\n"
            "           --pid pid1,pid2,...\n"
            "           --sid sid1,sid2,...\n"
            "           --sort [-]key1,[-]key2,...\n"
            "           --tty tty\n"
            "           --user {user1|uid1},{user2|uid2},..\n"
            "           --User as --user, but use real uid for matching\n"
            "           --version     prints version information\n");
    
    if (context)
        exit(1);	/* prevent bad exit status by calling usage("") */
}

/* set maximum chars displayed on a line based on screen size.
 * Always allow for the header, with n+1 lines of output per row.
 */
void set_cmdspc(int n) {
    struct winsize win;

    if (ioctl(1, TIOCGWINSZ, &win) != -1 && win.ws_col > 0)
	disp_options.Cols = win.ws_col;
    else 
        disp_options.Cols = 80; /* default to 80 chars per line */
    if (n > 100) n = 100;	/* max of 100 'w' options */
    if (GL_cols) {
        disp_options.Cols=GL_cols;
    } 
    cmdspc=(n+1)*disp_options.Cols;
    disp_options.Lines=n;
}

/* This is the main driver routine that iterates over the process table.
 */
void show_procs( int do_header, int pflags, void* args, int N) {
    static proc_t buf; /* less dynamic memory allocation when not sorting */
    PROCTAB* tab;
    proc_t **ptable = NULL, *next, *retbuf = NULL;
    static char s[4096];
    int n = 0;

    /* initiate process table scan */
    tab = openproc(pflags, args, N);

    if (do_header) puts(Header);	/* print header */

    if (!(CL_sort || CL_forest))	/* when sorting and forest are both */
	retbuf = &buf;			/* off we can use a static buffer */

    while ((next = readproc(tab,retbuf))) {	/* read next process */
        if (CL_pcpu) get_pcpu(next);
	n++;					/* Now either: */
	if (CL_forest) {			/*    add process to tree */
	    add_node(next);
	} else if (CL_sort) {			/*    add process to table */
	    ptable = realloc(ptable, n*sizeof(proc_t*));
	    ptable[n-1] = next;
	} else {				/*    or show it right away */
            show_task_info(&buf, GL_main_mem, GL_current_time, &disp_options);
            printf("%s\n",s);
	    if (buf.cmdline) free((void*)(buf.cmdline[0]));
	    if (buf.environ) free((void*)(buf.environ[0]));
	}
    }
    if (!n) {
	fprintf(stderr, "No processes available.\n");
	exit(1);
    }
    if (CL_sort && !CL_forest) {	/* just print sorted table */
	int i;
	qsort(ptable, n, sizeof(proc_t*), (void*)mult_lvl_cmp);
	for (i = 0; i < n; i++) {
	    show_task_info(ptable[i], GL_main_mem, GL_current_time, &disp_options);
            printf("%s\n",s);
	    freeproc(ptable[i]);
	}
	free(ptable);
    } else if (CL_forest)
	show_forest();
}

void get_pcpu(proc_t *p) {
    int total_time, seconds;
    time_t start;
    unsigned int pcpu;

    seconds = (((GL_current_time * HZ) - p->start_time) / HZ);
    start = GL_time_now - seconds;
    total_time = (p->utime + p->stime +
		  (CL_Sum ? p->cutime + p->cstime : 0));
    pcpu = seconds ?
	(total_time * 10 * 100/HZ) / seconds :
	0;
    if (pcpu > 999) pcpu = 999;
    p->pcpu = pcpu;
}


/* fancy process family tree based cmdline printing.  Building the tree
   should be relegated to libproc and only the printing logic should
   remain here.
*/
struct tree_node * node;  /* forest mode globals */
int      nodes = 0;
int      maxnodes = 0;

void add_node(proc_t *task) {
    if (maxnodes == 0) {
	maxnodes = 64;
        node = (struct tree_node *)
            malloc(sizeof(struct tree_node) * maxnodes);
    }
    if (nodes > maxnodes-1) {
	maxnodes *= 2;
        node = (struct tree_node *)
            realloc(node, sizeof(struct tree_node) * maxnodes);
    }
    node[nodes].proc        = task;
    node[nodes].pid         = task->pid;
    node[nodes].ppid        = task->ppid;
    node[nodes].cmd         = task->cmd;
    node[nodes].cmdline     = task->cmdline;
    node[nodes].environ     = task->environ;
    node[nodes].children    = 0;
    node[nodes].have_parent = 0;
    nodes++;
}

/* show the trailing command and environment in available space.
 * use abbreviated cmd if requested, NULL list, or singleton NULL string
 */
void show_cmd_env(struct tree_node tsk, unsigned maxch) {

    if (CL_kern_comm) {      /* no () when explicit request for tsk cmd */
        maxch -= print_str(tsk.cmd, maxch);
    } else if (!tsk.cmdline || !*tsk.cmdline || (!tsk.cmdline[1] && !*tsk.cmdline)) {
        /* no /proc//cmdline ==> bounding () */
        if (maxch) {
            fputc('(',stdout);
            maxch--;
        }
        maxch -= print_str(tsk.cmd, maxch);
        if (maxch) {
            fputc(')',stdout);
            maxch--;
        }
    } else {
        maxch -= fprint_strlist(stdout, tsk.cmdline, maxch," ");
    }
    if (CL_show_env && tsk.environ) {
        print_strlist(tsk.environ, maxch, " ");
    }
}

int node_cmp(const void *s1, const void *s2) {
    struct tree_node *n1 = (struct tree_node *) s1;
    struct tree_node *n2 = (struct tree_node *) s2;
    return n1->pid - n2->pid;
}

void show_tree(int n, int depth, char *continued) {
    int i, cols;
#ifdef PROC_COLOR
    char *mark=NULL;
    static char tmp[1024];
#endif

    cols=show_task_info(node[n].proc, GL_main_mem, GL_current_time, &disp_options);

#ifdef PROC_COLOR

    if ((disp_options.flags & DISP_COLOR) && (mark=markup(node[n].proc, 0))!= NULL ) {
        sprintf(tmp,LEFT_MARKUP);
	strcat(tmp, mark);
	strcat(tmp, RIGHT_MARKUP);
        printf("%s",tmp);
    } 
#endif
    for (i = 0; i < depth; i++) {
        if (cols + 4 >= cmdspc - 1)
            break; 
        if (i == depth - 1)
            printf(" \\_ ");
        else if (continued[i])
            printf(" |  ");
        else
            printf("    ");
        cols += 4;
    }
    show_cmd_env(node[n], cmdspc - cols);
#ifdef PROC_COLOR
    if ((disp_options.flags & DISP_COLOR) && mark!=NULL ) { 
        printf("%s",END_MARKUP);
    } 
#endif
    fputc('\n', stdout);

    for (i = 0; i < node[n].children; i++) {
        continued[depth] = i != node[n].children - 1;
        show_tree(node[n].child[i], depth + 1, continued);
    }
}

void show_forest() {
    register int i, j;
    int parent;
    char continued[1024];

    if (CL_sort)
	qsort((void*)node, nodes, sizeof(struct tree_node), (void*)node_mult_lvl_cmp);

    for (i = 0; i < nodes; i++) {
        if (node[i].ppid > 1 && node[i].pid != node[i].ppid) {
	    parent = -1;
	    for (j=0; j<nodes; j++)
		if (node[j].pid==node[i].ppid)
		    parent = j;
        } else
            parent = -1;
        if (parent >= 0) {
            node[i].have_parent++;
            if (node[parent].children == 0) {
                node[parent].child = (int*)malloc(16 * sizeof(int*));
                node[parent].maxchildren = 16;
            }
            else if (node[parent].children == node[parent].maxchildren) {
                node[parent].maxchildren *= 2;
                node[parent].child = (int*)realloc(node[parent].child,
						   node[parent].maxchildren
						   * sizeof(int*));
            }
            node[parent].child[node[parent].children++] = i;
        }
    }

    for (i = 0; i < nodes; i++) {
        if (!node[i].have_parent)
            show_tree(i, 0, continued);
    }
}
