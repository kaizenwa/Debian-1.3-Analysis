/*
 * showtask.c           An output routine for a single task to fp
 * 
 * Copyright (c) 1996 Helmut Geyer
 * 
 * completely rewritten, based on the top show_task-info routine from
 * the top program by Roger Binns, which is
 * Copyright (c) 1992 Branko Lankester
 * Copyright (c) 1992 Roger Binns
 *
 * Many other people contributed to the top programs (take a look at
 * top.c for more concise information about that). 
 *
 * This module implements these functions for external use:
 * 
 * void make_header (char *Fields , dopts *opts);
 * void setup_colors (dopts *opts, char *colorenv);
 * int show_task_info(char *s, proc_t *task, int pmem, dopts *opts); 
 *  
 * if it is compiled with color support (PROC_COLOR), it implements as
 * well: 
 * 
 * char * markup (proc_t * task, int pmem);
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <termcap.h>
#include <time.h>
#include <sys/resource.h>

#include "proc/ps.h"
#include "proc/readproc.h"
#include "proc/showtask.h"
#include "proc/output.h"

extern char *status (proc_t * task);
/*
 * Corresponding headers. Note that in cumulative mode the leading space
 * of some headers will be replaced by a 'C'. field length = -1 means that
 * it will be as large as possible.
 * Actual display may depend on other flags, e.g cumulative mode, 
 * command name/line switch or symbolic/numeric display of WCHAN.
 * third field is the sprintf format, the fourth the flags needed for
 * readproc to obtain this information.
 */

/* this is just a dummy, for values in both stat and status */
#define PROC_FILLBOTH 0x0   

header_t headers[MAXFIELDS] =
{    /* POSIX Headers */
    {"USER", 8, "%-*.*s ", PROC_FILLUSR|PROC_FILLBOTH }, 
    {"RUSER", 8, "%-*.*s ", PROC_FILLUSR|PROC_FILLSTATUS },
    {"GROUP", 8, "%-*.*s ", PROC_FILLUSR|PROC_FILLBOTH },
    {"RGROUP", 8, "%-*.*s ", PROC_FILLUSR|PROC_FILLSTATUS },
    {"PID", 5, "%*d ", PROC_FILLBOTH}, 
    {"PPID", 5, "%*d ", PROC_FILLBOTH}, 
    {"PGID", 5, "%*d ", PROC_FILLSTAT},
    {"%CPU", 4, "%2d.%1d ", PROC_FILLSTAT},
    {"VSZ", 5, "%*d ", PROC_FILLSTAT}, 
    {"NI", 3, "%*d ", PROC_FILLSTAT},
    {"ELAPSED", 7, "%*.*s ", PROC_FILLSTAT},
    {" TIME", 6, "%*.*s ", PROC_FILLSTAT},
    {"TT", 3, "%*.*s ", PROC_FILLSTAT|PROC_FILLTTY},
    {"COMMAND", -1, "\0\0", PROC_FILLBOTH }, 
    {"COMMAND", -1, "\0\0", PROC_FILLCMD|PROC_FILLSTAT},
    /* Linux headers */
    {"STAT", 4, "%-*.*s ", PROC_FILLBOTH}, 
    {"UID", 5, "%*d ", PROC_FILLBOTH},
    {"RUID", 5, "%*d ", PROC_FILLSTATUS},
    {"SUID", 5, "%*d ", PROC_FILLSTATUS},
    {"FSUID", 5, "%*d ", PROC_FILLSTATUS}, 
    {"GID", 5, "%*d ", PROC_FILLBOTH},
    {"RGID", 5, "%*d ", PROC_FILLSTATUS},
    {"SGID", 5, "%*d ", PROC_FILLSTATUS},
    {"FSGID", 5, "%*d ", PROC_FILLSTATUS},
    {"SID", 5, "%*d ", PROC_FILLSTAT},
    {" UTIME", 6, "%*.*s ", PROC_FILLSTAT},
    {" STIME", 6, "%*.*s ", PROC_FILLSTAT},
    {"PRI", 3, "%3d ", PROC_FILLSTAT}, 
    {"START", 6, "%*.*s ", PROC_FILLSTAT},
    {"FLAGS ", 8, "%*d ", PROC_FILLSTAT}, 
    {" MINFL", 6, "%*d ", PROC_FILLSTAT},
    {" MAJFL", 6, "%*d ", PROC_FILLSTAT},
    {"TMOUT", 5, "%*.*s ", PROC_FILLSTAT},
    {"ALARM", 5, "%*.*s ", PROC_FILLSTAT},
    {"RSS", 5, "%*d ", PROC_FILLSTAT}, 
    {"S_CODE", 8, "%*.*x ", PROC_FILLSTAT},
    {"E_CODE", 8, "%*.*x ", PROC_FILLSTAT},
    {"STACKP", 8, "%*.*x ", PROC_FILLSTAT},
    {"ESP", 8, "%*.*x ", PROC_FILLSTAT},
    {"EIP", 8, "%*.*x ", PROC_FILLSTAT},
    {"WCHAN", 10, "\0\0", PROC_FILLSTAT},
    {"SIGNAL", 8, "%*.*x ", PROC_FILLBOTH},
    {"BLOCKED", 8, "%*.*x ", PROC_FILLBOTH},
    {"IGNORED", 8, "%*.*x ", PROC_FILLBOTH},
    {"CATCHED",8, "%*.*x", PROC_FILLBOTH},
    {"%MEM", 4, "%2d.%1d ", PROC_FILLSTAT|PROC_FILLMEM}, 
    {"SUSER", 8, "%-*.*s ", PROC_FILLSTATUS|PROC_FILLUSR}, 
    {"FSUSER", 8, "%-*.*s ", PROC_FILLSTATUS|PROC_FILLUSR},
    {"SGROUP", 8, "%-*.*s ", PROC_FILLSTATUS|PROC_FILLUSR},
    {"FSGROUP", 8, "%-*.*s ", PROC_FILLSTATUS|PROC_FILLUSR},
    {"ENVIRONMENT", -1, "\0\0", PROC_FILLSTAT|PROC_FILLENV},
    {"LCK", 3, "%*d ", PROC_FILLSTATUS}, 
    {"DATA", 5, "%*d ", PROC_FILLSTATUS},
    {"STACK", 5,"%*d ", PROC_FILLSTATUS},
    {"EXE" , 5, "%*d ", PROC_FILLSTATUS},
    {"LIB" , 5, "%*d ", PROC_FILLSTATUS},
    {"SIZE", 5, "%*d ", PROC_FILLMEM},
    {"RES" , 5, "%*d ", PROC_FILLMEM},
    {"SHRD", 5, "%*d ", PROC_FILLMEM},
    {"TRS" , 5, "%*d ", PROC_FILLMEM},
    {"DRS" , 5, "%*d ", PROC_FILLMEM},
    {"DT"  , 4, "%*d ", PROC_FILLMEM},
    {"LRS" , 5, "%*d ", PROC_FILLMEM},
    {"SWAP", 5, "%*d ", PROC_FILLMEM},
    {"TPGID", 5, "%*d", PROC_FILLSTAT}
};
    
/* 
 * corresponding tags
 */
char tags[MAXFIELDS][16] = {
    /* POSIX Tags */
    "user", "ruser", "group", "rgroup", "pid", "ppid", "pgid", "pcpu",
    "vsz", "nice", "etime", "time", "tty", "comm", "args", 
    /* Linux specific tags */
    "state", "uid", "ruid", "suid", "fsuid", "gid", "rgid", "sgid", 
    "fsgid", "sid", "utime", "stime", "pri", "start_time", 
    "flags", "min_flt", "maj_flt", "timeout", "alarm", "rss", 
    "start_code", "end_code", "start_stack", "esp", "eip", 
    "wchan", "sig_pend", "sig_block", "sig_ignore", "sig_catch",
    "pmem", "suser", "fsuser", "sgroup", "fsgroup", "environ",
    /* vm fields from status not covered above */
    "vm_lock", "vm_data", "vm_stack", "vm_exe", "vm_lib",
    /* statm fields */
    "m_size", "m_resident", "m_share", "m_trs", "m_drs", "m_dt", "m_lrs",
    "m_swap", "tpgid"
};
/* corresponding field desciptions (only needed by top) */
char *headers2[] =
{
    "Process Id", "Parent Process Id", "User Id",
    "User Name", "CPU Usage", "Memory Usage",
    "Controlling tty", "Priority", "Nice Value",
    "Page Fault Count", "Code Size (kb)", "Data+Stack Size (kb)",
    "Virtual Size (kb)", "Res. Text Size (kb)", "Swapped kb",
    "Shared Pages (kb)", "Accessed Page count", "Write Protected Pages",
    "Dirty Pages", "Resident Set Size (kb)", "Sleeping in Function",
    "Process Status", "CPU Time", "Command",
    "Shared Lib. Pages (kb)",
    "Task Flags(cf sched.h)"
};

	/* The header printed at the top of the process list.*/
char Header[MAXLINES];

#define TEST(x) (opts->flags & DISP_ ## x)
#define SET(x)  (opts->flags |= DISP_ ## x)
#define CLEAR(x)  (opts->flags &= ~DISP_ ## x)
#define TOGGLE(x) (opts->flags ^= DISP_ ## x)

#define PROGNAME "showtask"

/*
 * set the field length of field number k to n. 
 */
int resize_field ( int k, dopts *opts, int n)
{
    char tmp2[2048];
    char tmp1[2048];
    int i;
    char *p;

    p=strchr(opts->Fieldstr, ',');
    if (k == 0) {
        tmp2[0]='\0';
    } else {
        for (i=1; i<k;i++){
            p=strchr(++p,',');
        }
        if (p==NULL) return 1;
        *p = '\0';
        strcpy(tmp2,opts->Fieldstr);
        strcat(tmp2,",");
        p=strchr(++p,',');
    }
    sprintf(tmp1,"%s=%d",tags[opts->p[k]],n);
    strcat(tmp2, tmp1);
    if (p!=NULL) strcat(tmp2,p);
    strcpy (opts->Fieldstr,tmp2);
    make_header (opts);
    return 0;
}
                   
/*
 * This subroutine parses the string FIELDS, which is of the form
 * "tag1[=n1],tag2[=n2], ... ,tagm[=nm]"
 * each tag is one out of tags[], the ni override the default length
 */
int parsetags( dopts *opts)
{
    char parsefield[2048];
    int j=0,len,i;
    char *c,*e;
    
    strcpy (parsefield, opts->Fieldstr);
    c=strtok(parsefield,",");
    while (c != NULL) {
        len = 0;
        if ((e=strchr(c,'='))!=NULL){
            if(sscanf(e, "=%d", &len)!=1) exit(1);
            *e='\0';
        }
        for(i=0; i< MAXFIELDS; i++){
            if (strcasecmp(c, tags[i])==0){
                opts->p[j++]=i;
                if (len!=0) {
          /* All non-variable fields shouldn't be longer than 9 spaces */ 
                    if (len > MAX_FIELD_LEN &&  headers[i].ln != -1)
                        len = MAX_FIELD_LEN;
                    headers[i].ln=len;
                }
                break;
            }
        }
        c=strtok(NULL,",");
    }
    
    return j;
}

int make_header (dopts *opts)
{
    int j, i, n, fixlen=0, var=0;
    int flags=0;
    char tmp[MAXLINES],format[256];

    if (opts->Cols == 0) {
        opts->Cols=MAXLINES-3;
        opts->Lines=0;
    }
    if (opts->Cols*(opts->Lines+1) >= MAXLINES)
        opts->Lines = (MAXLINES-1)/opts->Cols-1;
    j = parsetags (opts);    
    if (TEST(TABLEFORCE)) force_table_size =1;
    Header[0]='\0';
    if (TEST(CUMULATIVE)) {
        headers[11].fld[0]='C';
        headers[25].fld[0]='C';
        headers[26].fld[0]='C';
        headers[30].fld[0]='C';
        headers[31].fld[0]='C';
    } else {
        headers[11].fld[0]=' ';
        headers[25].fld[0]=' ';
        headers[26].fld[0]=' ';
        headers[30].fld[0]=' ';
        headers[31].fld[0]=' ';        
    }
    for (i = 0; i < j; i++){
        n = headers[opts->p[i]].ln;
        if (n < 0) 
            var++;
        else
            fixlen +=n+1;
    }
    i=(opts->Lines+1)*opts->Cols - fixlen;
    i--;
    if (i<0) i=0;
    if (var == 0) var = 1;
    fixlen = i/var;  /* distribute stretch equally on variable fields */
    opts->stretch=fixlen;
    for (i = 0; i < j; i++){
        n = headers[opts->p[i]].ln;
        flags |= headers[opts->p[i]].proc_flags;
        if (n==-1) {
            n=fixlen;
            sprintf(format,"%%-%d.%ds ",n,n);
        } else {
            sprintf(format,"%%%d.%ds ",n,n);
        }
        sprintf(tmp, format, headers[opts->p[i]].fld);
        strncat (Header, tmp, MAXLINES-strlen(Header)-1);
        switch(headers[opts->p[i]].format[1]){
        case '*':
            headers[opts->p[i]].format[1]=n+'0';
            if (headers[opts->p[i]].format[2] == '.') 
                headers[opts->p[i]].format[3]=n+'0';
            break;
        case '-':
            headers[opts->p[i]].format[2]=n+'0';
            headers[opts->p[i]].format[4]=n+'0';
            break;
        case '\0':
            sprintf(headers[opts->p[i]].format, "%%-%d.%ds ", n,n);
            break;
        }
    }
    if((strlen(Header)+2) > opts->Cols) 
        Header[opts->Cols - 1 ] = '\0';    
    opts->Numfields=j;
    j=strlen(Header)-1;
    while (Header[j] == ' ') j--;
    Header[j+1]='\0';
    if (!(flags & PROC_FILLSTAT) && !(flags & PROC_FILLSTATUS))
        flags |= PROC_FILLSTATUS;
    if (flags & PROC_FILLCMD) opts->flags |= DISP_CMD;
    return (flags);
}

#ifdef PROC_COLOR

struct markup_s *Markuplist;

int check_markup ( char *markup)
{
   int i,j, m[3];

   i = strlen(markup);
   if (i != 2 && i != 5 && i != 8) return 1; /* illegal length */
   j = sscanf (markup,"%d;%d;%d",&m[0], &m[1], &m[2]);
   if (3*j-1 != i) return 1;                    /* syntax error   */
   for (i=0; i<j ;i++)
	switch (m[i]) {
	case 1: case 4: case 5: case 7: case 8:
        case 30: case 31: case 32: case 33: case 34: case 35: case 36: case 37:
	case 40: case 41: case 42: case 43: case 44: case 45: case 46: case 47:
	      break;
        default: return 1;   /* illegal colour code */
	};
   return 0;
}

int singlecolorstring (char *color, dopts *opts)
{
    int type, margin;
    char tmp[256],markup[256],*c;
    static struct markup_s *last=NULL;

    if (Markuplist == NULL) {
        Markuplist = xcalloc ( NULL, sizeof(struct markup_s));
	last = Markuplist;
	last->type = none;
	last->spec = NULL;
	last->colspec="0";
	last->next = NULL;
    } else if (last == NULL) {
        last = Markuplist;
    }
    while ((c=strchr(color, ',')) != NULL) *c=' '; 
    if (strlen(color) > 200) {
	fprintf(stderr, "Color markup field too long, exiting.\n");
	exit(1);
    }
     
    if (sscanf(color, "%i %s %s", &type, tmp, markup) != 3) {
	return 0;   /* markup string failure */
    }
    if (check_markup (markup)) return 0;
    last->next = xcalloc(NULL,sizeof(struct markup_s));
    last = last->next;
    last->type=type;
    last->colspec=(char *)xcalloc(NULL, (strlen(markup)+1)*sizeof(char));
    strcpy (last->colspec, markup);
    switch (type) {
    case g_mem:
	break;
    case p_uid:
    case p_mem:
    case p_cpu:
    case p_size:
       	margin = atoi(tmp);
        last->spec = xcalloc(NULL,sizeof(int));
        *((int *)last->spec) = margin;
	break;
    case p_stat:
	last->spec =xcalloc(NULL, sizeof(char));
	*((char *)last->spec) = tmp[0];
	break;
    case p_tty:
        last->colspec=(char *)xcalloc(NULL, (strlen(tmp)+1)*sizeof(char));
        strcpy (last->colspec, tmp);
	break;
    }
    return 1;
}

int parsecolorstring (char *colors, dopts *opts)
{
    char *ce, tmp[1024];
    int result=0;

    strcpy(tmp, colors);
    ce=strtok( tmp, "%");
    while (ce != NULL) {
	if (strlen(ce) > 250) return 0;
        result += singlecolorstring( ce, opts);
        ce = strtok( NULL, "%");
    }
    return result;
}

#ifdef COLOR_FILE
int parsecolorfile (FILE *fp, dopts *opts)
{
    int i,result=0,type;
    int tmp_i;
    char *tmp_c;
    char tmp_ch;
    unsigned tmp_u;
    static struct markup_s *last=NULL;
    char *c,*d;
    char buf[256];

    if (Markuplist == NULL) {
        Markuplist = xcalloc ( NULL, sizeof(struct markup_s));
	last = Markuplist;
	last->type = none;
	last->spec = NULL;
	last->colspec="0";
	last->next = NULL;
    } else if (last == NULL) {
        last = Markuplist;
    }
    
    while (fgets(buf, 254, fp)!= NULL) {
        c=strchr(buf,'#');
	if(c!=NULL) *c='\0';
	if (sscanf(buf,"%d", &type) == 1) {
	    c=strchr(buf, ':');
	    if (c==NULL) {
                fprintf(stderr,PROGNAME ": error in color config file\n");
		return(0);
	    }
	    c++;
	    switch (type) {
	    case g_mem:
	        if (sscanf(c,"%ud",&tmp_u) != 1) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		last->next = xcalloc(NULL,sizeof(struct markup_s));
		last = last->next;
		last->type=g_mem;
		last->spec = xcalloc(NULL,sizeof(unsigned));
		*((unsigned *) last->spec) = tmp_u;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		}
		c++;
		d=c;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} else { 
		    last->colspec=(char *)xcalloc(NULL, (c-d+2)*sizeof(char));
		    *c=0;
		    strcpy (last->colspec, d);
		}
		result++;
		break;
	    case p_uid:
	        if (sscanf(c,"%d",&tmp_i) != 1) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		last->next = xcalloc(NULL,sizeof(struct markup_s));
		last = last->next;
		last->type=p_uid;
		last->spec = xcalloc(NULL,sizeof(int));
		*((int *)last->spec) = tmp_i;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		c++;
		d=c;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} else { 
		    last->colspec=(char *)xcalloc(NULL, (c-d+2)*sizeof(char));
		    *c=0;
		    strcpy (last->colspec, d);
		}
		result++;
		break;
	    case p_stat:
	        c=strchr(c,'\'');
	        if (c==NULL || sscanf(c,"'%c'",&tmp_ch) != 1) {
                    fprintf(stderr,PROGNAME ": error in color config file\n");
                    return(0);
		} 
		last->next = xcalloc(NULL,sizeof(struct markup_s));
		last = last->next;
		last->type=p_stat;
		last->spec = xcalloc(NULL,sizeof(char));
		*((char *)last->spec) = tmp_ch;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		c++;
		d=c;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} else { 
		    last->colspec=(char *)xcalloc(NULL, (c-d+2)*sizeof(char));
		    *c=0;
		    strcpy (last->colspec, d);
		}
		result++;
		break;
	    case p_mem:
	        if (sscanf(c,"%d%%",&tmp_i) != 1) {
                    fprintf(stderr,PROGNAME ": error in color config file\n");
                    return(0);
		} 
		last->next = xcalloc(NULL,sizeof(struct markup_s));
		last = last->next;
		last->type=p_mem;
		last->spec = xcalloc(NULL,sizeof(unsigned));
		*((int *)last->spec) = 10*tmp_i;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		c++;
		d=c;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} else { 
		    last->colspec=(char *)xcalloc(NULL, (c-d+2)*sizeof(char));
		    *c=0;
		    strcpy (last->colspec, d);
		}
		result++;
		break;
	    case p_cpu:
	        if (sscanf(c,"%ud%%",&tmp_u) != 1) {
                    fprintf(stderr,PROGNAME ": error in color config file\n");
                    return(0);
		} 
		last->next = xcalloc(NULL,sizeof(struct markup_s));
		last = last->next;
		last->type=p_cpu;
		last->spec = xcalloc(NULL,sizeof(unsigned));
		*((unsigned *)last->spec) = 10*tmp_u;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		c++;
		d=c;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} else { 
		    last->colspec=(char *)xcalloc(NULL, (c-d+2)*sizeof(char));
		    *c=0;
		    strcpy (last->colspec, d);
		}
		result++;
		break;
	    case p_size:
	        if (sscanf(c,"%d",&tmp_i) != 1) {
                    fprintf(stderr,PROGNAME ": error in color config file\n");
                    return(0);
		} 
		last->next = xcalloc(NULL,sizeof(struct markup_s));
		last = last->next;
		last->type=p_size;
		last->spec = xcalloc(NULL,sizeof(int));
		*((int *)last->spec) = tmp_i >> opts->pg_shift;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		c++;
		d=c;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} else { 
		    last->colspec=(char *)xcalloc(NULL, (c-d+2)*sizeof(char));
		    *c=0;
		    strcpy (last->colspec, d);
		}
		result++;
		break;
	    case p_tty:
	        c=strchr(c,'\'');
	        if (c==NULL) {
                    fprintf(stderr,PROGNAME ": error in color config file\n");
                    return(0);
		}
		c++;
		d=c;
		c=strchr(c,'\'');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		last->next = xcalloc(NULL,sizeof(struct markup_s));
		last = last->next;
		last->spec = xcalloc(NULL, (c-d+2)*sizeof(char));
		*c = 0;
		strcpy((char *)last->spec,d);
		*c = '\'';
		last->type=p_tty;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} 
		c++;
		d=c;
		c=strchr(c,'|');
		if (c==NULL) {
		    fprintf(stderr,PROGNAME ": error in color config file\n");
		    return(0);
		} else { 
		    last->colspec=(char *)xcalloc(NULL, (c-d+2)*sizeof(char));
		    *c=0;
		    strcpy (last->colspec, d);
		}
		result++;
		break;
	    } 
	}
    }
    return result;
}
#endif

char * markup (proc_t * task, int pmem)
{
    struct markup_s *p=Markuplist;

    while (p!=NULL ){
        switch (p->type){
	case p_cpu:
	    if (task->pcpu > *(int *)p->spec) return p->colspec;
	    break;
	case p_uid:
	    if (task->uid == *(unsigned *)p->spec) return p->colspec;
	    break;
	case p_stat:
	    if (task->state == *(char *)p->spec) return p->colspec;
	    break;
	case p_mem:
	    if (pmem > *(int *)p->spec) return p->colspec;
	    break;
	case p_size:
	    if (task->size > *(int *)p->spec) return p->colspec;
	    break;
	case p_tty:
	    if (strstr(task->ttyc, (char *)p->spec)!=NULL) return p->colspec;
	    break;
	default:
	    break;
	}
	p= p->next;
    }
    return NULL;
}
#endif /* PROC_COLOR */


void setup_colors (dopts *opts, char *colorenv)     /* note there is no default colorisation */
{
#ifdef PROC_COLOR
#ifdef COLOR_FILE
    FILE *fp;
    char rcfile[MAXNAMELEN];
#endif /* COLOR_FILE */
    int flag=0;

    if (colorenv){
        flag = parsecolorstring(colorenv, opts);
    }
#ifdef COLOR_FILE
    if (!flag){
        if (getenv("HOME")){
            strcpy (rcfile, getenv ("HOME"));
            strcat (rcfile, "/");
        }
        strcat (rcfile, COLOR_RC);
        if ((fp=fopen(rcfile, "r"))!= NULL) {
            flag |= parsecolorfile(fp, opts);
            fclose(fp);
        }
        if ((fp=fopen(SYS_COLOR_RC, "r"))!= NULL) {
            flag |= parsecolorfile(fp, opts);
            fclose(fp);
        }
    }
#endif /* COLOR_FILE*/
    if (!flag) 
#endif /* PROC_COLOR */
        CLEAR(COLOR);
}

char *prtime(char *s, unsigned long t, unsigned long rel) {
    if (t == 0) {
        sprintf(s, "     ");
        return s;
    }
    if ((long) t == -1) {
        sprintf(s, "   xx");
        return s;
    }
    if ((long) (t -= rel) < 0)
        t = 0;
    if (t > 9999)
        sprintf(s, "%5lu", t / 100);
    else
        sprintf(s, "%2lu.%02lu", t / 100, t % 100);
    return s;
}

int show_task_info (proc_t * task, int main_mem, int current_time, dopts *opts)
{
    int OUT_SZ_MODE= opts->pg_shift ? OUT_SZ_MK : OUT_SZ_PM;
    int i, n=0, N=0, pmem=0, max;
#ifdef PROC_COLOR
    char *mark=NULL;
#endif
    unsigned int t;
    char tmp3[MAXLINES] = "";
    time_t time_v, sec;

#ifdef PROC_COLOR
    if (TEST(COLOR) && (mark=markup(task, pmem))!= NULL ) {
        sprintf(tmp3,LEFT_MARKUP);
	strcat(tmp3, mark);
	strcat(tmp3, RIGHT_MARKUP);
        printf("%s",tmp3);
    } 
#endif
    for (i = 0; i < opts->Numfields; i++) {
        memset (tmp3, 0, 4096);
        max = headers[opts->p[i]].ln;
        if (max==-1) max=opts->stretch;
         switch (opts->p[i]) {
	case P_USER:
	    n = print_str (task->user, max);
	    break;
	case P_RUSER:
	    n = print_str (task->ruser, max);
	    break;
	case P_SUSER:
	    n = print_str (task->suser, max);
	    break;
	case P_FSUSER:
	    n = print_str (task->fsuser, max);
	    break;
        case P_GROUP:
	    n = print_str (task->group, max);
	    break;
        case P_RGROUP:
	    n = print_str (task->rgroup, max);
	    break;
        case P_SGROUP:
	    n = print_str (task->sgroup, max);
	    break;
        case P_FSGROUP:
	    n = print_str (task->fsgroup, max);
	    break;
	case P_COMMAND:
        case P_ARGS:
	    if (TEST(CMD) && task->cmdline && *(task->cmdline)) {
                n = print_strlist(task->cmdline, max, " ");
            } else {
                n = print_str(task->cmd, max);
	    }
	    break;
	case P_STAT:
	    n = print_str (status (task), max);
	    break;
	case P_TTY:
	    n = print_str (task->ttyc, max);
	    break;
        case P_ENV:
            if (task->environ) {
                n = print_strlist(task->environ, max, " ");
            } else {
                n = print_str("-access not permitted-", max);
            }
	    break;
	case P_UID:
	    n = print_dec (task->uid, max);
	    break;
	case P_RUID:
	    n = print_dec (task->ruid, max);
	    break;
	case P_SUID:
	    n = print_dec (task->suid, max);
	    break;
	case P_FSUID:
	    n = print_dec (task->fsuid, max);
	    break;
	case P_GID:
	    n = print_dec (task->gid, max);
	    break;
	case P_RGID:
	    n = print_dec (task->rgid, max);
	    break;
	case P_SGID:
	    n = print_dec (task->sgid, max);
	    break;
	case P_FSGID:
	    n = print_dec (task->fsgid, max);
	    break;
	case P_PID:
	    n = print_dec (task->pid, max);
	    break;
	case P_PPID:
	    n = print_dec (task->ppid, max);
	    break;
	case P_PGID:
	    n = print_dec (task->pgrp, max);
	    break;
        case P_SID:
	    n = print_dec (task->session, max);
            break;
        case P_TPGID:
	    n = print_dec (task->tpgid, max);
            break;
	case P_UTIME:
	    t = task->utime;
	    if (TEST(CUMULATIVE))
		t += task->cutime;
            if (TEST(NEWTIMFORM)){
                n = print_time_ival (t/HZ, max, (t%HZ) * 100 / HZ);
            } else {
                n = print_time_old_ival(t/HZ, max);
            }
	    break;
	case P_STIME:
	    t = task->stime;
	    if (TEST(CUMULATIVE))
		t += task->cstime;
            if (TEST(NEWTIMFORM)){
                n = print_time_ival (t/HZ, max, (t%HZ) * 100 / HZ);
            } else {
                n = print_time_old_ival(t/HZ, max);
            }
	    break;
	case P_TIME:
	    t = (task->utime + task->stime);
	    if (TEST(CUMULATIVE))
		t += (task->cutime + task->cstime);
            if (TEST(NEWTIMFORM)){
                n = print_time_ival (t/HZ, max, (t%HZ) * 100 / HZ);
            } else {
                n = print_time_old_ival(t/HZ, max);
            }
	    break;
	case P_PRI:
	    n = print_dec (task->priority, max);
	    break;
	case P_NICE:
	    n = print_dec (task->nice, max);
	    break;
	case P_ETIME:
            if (task->start_time > current_time*HZ) {
                t = 0;
            } else {
                t = (current_time*HZ - task->start_time);
            }
            if (TEST(NEWTIMFORM)){
                n = print_time_ival(t/HZ, max, (t%HZ) * 100 / HZ);
            } else {
                n = print_time_old_ival(t/HZ, max);
            }
	    break;
        case P_START:
            sec = (current_time*HZ - task->start_time)/HZ;
            time_v = time(0L) - sec;
            n = printf("%.6s ", ctime(&time_v) 
                    + (sec > 3600*24 ? 4 : 10));
            break;
	case P_MSIZE:
            n = print_size (task->size << opts->pg_shift, max, OUT_SZ_MODE);
            break;
        case P_MRES:
            n = print_size (task->resident << opts->pg_shift, max, OUT_SZ_MODE);
	    break;
	case P_MSHRD:
            n = print_size (task->share << opts->pg_shift, max, OUT_SZ_MODE);
	    break;
	case P_MSWAP:
            n = print_size ((task->size - task->resident) << opts->pg_shift,
                         max, OUT_SZ_MODE);
	    break;
	case P_MDRS:
            n = print_size (task->drs << opts->pg_shift, max, OUT_SZ_MODE);
	    break;
	case P_MTRS:
            n = print_size (task->trs << opts->pg_shift, max, OUT_SZ_MODE);
	    break;
	case P_MLRS:
            n = print_size (task->lrs << opts->pg_shift, max, OUT_SZ_MODE);
	    break;
	case P_MDT:
	    n = print_dec (task->dt, max);
	    break;
	case P_FLAGS:
	    n = print_hex (task->flags, max);
	    break;
	case P_MAJFLT:
	    if (TEST(CUMULATIVE)) {
                n = print_dec (task->cmaj_flt,max);
            } else {
                n = print_dec (task->maj_flt,max);
            }
            break;
	case P_MINFLT:
	    if (TEST(CUMULATIVE)) {
                n = print_dec (task->cmin_flt, max);
            } else {
                n = print_dec (task->min_flt, max);
            }
            break;
	case P_VSIZE:
	    n = print_size (task->vsize >> (PAGE_SHIFT - opts->pg_shift), 
                         max, OUT_SZ_MODE);
	    break;
	case P_RSS:
	    n = print_size (task->rss << opts->pg_shift, max, OUT_SZ_MODE);
	    break;
        case P_SCODE:
            n = print_hex (task->start_code, max);
	    break;
        case P_ECODE:
            n = print_hex (task->end_code, max);
	    break;
        case P_STACKP:
            n = print_hex (task->start_stack, max);
	    break;
        case P_ESP:
            n = print_hex (task->kstk_esp, max);
	    break;
        case P_EIP:
            n = print_hex (task->kstk_eip, max);
	    break;
	case P_WCHAN:
 	    if (TEST(WCHAN))
		n = print_str (wchan (task->wchan), max);
	    else /* just the lower 32 bits of a kernel address are really
                    interesting */
		n = print_hex (task->wchan & 0xffffffff, max);
	    break;
        case P_SIGNAL:
            n = print_hex (task->signal, max);
	    break;
        case P_SIGBLOCK:
            n = print_hex (task->blocked, max);
	    break;
        case P_SIGIGN:
            n = print_hex (task->sigignore, max);
	    break;
        case P_SIGCATCH:
            n = print_hex (task->sigcatch, max);
	    break;
	case P_VLOCK:
	    n = print_size (task->vm_lock, max, OUT_SZ_MK);
	    break;
	case P_VEXE:
	    n = print_size (task->vm_exe, max, OUT_SZ_MK);
	    break;
	case P_VSTACK:
	    n = print_size (task->vm_stack, max, OUT_SZ_MK);
	    break;
	case P_VLIB:
	    n = print_size (task->vm_lib, max, OUT_SZ_MK);
	    break;
	case P_VDATA:
            n = print_size (task->vm_data, max, OUT_SZ_MK);
	    break;
	case P_PCPU:
            n = print_perc (task->pcpu, max);
	    break;
        case P_PMEM:
            pmem = task->rss * 1000 / (main_mem >> PAGE_SHIFT);
            if (pmem > 999) pmem = 999;
	    n = print_perc (pmem, max);
	    break;
        case P_TIMEOUT:
            n = print_str (prtime(tmp3, task->timeout, current_time*HZ),
                        max);
            break;
        case P_ALARM:
            n = print_str(prtime(tmp3, task->it_real_value, 0), max);
            break;
        }
         if ( n<0 ) {
             resize_field ( i, opts, -n);
             N-=n;
             printf(" ");
             N++;
         } else {
             N+=n;

             if (headers[opts->p[i]].ln!=-1) {
                 printf("%*.*s",max-n+1,max-n+1,"                                                                               ");
                 N+=max-n+1;
             } else {
                 printf(" ");
                 N++;
             }
         }
         
        if (N > (opts->Cols * (opts->Lines+1)) -1 ) break;
    }
#ifdef PROC_COLOR
    if (TEST(COLOR) && mark!=NULL ) { 
        printf("%s",END_MARKUP);
    } 
#endif
    return(N);
}


