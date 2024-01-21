/* fuser.c - identify processes using files */

/* Copyright 1993-1997 Werner Almesberger. See file COPYING for details. */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <dirent.h>
#include <pwd.h>
#include <signal.h>
#include <limits.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <linux/kdev_t.h> /* for MKDEV */

#include "comm.h"
#include "signals.h"


#define PROC_BASE  "/proc"
#define UID_UNKNOWN -1
#define NAME_FIELD 20 /* space reserved for file name */

#define MAX_LINE 256 /* longest line we may ever find in /proc */


#define REF_FILE   1	/* an open file */
#define REF_ROOT   2	/* current root */
#define REF_CWD    4	/* current directory */
#define REF_EXE    8	/* executable */
#define REF_MMAP  16	/* mmap'ed file or library */

#define FLAG_KILL  1	/* kill process */
#define FLAG_UID   2	/* show uid */
#define FLAG_VERB  4	/* show verbose output */
#define FLAG_DEV   8	/* show all processes using this device */


typedef struct _net_cache {
    int lcl_port;
    int rmt_port;
    unsigned long rmt_addr;
    ino_t ino;
    struct _net_cache *next;
} NET_CACHE;

typedef struct _unix_cache {
    dev_t fs_dev;
    ino_t fs_ino;
    ino_t net_ino;
    struct _unix_cache *next;
} UNIX_CACHE;

typedef struct {
    const char *name;
    NET_CACHE *cache;
    int once;
} SPACE_DSC;

typedef struct proc_dsc {
    pid_t pid;
    int ref_set;
    int uid; /* must also accept UID_UNKNOWN */
    struct proc_dsc *next;
} PROC_DSC;

typedef struct file_dsc {
    const char *name;  /* NULL if previous entry has name */
    dev_t dev;
    ino_t ino;
    int flags,sig_num;
    SPACE_DSC *name_space; /* or NULL if no indication */
    PROC_DSC *procs;
    struct file_dsc *named,*next;
} FILE_DSC;

static SPACE_DSC name_spaces[] = {
    { "file", NULL, 0 }, /* must be first */
    { "tcp",  NULL, 0 },
    { "udp",  NULL, 0 },
    { NULL,   NULL, 0 }
};


static FILE_DSC *files = NULL;
static UNIX_CACHE *unix_cache = NULL;
static int all = 0,found_proc = 0;


static void fill_net_cache(SPACE_DSC *dsc)
{
    FILE *file;
    NET_CACHE *new,*last;
    char buffer[PATH_MAX+1],line[MAX_LINE+1];

    if (dsc->once) return;
    dsc->once = 1;
    sprintf(buffer,PROC_BASE "/net/%s",dsc->name);
    if (!(file = fopen(buffer,"r"))) {
	perror(buffer);
	exit(1);
    }
    last = NULL;
    (void) fgets(line,MAX_LINE,file);
    while (fgets(line,MAX_LINE,file)) {
	new = malloc(sizeof(NET_CACHE));
	if (!new) {
	    perror("malloc");
	    exit(1);
	}
	if (sscanf(line,"%*d: %*x:%x %lx:%x %*x %*x:%*x %*x:%*x %*x %*d %*d "
	  "%ld",&new->lcl_port,&new->rmt_addr,&new->rmt_port,&new->ino) != 4) {
	    free(new);
	    continue;
	}
	if (!new->ino) {
	    free(new);
	    continue;
	}
	new->next = NULL;
	if (last) last->next = new;
	else dsc->cache = new;
	last = new;
    }
    (void) fclose(file);
}


static void fill_unix_cache(void)
{
    static int once;
    FILE *file;
    UNIX_CACHE *new,*last;
    struct stat st;
    char path[PATH_MAX+1],line[MAX_LINE+1];
    int ino;

    if (once) return;
    once = 1;
    if (!(file = fopen(PROC_BASE "/net/unix","r"))) {
	perror(PROC_BASE "/net/unix");
	exit(1);
    }
    last = NULL;
    (void) fgets(line,MAX_LINE,file);
    while (fgets(line,MAX_LINE,file)) {
	if (sscanf(line,"%*x: %*x %*x %*x %*x %*x %d %s",&ino,path) != 2)
	    continue;
	if (stat(path,&st) < 0) continue;
	new = malloc(sizeof(UNIX_CACHE));
	new->fs_dev = st.st_dev;
	new->fs_ino = st.st_ino;
	new->net_ino = ino;
	new->next = NULL;
	if (last) last->next = new;
	else unix_cache = new;
	last = new;
    }
    (void) fclose(file);
    
}


static void add_file(const char *path,unsigned long device,unsigned long inode,
  pid_t pid,int type)
{
    struct stat st;
    FILE_DSC *file,*next;
    PROC_DSC **proc,*this;

    for (file = files; file; file = next) {
	next = file->next;
	if ((inode == file->ino || (file->flags & FLAG_DEV)) &&
	  device == file->dev) {
	    if (!file->name) file = file->named;
	    for (proc = &file->procs; *proc; proc = &(*proc)->next)
		if ((*proc)->pid >= pid) break;
	    if (*proc && (*proc)->pid == pid) this = *proc;
	    else {
		if (!(this = malloc(sizeof(PROC_DSC)))) {
		    perror("malloc");
		    exit(1);
		}
		this->pid = pid;
		this->ref_set = 0;
		this->uid = UID_UNKNOWN;
		this->next = *proc;
		*proc = this;
		found_proc = 1;
	    }
	    this->ref_set |= type;
	    if ((file->flags & (FLAG_UID | FLAG_VERB)) && this->uid ==
	      UID_UNKNOWN && lstat(path,&st) >= 0) this->uid = st.st_uid;
	}
    }
}


static void check_link(const char *path,pid_t pid,int type)
{
    char buffer[PATH_MAX+1];
    unsigned long device,inode;
    int length;

    if ((length = readlink(path,buffer,PATH_MAX)) < 0) return;
    buffer[length] = 0;
    if (sscanf(buffer,"[%lx]:%ld",&device,&inode) == 2)
	add_file(path,device,inode,pid,type);
}


static void check_map(const char *rel,pid_t pid,int type)
{
    FILE *file;
    char line[MAX_LINE+1];
    int major,minor;
    unsigned long inode;

    if (!(file = fopen(rel,"r"))) return;
    while (fgets(line,MAX_LINE,file)) {
	if (sscanf(line,"%*s %*s %*s %x:%x %ld",&major,&minor,&inode) != 3)
	    continue;
	if (major || minor || inode)
	    add_file(rel,MKDEV(major,minor),inode,pid,type);
    }
    fclose(file);
}


static void check_dir(const char *rel,pid_t pid,int type)
{
    DIR *dir;
    struct dirent *de;
    char path[PATH_MAX+1];

    if (!(dir = opendir(rel))) return;
    while (de = readdir(dir))
	if (strcmp(de->d_name,".") && strcmp(de->d_name,"..")) {
	    sprintf(path,"%s/%s",rel,de->d_name);
	    check_link(path,pid,type);
	}
    (void) closedir(dir);
}


static void scan_proc(void)
{
    DIR *dir;
    struct dirent *de;
    char path[PATH_MAX+1];
    pid_t pid;
    int empty;

    if (!(dir = opendir(PROC_BASE))) {
	perror(PROC_BASE);
	exit(1);
    }
    empty = 1;
    while (de = readdir(dir))
	if (pid = atoi(de->d_name)) {
	    empty = 0;
	    sprintf(path,"%s/%d",PROC_BASE,pid);
	    if (chdir(path) >= 0) {
		check_link("root",pid,REF_ROOT);
		check_link("cwd",pid,REF_CWD);
		check_link("exe",pid,REF_EXE);
		check_dir("lib",pid,REF_MMAP);
		check_dir("mmap",pid,REF_MMAP);
		check_map("maps",pid,REF_MMAP);
		check_dir("fd",pid,REF_FILE);
	    }
	}
    (void) closedir(dir);
    if (empty) {
	fprintf(stderr,PROC_BASE " is empty (not mounted ?)\n");
	exit(1);
    }
}


static void show_files(void)
{
    const FILE_DSC *file;
    const PROC_DSC *proc;
    FILE *f;
    const struct passwd *pw;
    const char *name,*scan;
    char tmp[10],path[PATH_MAX+1],comm[COMM_LEN+1];
    int length,header,first,dummy,last_namelen;
    pid_t self;

    self = getpid();
    header = 1;
    for (file = files; file; file = file->next)
	if (file->procs || all) {
	    if (header && (file->flags & FLAG_VERB)) {
		printf("\n%*s USER       PID ACCESS COMMAND\n",NAME_FIELD,"");
		header = 0;
	    }
	    length = 0;
	    for (scan = file->name; scan && *scan; scan++)
		if (*scan == '\\') length += printf("\\\\");
		else if (*scan > ' ' && *scan <= '~') {
			putchar(*scan);
			length++;
		    }
		    else length += printf("\\%03o",*scan);
	    if (file->name_space)
		length += printf("/%s",file->name_space->name);
            if (length != 0) 
                last_namelen=length;
            else
                printf("%*.*s",last_namelen,last_namelen,"                                                                              ");
	    if (!(file->flags & FLAG_VERB)) {
		putchar(':');
		length++;
	    }
	    while (length < NAME_FIELD) {
		putchar(' ');
		length++;
	    }
	    first = 1;
	    for (proc = file->procs; proc; proc = proc->next)
		if (!(file->flags & FLAG_VERB)) {
		    if (proc->ref_set & REF_FILE) printf("%6d",proc->pid);
		    if (proc->ref_set & REF_ROOT) printf("%6dr",proc->pid);
		    if (proc->ref_set & REF_CWD) printf("%6dc",proc->pid);
		    if (proc->ref_set & REF_EXE) printf("%6de",proc->pid);
		    else if (proc->ref_set & REF_MMAP) printf("%6dm",proc->pid);
		    if (file->flags & FLAG_UID && proc->uid != UID_UNKNOWN)
			if (pw = getpwuid(proc->uid))
			    printf("(%s)",pw->pw_name);
			else printf("(%d)",proc->uid);
		}
		else {
		    sprintf(path,PROC_BASE "/%d/stat",proc->pid);
		    strcpy(comm,"???");
		    if (f = fopen(path,"r")) {
			(void) fscanf(f,"%d (%[^)]",&dummy,comm);
			(void) fclose(f);
		    }
		    if (proc->uid == UID_UNKNOWN) name = "???";
		    else if (pw = getpwuid(proc->uid)) name = pw->pw_name;
			else {
			    sprintf(tmp,"%d",proc->uid);
			    name = tmp;
			}
		    if (!first) printf("%*s",NAME_FIELD,"");
		    else if (length > NAME_FIELD)
			    printf("\n%*s",NAME_FIELD,"");
		    first = 0;
		    printf(" %-8s %5d %c%c%c%c%c  ",name,proc->pid,
		      proc->ref_set & REF_FILE ? 'f' : '.',proc->ref_set &
		      REF_ROOT ? 'r' : '.',proc->ref_set & REF_CWD ? 'c' : '.',
		      proc->ref_set & REF_EXE ? 'e' : '.',(proc->ref_set &
		      REF_MMAP) && !(proc->ref_set & REF_EXE) ? 'm' : '.');
		    for (scan = comm; *scan; scan++)
			if (*scan == '\\') printf("\\\\");
			else if (*scan > ' ' && *scan <= '~') putchar(*scan);
			    else printf("\\%03o",(unsigned char) *scan);
		    putchar('\n');
		}
	    if (!(file->flags & FLAG_VERB) || first) putchar('\n');
	    if (file->flags & FLAG_KILL)
		for (proc = file->procs; proc; proc = proc->next)
		    if (proc->pid != self)
			if (kill(proc->pid,file->sig_num) < 0) {
			    sprintf(tmp,"kill %d",proc->pid);
			    perror(tmp);
			}
	}
}


static void enter_item(const char *name,int flags,int sig_number,dev_t dev,
  ino_t ino,SPACE_DSC *name_space)
{
    static FILE_DSC *last = NULL;
    static FILE_DSC *last_named = NULL;
    FILE_DSC *new;

    if (!(new = malloc(sizeof(FILE_DSC)))) {
	perror("malloc");
	exit(1);
    }
    if (last_named && !strcmp(last_named->name,name) &&
      last_named->name_space == name_space) new->name = NULL;
    else if (!(new->name = strdup(name))) {
	    perror("strdup");
	    exit(1);
	}
    new->flags = flags;
    new->sig_num = sig_number;
    new->procs = NULL;
    new->next = NULL;
    new->dev = dev;
    new->ino = ino;
    new->name_space = name_space;
    if (last) last->next = new;
    else files = new;
    last = new;
    new->named = last_named;
    if (new->name) last_named = new;
}


static int parse_inet(const char *spec,const char *name_space,int *lcl_port,
  unsigned long *rmt_addr,int *rmt_port)
{
    char *s,*here,*next,*end;
    int port,field;

    if (!(s = strdup(spec))) {
	perror("strdup");
	exit(1);
    }
    *lcl_port = *rmt_port = -1;
    *rmt_addr = 0;
    field = 0;
    for (here = s; here; here = next ? next+1 : NULL) {
	next = strchr(here,',');
	if (next) *next = 0;
	switch (field) {
	    case 0:
		/* fall through */
	    case 2:
		if (!*here) break;
		port = strtoul(here,&end,0);
		if (*end) {
		    struct servent *se;

		    if (!(se = getservbyname(here,name_space)))
			return 0;
		    port = ntohs(se->s_port);
		}
		if (field) *rmt_port = port;
		else *lcl_port = port;
		break;
	    case 1:
		if (!*here) break;
		if ((long) (*rmt_addr = inet_addr(here)) == -1) {
		    struct hostent *hostent;

		    if (!(hostent = gethostbyname(here))) return 0;
		    if (hostent->h_addrtype != AF_INET) return 0;
		    memcpy(rmt_addr,hostent->h_addr,hostent->h_length);
		}
		break;
	    default:
		return 0;
	}
	field++;
    }
    return 1;
}


static void usage(void)
{
    fprintf(stderr,"usage: fuser [ -a | -q ] [ -n space ] [ -signal ] "
      "[ -kmuv ] filename ...\n%13s[ - ] [ -n space ] [ -signal ] [ -kmuv ] "
      "filename ...\n","");
    fprintf(stderr,"       fuser -l\n");
    fprintf(stderr,"       fuser -V\n\n");
    fprintf(stderr,"    -a        display unused files too\n");
    fprintf(stderr,"    -k        kill processes accessing that file\n");
    fprintf(stderr,"    -l        list signal names\n");
    fprintf(stderr,"    -m        mounted FS\n");
    fprintf(stderr,"    -n space  search in the specified name space (file, "
      "udp, or tcp)\n");
    fprintf(stderr,"    -s        silent operation\n");
    fprintf(stderr,"    -signal   send signal instead of SIGKILL\n");
    fprintf(stderr,"    -u        display user ids\n");
    fprintf(stderr,"    -v        verbose output\n");
    fprintf(stderr,"    -V        display version information\n");
    fprintf(stderr,"    -         reset options\n\n");
    exit(1);
}


int main(int argc,char **argv)
{
    SPACE_DSC *name_space;
    char path[PATH_MAX+1];
    int flags,silent,sig_number,no_files;

    flags = silent = 0;
    sig_number = SIGKILL;
    name_space = name_spaces;
    no_files = 1;
    if (argc < 2) usage();
    if (argc == 2 && !strcmp(argv[1],"-l")) {
	list_signals();
	return 0;
    }
    while (--argc) {
	argv++;
	if (**argv == '-')
	    if (!argv[0][1]) {
		flags = 0;
		sig_number = SIGKILL;
	    }
	    else while (*++*argv) {
		    int end;

		    end = 0;
		    switch (**argv) {
			case 'a':
			    all = 1;
			    break;
			case 'k':
			    flags |= FLAG_KILL;
			    break;
			case 'm':
			    flags |= FLAG_DEV;
			    break;
			case 'n':
			    if (!--argc) usage();
			    argv++;
			    for (name_space = name_spaces; name_space->name;
			      name_space++)
				if (!strcmp(*argv,name_space->name)) break;
			    if (!name_space->name) usage();
			    end = 1;
			    break;
			case 's':
			    silent = 1;
			    break;
			case 'u':
			    flags |= FLAG_UID;
			    break;
			case 'v':
			    flags |= FLAG_VERB;
			    break;
			case 'V':
			    fprintf(stderr,"fuser from psmisc version "
			      PSMISC_VERSION "\n");
			    return 0;
			default:
			    if (isupper(**argv) || isdigit(**argv)) {
				sig_number = get_signal(*argv,"fuser");
				argv[0][1] = 0;
				break;
			    }
			    usage();
		    }
		    if (end) break;
		}
	else {
	    SPACE_DSC *this_name_space;
	    char *here;

	    no_files = 0;
	    this_name_space = name_space;
	    here = strchr(*argv,'/');
	    if (here && here != *argv)
		for (this_name_space = name_spaces; this_name_space->name;
		  this_name_space++)
		    if (!strcmp(here+1,this_name_space->name)) {
			*here = 0;
			break;
		    }
	    if (!this_name_space->name) this_name_space = name_space;
	    if (this_name_space == name_spaces) {
		struct stat st;

		if (stat(*argv,&st) < 0) {
		    perror(*argv);
		    continue;
		}
		if (S_ISSOCK(st.st_mode)) {
		    UNIX_CACHE *walk;

		    fill_unix_cache();
		    for (walk = unix_cache; walk; walk = walk->next)
			if (walk->fs_dev == st.st_dev && walk->fs_ino ==
			  st.st_ino)
			    enter_item(*argv,flags,sig_number,0,walk->net_ino,
			      NULL);
		}
		else {
		    if (flags & FLAG_DEV)
			if (S_ISBLK(st.st_mode)) st.st_dev = st.st_rdev;
			else if (S_ISDIR(st.st_mode)) {
				sprintf(path,"%s/.",*argv);
				if (stat(*argv,&st) < 0) {
				    perror(*argv);
				    continue;
				}
			    }
		    enter_item(*argv,flags,sig_number,st.st_dev,st.st_ino,NULL);
		}
	    }
	    else {
		NET_CACHE *walk;
		unsigned long rmt_addr;
		int lcl_port,rmt_port;

		fill_net_cache(this_name_space);
		if (!parse_inet(*argv,this_name_space->name,&lcl_port,
		  &rmt_addr,&rmt_port)) {
		    fprintf(stderr,"%s/%s: invalid specificiation\n",*argv,
		      this_name_space->name);
		    continue;
		}
		for (walk = this_name_space->cache; walk; walk = walk->next)
		    if ((lcl_port == -1 || walk->lcl_port == lcl_port) &&
		      (!rmt_addr || walk->rmt_addr == rmt_addr) &&
		      (rmt_port == -1 || walk->rmt_port == rmt_port))
			enter_item(*argv,flags,sig_number,0,walk->ino,
			    this_name_space);
	    }
	}
    }
    if (no_files || (all && silent)) usage();
    scan_proc();
    if (!silent) {
	if (seteuid(getuid()) < 0) {
	    perror("seteuid");
	    return 1;
	}
	show_files();
    }
    return found_proc ? 0 : 1;
}
