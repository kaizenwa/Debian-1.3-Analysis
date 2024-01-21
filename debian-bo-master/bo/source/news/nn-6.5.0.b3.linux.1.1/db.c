/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 *	Database access and update
 */

#include "config.h"
#include "db.h"

#ifdef NOV
#include "hash.h"
#include "hdbm.h"
#include "newsoverview.h"
#endif /* NOV */

/* db.c */

#ifndef NOV
static char *mk_archive_file __APROTO((register group_header *gh, register char *cp, int logerr));
#endif
static sort_gh __APROTO((group_header **g1, group_header **g2));

#ifdef NOV
static void db_init_group __APROTO((group_header *gh, int num));
static void readtimfile __APROTO((void));
static void readactfile __APROTO((void));
static void db_fixup_cross_postings __APROTO((data_header *dhp,
					      data_dynamic_data *ddp,
					      struct novart *artp));

static struct novgroup *ngovp;
static struct novart *allarts;
static HASHTABLE *grouptbl, *acttbl, *timtbl;

extern long	atol();
extern char	*strsave();
extern void novclose();		/* trash last group's data */
#endif

import char
*master_directory, *db_directory, *db_data_directory, *news_directory,
  *news_lib_directory;
import int db_data_subdirs;

export master_header master;
#ifdef MALLOC_64K_LIMITATION
export group_header **active_groups = NULL;
#else
export group_header *active_groups = NULL;
#endif
export group_header **sorted_groups = NULL;

export int  reread_groups_file = 0;  /* nnmaster -G */

export int  check_group_access = 0;

export data_header db_hdr;
export data_dynamic_data db_data;

export int32 db_read_counter = 0; 	/* articles read by db_read_art */
export int32 db_write_counter = 0;	/* articles written by db_write_art */

/*
 * Init access to a group
 */

export group_header *current_group = NULL;

export char group_path_name[FILENAME];
export char *group_file_name = NULL;

static char *group_position = NULL;
static article_number current_digest_article = 0;

int
init_group(gh)
register group_header *gh;
{
    register char *p, *q;

    current_digest_article = 0;

    if (gh == NULL) 
      return 0;
    /*    if (gh->master_flag & M_IGNORE_GROUP) return 0; */	/* OBS */
    if (gh == current_group) 
      return 1;

    current_group = gh;

    if (gh->group_flag & G_FOLDER) {
	group_position = NULL;
	group_file_name = NULL;
	strcpy(group_path_name, gh->archive_file);
	return 1;
    }

#ifdef NNTP
    if (use_nntp && nntp_set_group(gh) < 0)
	return 0;
#endif /* NNTP */

    if (group_position == NULL)
	if (who_am_i == I_AM_MASTER || who_am_i == I_AM_EXPIRE)
	    group_position = group_path_name;
	else {
	    strcpy(group_path_name, news_directory);
	    group_position = group_path_name + strlen(group_path_name);
	    *group_position++ = '/';
	}

    for (p = group_position, q = gh->group_name; *q; q++)
	*p++ = (*q == '.') ? '/' : *q;

    if (who_am_i == I_AM_MASTER) {

	/* The master will chdir to the group's directory to get */
	/* better performance (can use relative path names). */

	*p++ = NUL;

	if (!use_nntp) {
	    if (chdir(news_directory) < 0)
		sys_error(news_directory);

	    if (chdir(group_path_name) < 0)
		return 0;
	}
	group_file_name = group_path_name;
	return 1;
    }

    /* client */
    if (gh->master_flag & M_NO_DIRECTORY) 
        return 0;

    if (check_group_access && !use_nntp) {
	*p = NUL;
	if (file_exist(group_path_name, "dxr") == 0) 
	    return 0;
    }

    *p++ = '/';
    *p = NUL;
    group_file_name = p;
    return 1;
}

#ifndef NOV
/*
 *	Open master & group file; read it in if first open.
 *
 *	GROUPS file format is:
 *
 *	One line per group:
 *	<group name> [<space><timestamp>] [<space><options>] <NL>
 *	If <timestamp> is omitted, a timestamp of 0 is used (very old group).
 *	If <group name> is "@", the master entry is ignored.
 *
 *	<options>:
 *	D	Digest all articles in the group
 *	N	Never digest articles
 *	@	Bogus group, ignore completely
 *	!	Don't collect this group
 *
 *	Do not edit the GROUPS file while nnmaster is running.
 *	After editing the groups file (options), run nnmaster -G.
 */

static FILE *group_file = NULL;

int
open_groups(mode)
int mode;
{
    group_file = open_file(relative(db_directory, "GROUPS"), mode);

    if (group_file != NULL && (mode & OPEN_CREATE)) {
	fprintf(group_file, 
"#\n#\tNEVER MODIFY THIS FILE WHILE nnmaster IS RUNNING\n");
	fprintf(group_file, 
"#\n#\tRUN 'nnmaster -G' AFTER MODIFYING THIS FILE\n");
	fprintf(group_file, 
"#\n#\tDO NOT REMOVE OR REORDER ANY LINES IN THIS FILE\n#\n");
    }
    
    return group_file != NULL;
}

void
close_groups()
{
    if (group_file != NULL) {
	fclose(group_file);
	group_file = NULL;
    }
}

void
db_append_group(gh)
register group_header *gh;
{
    char flags[16], *fp;

    if (gh->group_name[0] == NUL) {
	fputc('@', group_file);
	goto out;
    }

    fprintf(group_file, "%s", gh->group_name);
    if (gh->creation_time > 0)
	fprintf(group_file, " %ld", (long)(gh->creation_time));

    fp = flags;

    if (gh->master_flag & M_IGNORE_G)
	*fp++ = '!';
    if (gh->master_flag & M_ALWAYS_DIGEST)
	*fp++ = 'D';
    if (gh->master_flag & M_NEVER_DIGEST)
	*fp++ = 'N';
    if (gh->master_flag & M_INCLUDE_OLD)
	*fp++ = 'O';
    if (gh->master_flag & M_AUTO_RECOLLECT)
	*fp++ = 'R';
    if (gh->archive_file != NULL)
	*fp++ = '>';

    if (fp != flags) {
	*fp++ = NUL;
	fprintf(group_file, " %s%s", flags,
		gh->archive_file != NULL ? gh->archive_file : "");
    }

 out:
    fputc(NL, group_file);
}

void
db_rewrite_groups(save_groups, group_list)
int save_groups;
group_header *group_list;
{
    register group_header *gh;

    if (save_groups)
	if (save_old_file(relative(db_directory, "GROUPS"), "~") < 0)
	    sys_error("Cannot rename GROUPS file");
    
    open_groups(OPEN_CREATE|MUST_EXIST);

    if (group_list != NULL) {
	for (gh = group_list->next_group; gh != NULL; gh = gh->next_group)
	    db_append_group(gh);
    } else {
	Loop_Groups_Header(gh) {
	    db_append_group(gh);
	}
    }
    close_groups();
}

static char *mk_archive_file(gh, cp, logerr)
register group_header *gh;
register char *cp;
int logerr;
{
    char *name;

    while (*cp && (*cp == '>' || isspace(*cp))) 
        cp++;

    name = cp;
    while (*cp && !isspace(*cp)) 
        cp++;
    if (*cp) 
        *cp++ = NUL;

    if (*name) {
	gh->archive_file = copy_str(name);

	if (*name == '/')
	    gh->master_flag |= M_AUTO_ARCHIVE;
	else {
	    gh->master_flag &= ~M_AUTO_ARCHIVE;
	    if (who_am_i == I_AM_MASTER)
		if (logerr)
		    log_entry('E', "GROUPS %s >%s: Full path required",
			      gh->group_name, name);
		else
		    printf("Error in GROUPS: %s >%s: Full path required\n",
			   gh->group_name, name);
	}
    }

    return cp;
}

int
db_parse_group(gh, trust_master)
register group_header *gh;
int trust_master;		/* trust what is in the master file */
{
    char line[256];
    register char *cp, *name;
    int ignore;

    do {
	if (fgets(line, 256, group_file) == NULL) return 0;
	for (cp = line; *cp && isspace(*cp); cp++);
    } while (*cp == NUL || *cp == '#');

    gh->archive_file = NULL;

    name = cp;

    if (trust_master) {
	if (gh->group_name_length == 0) {
	    gh->group_name = "";
	    return 1;
	}
	cp = name + gh->group_name_length;
	if (*cp == NUL || !isspace(*cp))
	    sys_error("MASTER/GROUPS conflict: %d/%s", gh->group_num, line);
    } else {
	/* parse GROUPS line */

	if (*cp == '@') {
	    ignore = 1;
	    gh->group_name_length = 0;
	    gh->group_name = "";
	    goto ignore_group;
	}

	if (gh->group_name_length == 0) {
	    while (*cp && !isspace(*cp)) cp++;
	    gh->group_name_length = cp - name;
	} else {
	    cp = name + gh->group_name_length;
	    if (*cp == NUL || !isspace(*cp)) {
		sys_error("MASTER/GROUPS conflict: %d/%s", gh->group_num, line);
	    }
	}
    }

    if (*cp) *cp++ = NUL;
    if (gh->group_name_length > 0)
	gh->group_name = copy_str(name);
    else
	gh->group_name = "";

    if (trust_master) {
	if (gh->master_flag & M_AUTO_ARCHIVE) {
	    while (*cp && *cp != '>') cp++;
	    if (*cp == '>') cp = mk_archive_file(gh, cp, 1);
	}
	return 1;
    }

    while (*cp && isspace(*cp)) cp++;

    if (*cp && isdigit(*cp)) {
	gh->creation_time = atol(cp);
	while (*cp && isdigit(*cp)) cp++;
    } else
	gh->creation_time = 0;

    while (*cp && isspace(*cp)) cp++;

    ignore = 0;
    gh->master_flag &= ~(M_ALWAYS_DIGEST | M_NEVER_DIGEST |
			M_AUTO_RECOLLECT | M_AUTO_ARCHIVE);

    while (*cp) {
	switch (*cp++) {
	 case ' ':
	 case '\t':
	 case NL:
	 case CR:
	    continue;

	 case 'D':	/* Collect this group, digest all articles */
	    gh->master_flag |= M_ALWAYS_DIGEST;
	    continue;

	 case 'N':	/* Collect this group, never digest articles */
	    gh->master_flag |= M_NEVER_DIGEST;
	    continue;

	 case 'O':	/* Ignore -O option for this group */
	    gh->master_flag |= M_INCLUDE_OLD;
	    continue;

	 case 'R':	/* Recollect this group when new articles arrive */
	    gh->master_flag |= M_AUTO_RECOLLECT;
	    continue;

	 case '>':	/* Archive all new articles in gh->archive_file */
	    cp = mk_archive_file(gh, cp, 0);
	    continue;

	 case '@':	/* Bogus GROUP -- ignore completely */
	    ignore = 1;
	    gh->group_name_length = 0;
	    break;

	 case '!':	/* Do not collect this group */
	 case 'X':
	    ignore = 1;
	    break;

	 case '#':	/* comment */
	    *cp = NUL;
	    break;

	 default:
	    printf("Bad GROUPS flag for %s: `%c'\n", gh->group_name, *--cp);
	    break;
	}
	break;
    }

 ignore_group:

    /* G_DONE indicates to master that the group must be cleaned */

    if (ignore) {
	if ((gh->master_flag & M_IGNORE_GROUP) == 0) {
	    gh->master_flag |= M_MUST_CLEAN;
	    log_entry('X', "Group %s ignored", gh->group_name);
	}
	gh->master_flag |= M_IGNORE_G;
    } else {	/* was group ignored in GROUPS, but not active before? */
	if ((gh->master_flag & M_IGNORE_GROUP) == M_IGNORE_G) {
	    gh->master_flag &= ~M_NO_DIRECTORY;
	    log_entry('X', "Group %s activated", gh->group_name);
	}
	gh->master_flag &= ~M_IGNORE_G;
    }
    
    return 1;
}

static FILE *master_file = NULL;
static int db_sequential = 0;

#ifdef APOLLO_DOMAIN_OS
/* make copy of master file to CLIENT, but only once every 100 passes or */
/* if forced */
static make_master_copy(force_copy)
int force_copy;
{
  char client_path[FILENAME];
  static int pass_count = 0;
  int n;
    
  if (!force_copy) {	/* if not forced copy, only do copy every %100 passes */
      pass_count++;
      if (pass_count < 100) {
          return;
      }
  }
  pass_count = 0;
    
  strcpy(client_path,relative(db_directory, "CLIENT"));
  unlink(client_path);
  if ((n = copy_file(relative(db_directory, "MASTER"), client_path, 0)) < 0)
      log_entry('R', "Copy of MASTER to CLIENT failed (err=%d)", n);
}
#endif
#endif /* NOV */

#ifdef NOV
/*
 *	Init the groups data from active file.
 */
void
open_master(mode)
int	mode;
{
    register group_header *gh;

    freeobj(sorted_groups);
    freeobj(active_groups);
    active_groups = NULL;
    sorted_groups = NULL;

    readactfile();		/* Read in the active file - count groups */
    readtimfile();		/* Read the newsgroup creation time file */

    db_expand_master();		/* uses count from readact() call! */
  
    Loop_Groups_Header(gh) {
        /* db_read_group opens a file per call; use db_init_group instead */
        db_init_group(gh, l_g_index);
    }

    sort_groups();
}

#else /* NOV */

/*
 *	Open master & group files; read then in if first open.
 */

void
open_master(mode)
int mode;
{
    register group_header *gh;
    int trust_master;

    close_master();

#ifdef APOLLO_DOMAIN_OS
    if (who_am_i != I_AM_MASTER && who_am_i != I_AM_ADMIN)
	master_file = open_file(relative(db_directory, "CLIENT"), mode|MUST_EXIST);
    else
#endif
    master_file = open_file(relative(db_directory, "MASTER"), mode|MUST_EXIST);

    db_sequential = 0;
    if (mode == OPEN_CREATE) db_sequential = 1;

    if (mode != OPEN_READ) return;

    db_read_master();

    if (who_am_i != I_AM_MASTER && master.db_lock[0])
	nn_exitmsg(88, "DATABASE LOCKED.\n%s\n", master.db_lock);

    freeobj(sorted_groups);
#ifdef MALLOC_64K_LIMITATION
    if (active_groups)
	Loop_Groups_Header(gh) freeobj(gh);
#endif
    freeobj(active_groups);
    active_groups = NULL;
    sorted_groups = NULL;

    db_expand_master();

    open_groups(OPEN_READ|MUST_EXIST);

    trust_master = (who_am_i != I_AM_MASTER || !reread_groups_file);

    db_sequential = 1;
#ifdef MALLOC_64K_LIMITATION
    Loop_Groups_Number(l_g_index) {
	gh = newobj(group_header, 1);
	active_groups[l_g_index] = gh;
#else
    Loop_Groups_Header(gh) {
#endif
	gh->group_num = l_g_index;
	db_read_group(gh);
	db_parse_group(gh, trust_master);
    }
    db_sequential = 0;

    close_groups();

    sort_groups();
}
#endif /* NOV */

void
db_expand_master()
{
    master.free_groups = 20;

#ifndef NOV
#ifdef MALLOC_64K_LIMITATION
    active_groups = resizeobj(active_groups, group_header *,
			      master.number_of_groups + master.free_groups);
#else
    active_groups = resizeobj(active_groups, group_header,
			      master.number_of_groups + master.free_groups);
    clearobj(active_groups + master.number_of_groups, group_header,
	     master.free_groups);
#endif /* MALLOC_64K_LIMITATION */
#else
    active_groups = resizeobj(active_groups, group_header,
			      master.number_of_groups + master.free_groups);
    clearobj(active_groups + master.number_of_groups, group_header,
	     master.free_groups);
#endif /* NOV */
    sorted_groups = resizeobj(sorted_groups, group_header * , master.number_of_groups + master.free_groups);
}

#ifdef NOV
static void
freeup(key, data, hook)
char *key, *data, *hook;
{
    hashdelete((HASHTABLE *)data, key);
    free(key);
    free(data);
}
#endif

void
close_master()
{
#ifdef NOV
#if 0
    if (ngovp) {
	novclose(ngovp);
	ngovp = NULL;
    }
    if (acttbl != NULL)
	hashwalk(acttbl, freeup, (char *)acttbl);
    if (grouptbl != NULL)
	hashwalk(grouptbl, freeup, (char *)grouptbl);
    if (timtbl != NULL)
	hashwalk(timtbl, freeup, (char *)timtbl);
#endif
	
#else /* NOV */
    if (master_file != NULL) {
	fclose(master_file);
	master_file = NULL;

#ifdef APOLLO_DOMAIN_OS
	if (who_am_i == I_AM_MASTER) make_master_copy(1);
#endif
    }
#endif /* NOV */
}

int
update_group(gh)
group_header *gh;
{
#ifndef NOV
    group_number numg;

    numg = master.number_of_groups;

    db_read_master();
    master.number_of_groups = numg;
    if (master.db_lock[0]) return -3;
#endif /* NOV */

#ifndef NOV
    db_read_group(gh);
#else
    db_read_group(gh, gh->first_a_article, gh->last_a_article);	/* XXX */
#endif

    if (gh->master_flag & M_IGNORE_GROUP) return 0;
    if (gh->master_flag & M_BLOCKED) return -1;

    return 1;
}


static int
sort_gh(g1, g2)
group_header **g1, **g2;
{
    return strcmp((*g1)->group_name, (*g2)->group_name);
}


void
sort_groups()
{
    register group_header *gh;

    Loop_Groups_Header(gh)
	sorted_groups[l_g_index] = gh;

    quicksort(sorted_groups, master.number_of_groups, group_header *, sort_gh);

    s_g_first = 0;
    Loop_Groups_Sorted(gh)
	if (gh->group_name[0] != NUL) {
	    s_g_first = l_g_index;
	    break;
	}
}


group_header *lookup_no_alias(name)
char *name;
{
    register i, j, k, t;

    i = s_g_first;
    j = master.number_of_groups - 1;

    while (i <= j) {
	k = (i + j) / 2;

	if ( (t=strcmp(name, sorted_groups[k]->group_name)) > 0)
	    i = k+1;
	else
	if (t < 0)
	    j = k-1;
	else
	    return sorted_groups[k];
    }

    return NULL;
}

group_header *lookup(name)
char *name;
{
    register group_header *gh;
    group_header *gh_na;
    register int32 n, x;

    gh = lookup_no_alias(name);
    if (gh == NULL || (gh->master_flag & M_ALIASED) == 0) return gh;

    gh_na = gh;
    x = 16;
    do {
	if (--x == 0) {
	    log_entry('R', "Possible alias loop: %s", name);
	    return gh_na;
	}
	n = (int32)gh->data_write_offset;
	/* if alias info is unreliable, return original group
	   which will be ignored anyway */
	if (n < 0 || n >= master.number_of_groups) {
	    log_entry('R', "Bad aliasing of %s -> %d", gh->group_name, n);
	    return gh_na;
	}
	gh = ACTIVE_GROUP(n);
    } while (gh->master_flag & M_ALIASED);

    return gh;
}

int
art_collected(gh, art_num)
group_header *gh;
article_number art_num;
{
    return gh->first_db_article <= art_num && gh->last_db_article >= art_num;
}

#ifndef NOV

char *db_data_path(namebuf, gh, d_or_x)
char *namebuf;
group_header *gh;
char d_or_x;
{
    register char *cp, *np;

    if (db_data_directory != NULL) {
#ifdef DB_LONG_NAMES
	sprintf(namebuf, "%s/%s.%c", db_data_directory, gh->group_name, d_or_x);
#else
	if (db_data_subdirs)
	    sprintf(namebuf, "%s/%d/%d.%c", db_data_directory, 
		    gh->group_num/100, gh->group_num, d_or_x);
	else
	sprintf(namebuf, "%s/%d.%c", db_data_directory, gh->group_num, d_or_x);
#endif
    } else {
	np = namebuf;
	/* master chdir to the group's directory */
	if (who_am_i != I_AM_MASTER) {
	    for (cp = news_directory; (*np = *cp++); np++);
	    *np++ = '/';
	    for (cp = gh->group_name; *cp; cp++)
		*np++ = *cp == '.' ? '/' : *cp;
	    *np++ = '/';
	}

	*np++ = '.';
	*np++ = 'n';
	*np++ = 'n';
	*np++ = d_or_x;
	*np++ = NUL;
    }

    return namebuf;
}

/* STUB */
FILE *open_data_file(gh, d_or_x, mode)
group_header *gh;
char d_or_x;
int mode;
{
    FILE *f;
    char data_file[FILENAME];

    db_data_path(data_file, gh, d_or_x);

    if (mode == -1) {
	if (unlink(data_file) < 0 && errno != ENOTDIR && errno != ENOENT)
	    log_entry('E', "Cannot unlink %s (errno=%d)", data_file, errno);
	f = NULL;
    } else {
     again:
	f = open_file(data_file, (mode & ~MUST_EXIST));
	if (f != NULL) return f;
#ifndef DB_LONG_NAMES
	if (db_data_subdirs && (mode&0xf) == OPEN_CREATE && errno == ENOENT) {
	    char *s;
	    s = strrchr(data_file, '/');
	    *s = NUL;
	    if (!file_exist(data_file, "dx")) {
		if (mkdir(data_file, 0755) < 0)
		    sys_error("Cannot create directory %s", data_file);
		log_entry('C', "Created directory %s", data_file);
		*s = '/';
		goto again;
	    }
	    *s = '/';
	    errno = ENOENT;
	}
#endif
	if (mode & MUST_EXIST)
	    sys_error("%s (%d): cannot open '%c' file (mode=%x, errno=%d)",
		      gh->group_name, (int)(gh->group_num), d_or_x,
		      mode, errno);
    }
    return f;
}

#ifdef NETWORK_DATABASE

#define MASTER_FIELDS	5	/* + DB_LOCK_MESSAGE bytes */
#define	GROUP_FIELDS	9
#define	ARTICLE_FIELDS	10


typedef int32 net_long;


#ifdef NETWORK_BYTE_ORDER

#define net_to_host(buf, n)
#define host_to_net(buf, n)

#else

static net_to_host(buf, lgt)
register net_long *buf;
int lgt;
{
    while (--lgt >= 0) {
	*buf = ntohl(*buf);
	buf++;
    }
}

static host_to_net(buf, lgt)
register net_long *buf;
int lgt;
{
    while (--lgt >= 0) {
	*buf = htonl(*buf);
	buf++;
    }
}

#endif /* not NETWORK_BYTE_ORDER */
#endif /* NETWORK_DATABASE */

#define NWDB_MAGIC 0x00190000	/* NN#n <-> NW#n */


void
db_read_master()
{
#ifdef NETWORK_DATABASE
    net_long buf[MASTER_FIELDS];

    rewind(master_file);
    if (fread((char *)buf, sizeof(net_long), MASTER_FIELDS, master_file)
	!= MASTER_FIELDS) goto err;
    if (fread(master.db_lock, sizeof(char), DB_LOCK_MESSAGE, master_file)
	!= DB_LOCK_MESSAGE) goto err;

    net_to_host(buf, MASTER_FIELDS);

    master.db_magic = buf[0] ^ NWDB_MAGIC;
    master.last_scan = buf[1];
    master.last_size = buf[2];
    master.number_of_groups = buf[3];
    master.db_created = buf[4];
#else
    rewind(master_file);
    if (fread((char *)&master, sizeof(master_header), 1, master_file) != 1)
	goto err;
#endif

    if (master.db_magic != NNDB_MAGIC)
	sys_error("Database magic number mismatch");
    return;

 err:
    sys_error("Incomplete MASTER file");
}

#endif	/* NOV */


#ifdef NOV


static char	*
grpnumtoname(num)
int	num;
{
    char acount[30];
    if (grouptbl == NULL)
        nn_exitmsg(50, "Oh no! I've lost the plot!\n");
    sprintf(acount, "%ld", num);
    return hashfetch(grouptbl, acount);
}

static void
setgrpname(gh)
register group_header *gh;
{
    if (gh->group_name == NULL) {
        /*
         * how bleedin' useful!
         * turn gh->group_num into gh->group_name.
         * remember to spoon-feed nn with gh->group_name_length.
         */
        gh->group_name = grpnumtoname(gh->group_num);
        if (gh->group_name == NULL) {  
	  nn_exitmsg(1, "nn: can't map group %d to name\n", gh->group_num);
        }
    }
    gh->group_name_length = strlen(gh->group_name);
}

static void
readactfile()
{
  register int	count = 0;
  char	actline[512];
  FILE *actfp;

  if (acttbl != NULL)
    return;
  acttbl = hashcreate(3000, (unsigned (*)())NULL);
  if (acttbl == NULL) {
    nn_exitmsg(1, "nn: can't create hash table\n");
  }

  grouptbl = hashcreate(3000, (unsigned (*)())NULL);
  if (grouptbl == NULL) {
    nn_exitmsg(1, "nn: can't create group table\n");
  }

#ifdef NNTP 
  if (use_nntp) {
    actfp = nntp_fopen_list("LIST");
  } else
#endif
    actfp = fopen(relative(news_lib_directory, "active"), "r");

  if (actfp == NULL) {
    nn_exitmsg(1, "could not fetch active file\n");
  }

#ifdef NNTP
  while ( use_nntp  ?  nntp_fgets(actline, sizeof actline)
		    :  fgets(actline, sizeof actline, actfp) )
#else
  while ( fgets(actline, sizeof actline, actfp) )
#endif
  {
    char	acount[30];
    char	*line = strsave(actline);
    char	*p = strchr(line, ' ');
    char	*g;

    if (p != NULL)
      *p = '\0';
    g = strsave(line);
    
    if (!hashstore(acttbl, strsave(line), line)) {
      nn_exitmsg(1, "nn: active hashstore failed\n");
    }
    (void) sprintf(acount, "%ld", count);
    if (!hashstore(grouptbl, strsave(acount),  g)) {
      nn_exitmsg(1, "nn: group hashstore failed\n");
    }
    if (p != NULL)
      *p = ' ';
    count++;
  }

  if (!use_nntp)
    (void) fclose(actfp);

  /* init the master struct */
  clearobj(&master, sizeof(master), 1);
  master.number_of_groups = count;

}

#endif /* NOV */



#ifndef NOV

void
db_write_master()
{
#ifdef NETWORK_DATABASE
    net_long buf[MASTER_FIELDS];

    buf[0] = master.db_magic ^ NWDB_MAGIC;
    buf[1] = master.last_scan;
    buf[2] = master.last_size;
    buf[3] = master.number_of_groups;
    buf[4] = master.db_created;

    host_to_net(buf, MASTER_FIELDS);
    rewind(master_file);
    if (fwrite((char *)buf, sizeof(net_long), MASTER_FIELDS, master_file)
	!= MASTER_FIELDS) goto err;
    if (fwrite(master.db_lock, sizeof(char), DB_LOCK_MESSAGE, master_file)
	!= DB_LOCK_MESSAGE) goto err;
#else
    rewind(master_file);
    if (fwrite((char *)&master, sizeof(master_header), 1, master_file) != 1)
	goto err;
#endif

    fflush(master_file);
#ifdef APOLLO_DOMAIN_OS
    if (who_am_i == I_AM_MASTER) make_master_copy(0);
#endif
    return;

 err:
    sys_error("Write to MASTER failed");
}


void
db_read_group(gh)
register group_header *gh;
{
#ifdef NETWORK_DATABASE
    net_long buf[GROUP_FIELDS];

    if (!db_sequential)
	fseek(master_file,
	      (off_t)(MASTER_FIELDS * sizeof(net_long) + DB_LOCK_MESSAGE +
	      GROUP_FIELDS * sizeof(net_long) * gh->group_num), 0);

    if (fread((char *)buf, sizeof(net_long), GROUP_FIELDS, master_file) != GROUP_FIELDS)
	goto err;

    net_to_host(buf, GROUP_FIELDS);

    gh->first_db_article = buf[0];
    gh->last_db_article = buf[1];
    gh->index_write_offset = buf[2];
    gh->data_write_offset = buf[3];
    gh->group_name_length = buf[4];
    gh->master_flag = buf[5];
    gh->first_a_article = buf[6];
    gh->last_a_article = buf[7];
    gh->creation_time = buf[8];
#else
    if (!db_sequential)
	fseek(master_file, 
	      (off_t)(sizeof(master_header) + SAVED_GROUP_HEADER_SIZE(*gh) * gh->group_num), 0);

    if (fread((char *)gh, SAVED_GROUP_HEADER_SIZE(*gh), 1, master_file) != 1)
	goto err;
#endif

    return;

 err:
    sys_error("Read GROUPS failed");
}


void
db_write_group(gh)
register group_header *gh;
{
#ifdef NETWORK_DATABASE
    net_long buf[GROUP_FIELDS];

    if (!db_sequential)
	fseek(master_file,
	      (off_t)(MASTER_FIELDS * sizeof(net_long) + DB_LOCK_MESSAGE +
	      GROUP_FIELDS * sizeof(net_long) * gh->group_num), 0);

    buf[0] = gh->first_db_article;
    buf[1] = gh->last_db_article;
    buf[2] = gh->index_write_offset;
    buf[3] = gh->data_write_offset;
    buf[4] = gh->group_name_length;
    buf[5] = gh->master_flag;
    buf[6] = gh->first_a_article;
    buf[7] = gh->last_a_article;
    buf[8] = gh->creation_time;

    host_to_net(buf, GROUP_FIELDS);
    if (fwrite((char *)buf, sizeof(net_long), GROUP_FIELDS, master_file) != GROUP_FIELDS)
	goto err;
#else
    if (!db_sequential)
	fseek(master_file, (off_t)(sizeof(master_header) + SAVED_GROUP_HEADER_SIZE(*gh) * gh->group_num), 0);


    if (fwrite((char *)gh, SAVED_GROUP_HEADER_SIZE(*gh), 1, master_file) != 1)
	goto err;
#endif

    fflush(master_file);

#ifdef APOLLO_DOMAIN_OS
    if (who_am_i == I_AM_MASTER) make_master_copy(0);
#endif
    return;

 err:
    sys_error("Write GROUPS failed");
}
#endif


#ifdef NOV
static void
readtimfile()
{
	char	timline[512];
	FILE	*timfp;

	if (timtbl != NULL)
		return;
	timtbl = hashcreate(500, (unsigned (*)())NULL);
	if (timtbl == NULL) {
		nn_exitmsg(1, "nn: can't create hash table\n");
	}

#ifdef NNTP
	if (use_nntp)
		timfp = nntp_fopen_list("LIST active.times");
	else
#endif
		timfp = fopen(relative(news_lib_directory, "active.times"), "r");

	if (timfp == NULL)
		return;		/* no great shakes if its missing */

	/* alt.fan.marla-thrift 736668095 netnews@ccc.amdahl.com */

#ifdef NNTP
	while ( use_nntp  ?  nntp_fgets(timline, sizeof timline)
			  :  fgets(timline, sizeof timline, timfp) )
#else
	while ( fgets(timline, sizeof timline, timfp) )
#endif
	{
		char	*line = strsave(timline);
		char	*p = strchr(line, ' ');

		if (p != NULL)
			*p = '\0';
		if (!hashstore(timtbl, strsave(line), line)) {
			nn_exitmsg(1, "nn: time hashstore failed\n");
		}
		if (p != NULL)
			*p = ' ';
	}
	if (!use_nntp)
		(void) fclose(timfp);

}
#endif /* NOV */



#ifndef NOV
off_t db_read_art(f)
FILE *f;
{
    off_t bytes;

#ifdef NETWORK_DATABASE
    net_long buf[ARTICLE_FIELDS];

    if (fread((char *)buf, sizeof(net_long), ARTICLE_FIELDS, f) != ARTICLE_FIELDS)
	return 0;
    bytes = sizeof(net_long) * ARTICLE_FIELDS;

    net_to_host(buf, ARTICLE_FIELDS);

    db_hdr.dh_number = buf[0];
    db_hdr.dh_date = buf[1];
    db_hdr.dh_hpos = buf[2];
    db_hdr.dh_lpos = buf[3];
    db_hdr.dh_fpos = buf[4];
    db_hdr.dh_lines = buf[5];
    db_hdr.dh_replies = buf[6];
    db_hdr.dh_cross_postings = buf[7];
    db_hdr.dh_subject_length = buf[8];
    db_hdr.dh_sender_length = buf[9];
#else
    if (fread((char *)&db_hdr, sizeof(data_header), 1, f) != 1) return 0;
    bytes = sizeof(data_header);
#endif

    if (db_hdr.dh_number < 0) {
	current_digest_article = db_hdr.dh_number = -db_hdr.dh_number;
	db_data.dh_type = DH_DIGEST_HEADER;
    } else
    if (db_hdr.dh_number == 0) {
	db_hdr.dh_number = current_digest_article;
	db_data.dh_type = DH_SUB_DIGEST;
    } else {
	current_digest_article = 0;
	db_data.dh_type = DH_NORMAL;
    }

    if (db_hdr.dh_cross_postings) {
    	if (fread((char *)db_data.dh_cross, sizeof(cross_post_number),
		  (int)db_hdr.dh_cross_postings, f)
	    != (int)db_hdr.dh_cross_postings) return -1;
	bytes += sizeof(cross_post_number) * (int)db_hdr.dh_cross_postings;
    }

    if (db_hdr.dh_sender_length) {
	if (fread(db_data.dh_sender, sizeof(char),
		  (int)db_hdr.dh_sender_length, f)
	    != db_hdr.dh_sender_length) return -1;
	bytes += sizeof(char) * (int)db_hdr.dh_sender_length;
    }
    db_data.dh_sender[db_hdr.dh_sender_length] = NUL;

    if (db_hdr.dh_subject_length) {
	if (fread(db_data.dh_subject, sizeof(char),
		  (int)db_hdr.dh_subject_length, f)
	    !=  db_hdr.dh_subject_length) return -1;
	bytes += sizeof(char) * (int)db_hdr.dh_subject_length;
    }
    db_data.dh_subject[db_hdr.dh_subject_length] = NUL;

    db_read_counter++;

    return bytes;
}
#endif NOV


#ifdef NOV
/*
 * initialise *gh; this is much cheaper than calling db_read_group.
 */
static void
db_init_group(gh, num)
register group_header *gh;
int	num;
{
	register char	*line;

	/* tidy up the struct */
	clearobj(gh, sizeof(struct group_header), 1);

	gh->group_num = num;
	setgrpname(gh);
	gh->master_flag = M_VALID;
	/* control.newgrp, etc are control groups */
	if (strncmp(gh->group_name, "control", 7) == 0)
		gh->master_flag |= M_CONTROL;
	/* these next two are subtle and we need to lie below */
	/* gh->first_db_article = 0;*/		/* lowest # in ov. data */
	/* gh->last_db_article = 0; */		/* highest # in ov. data */
	gh->first_a_article = 1; 		/* lowest # in active */
	/* gh->last_a_article = 0; */		/* highest number in active */
	/* gh->index_write_offset = 0; */	/* dunno */
	/* gh->data_write_offset = 0; */	/* dunno */

	/* set the creation time */
	gh->creation_time = 1;                  /* group creation date (~epoch) */
	line = hashfetch(timtbl, gh->group_name);
	if (line != NULL) {
		register char	*p = strchr(line, ' ');

		if (p != NULL)
			gh->creation_time = atol(p + 1);
	}

	line = hashfetch(acttbl, gh->group_name);
	if (line != NULL) {
		register char	*p = strchr(line, ' ');

		if (p == NULL)
			return;
		p++;
		gh->last_a_article = atol(p);
		p = strchr(p, ' ');
		if (p == NULL)
			return;
		p++;
		gh->first_a_article = atol(p);
	}
	gh->first_db_article = gh->first_a_article; /* lowest # in ov. data */
	gh->last_db_article = gh->last_a_article; /* highest # in ov. data */
}


/*
 * slurp up the overview data for this group into *gh.
 * this costs a file open and so should not be done frivolously.
 */
void
db_read_group(gh, first, last)
register group_header *gh;
article_number first, last;
{
	register struct novart *artp, *lastartp;

	/* db_init_group(gh, group_num?? );  already done early at init time */

	if (ngovp != NULL)
		novclose(ngovp);		/* trash last group's data */

#ifdef NNTP
	if (use_nntp) {
		ngovp = nntp_get_overview(gh, first, last);
	} else
#endif
		ngovp = novopen(gh->group_name);

	if (ngovp == NULL) {
		printf("no overview data for group `%s'\n", gh->group_name);
		return;
	}
	allarts = novall(ngovp);
	if (allarts == NULL) {
		/*
		   printf("overview data inaccessible for group `%s'\n",
			   gh->group_name);
		 */
		return;
	}
	if (!use_nntp || first == gh->first_a_article)
		gh->first_db_article = atol(allarts->a_num); /* lowest # */

	if (!use_nntp || last == gh->last_a_article) {

		for (artp = allarts; artp != NULL; artp = artp->a_nxtnum)
			lastartp = artp;

		gh->last_db_article  = atol(lastartp->a_num); /* highest # */
	}
}


/*
 * fill in db_hdr and db_data from the overview data for the next
 * article in this group.  does weirdo nn encodings of header fields.
 */
off_t
db_read_art(f)
FILE *f;
{
	register data_header *dhp = &db_hdr;
	register data_dynamic_data *ddp = &db_data;
	register struct novart *artp;
	int	recnt = 0;

	if (ngovp == NULL || ngovp->g_first == NULL)
		return 0;
	if (ngovp->g_first == NULL)		/* XXX */
		return 0;
	artp = novnext(ngovp);
	if (artp == NULL)
		return 0;			/* group exhausted */

	dhp->dh_number = atol(artp->a_num);
	/* printf("article #%ld\n", dhp->dh_number); */	/* DEBUG */
	dhp->dh_date = pack_date(artp->a_date);	/* "encoded Date: filed" */
	dhp->dh_hpos = 0;			/* 1st hdr byte */
	dhp->dh_lpos = 1L << 30;		/* last article byte */
	dhp->dh_fpos = 0;			/* 1st article text byte */
	dhp->dh_lines = -1;			/* -1 == "unknown" */
	if (isascii(artp->a_lines[0]) && isdigit(artp->a_lines[0]))
		dhp->dh_lines = atoi(artp->a_lines);
	dhp->dh_replies = 0;			/* # of References: */
	if (artp->a_refs != NULL) {
		register char	*p;

		for (p = artp->a_refs; *p != '\0'; p++)
			if (*p == '<')
				dhp->dh_replies++;
	}

	db_fixup_cross_postings(dhp, ddp, artp);

	if (dhp->dh_number < 0) {
		current_digest_article = dhp->dh_number = -dhp->dh_number;
		ddp->dh_type = DH_DIGEST_HEADER;
	} else if (dhp->dh_number == 0) {
		dhp->dh_number = current_digest_article;
		ddp->dh_type = DH_SUB_DIGEST;
	} else {
		current_digest_article = 0;
		ddp->dh_type = DH_NORMAL;
	}

	dhp->dh_sender_length =  pack_name(ddp->dh_sender, artp->a_from, NAME_LENGTH);
	dhp->dh_subject_length = pack_subject(ddp->dh_subject, artp->a_subj, &recnt, DBUF_SIZE);

	if (recnt)		/* 5/3/93 wolfgang@wsrcc.com */
		dhp->dh_replies |= 0x80;

	db_read_counter++;
	return 1;
}

static void
db_fixup_cross_postings(dhp, ddp, artp)
data_header *dhp;
data_dynamic_data *ddp;
struct novart *artp;
{
    char *curg, *tmp;
    int numgrps = 0;

    dhp->dh_cross_postings = 0; /* assume none as default until we can show 
				   otherwise */

    /* If no "other" header lines are in NOV database, we're out of luck,
       can only assume no crosspostings, so return. */
    if ((artp->a_others) == NULL) return;

    /* Scan until we find a Xref: header line. */
    for (curg = artp->a_others; ; ++curg) {
	if (strncmp("Xref: ", curg, 6) == 0 ||
	    strncmp("xref: ", curg, 6) == 0)
	{
	    break;
	}
	curg = strchr(curg, '\t'); /* Not this header, skip to the next */
	if (curg == NULL) return;
    }

    curg += 6;			   /* Skip over "Xref: " */

    while (*curg == ' ') ++curg;   /* Skip to the hostname field after Xref: */

    /* Skip over the hostname to the space following hostname */
    if ( (curg = strchr(curg, ' ')) == NULL ) {
	return;			   /* header is malformed, punt. */
    }
    /*
     * Start reading the entries one at a time.  Each entry is of the
     * form "newsgroup:number", and entries are separated by spaces.
     * Algorithm loosely based on the orignal one in collect.c for
     * setting up the crosspost information.
     */
    while (*curg == ' ' && numgrps < DBUF_SIZE) {
	group_header *gh;

	while (*curg == ' ') ++curg;	/* Skip spaces to the next entry */

	/* Zap colon at end of current entry. */
	for (tmp = curg ; ; ++tmp) {
	    if (*tmp == ':' || *tmp == '\0' || *tmp == '\t')
		break;
	}
	if (*tmp != ':') break;		/* malformed entry, punt. */
	*tmp = '\0';

	/* Find gh struct for the group. */
	if ( (gh = lookup(curg)) != NULL) {
	    /* and add group number to the crosspost list. */
	    ddp->dh_cross[numgrps++] = gh->group_num;
	}
	curg = tmp + 1;
	while (isdigit(*curg)) ++curg; /* Skip over the article number */
    }
    if (numgrps > 1) {
	/* Note: if # of groups is only 1, we leave dh_cross_postings 
	   at its original value of zero. */
	dhp->dh_cross_postings = numgrps;
    }
    return;
}
#endif /* NOV */


#ifndef NOV
int
db_write_art(f)
FILE *f;
{
#ifdef NETWORK_DATABASE
    net_long buf[ARTICLE_FIELDS];
#endif
    article_number art_num = db_hdr.dh_number;

    switch (db_data.dh_type) {
     case DH_NORMAL:
	break;
     case DH_SUB_DIGEST:
	db_hdr.dh_number = 0;
	break;
     case DH_DIGEST_HEADER:
	db_hdr.dh_number = -art_num;
	break;
    }

#ifdef NETWORK_DATABASE
    buf[0] = db_hdr.dh_number;
    buf[1] = db_hdr.dh_date;
    buf[2] = db_hdr.dh_hpos;
    buf[3] = db_hdr.dh_lpos;
    buf[4] = db_hdr.dh_fpos;
    buf[5] = db_hdr.dh_lines;
    buf[6] = db_hdr.dh_replies;
    buf[7] = db_hdr.dh_cross_postings;
    buf[8] = db_hdr.dh_subject_length;
    buf[9] = db_hdr.dh_sender_length;

    host_to_net(buf, ARTICLE_FIELDS);

    if (fwrite((char *)buf, sizeof(net_long), ARTICLE_FIELDS, f) != ARTICLE_FIELDS)
	return -1;
#else

    if (fwrite((char *)&db_hdr, sizeof(data_header), 1, f) != 1)
	return -1;

#endif
    if (db_hdr.dh_cross_postings)
    	if (fwrite((char *)db_data.dh_cross, sizeof(cross_post_number),
		  (int)db_hdr.dh_cross_postings, f)
	    != (int)db_hdr.dh_cross_postings) return -1;

    if (db_hdr.dh_sender_length)
	if (fwrite(db_data.dh_sender, sizeof(char),
		  (int)db_hdr.dh_sender_length, f)
	    != db_hdr.dh_sender_length) return -1;

    if (db_hdr.dh_subject_length)
	if (fwrite(db_data.dh_subject, sizeof(char),
		  (int)db_hdr.dh_subject_length, f)
	    != db_hdr.dh_subject_length) return -1;

    db_hdr.dh_number = art_num;

    db_write_counter++;

    return 1;
}


off_t get_index_offset(gh, art_num)
group_header *gh;
article_number art_num;
{
#ifdef NETWORK_DATABASE
    return (off_t)((art_num - gh->first_db_article) * sizeof(net_long));
#else
    return (off_t)((art_num - gh->first_db_article) * sizeof(off_t));
#endif
}

off_t get_data_offset(gh, art_num)
group_header *gh;
article_number art_num;
{
    FILE *index;
    off_t data_offset;

    if (gh->first_db_article == art_num) return (off_t)0;

    index = open_data_file(gh, 'x', OPEN_READ);
    if (index == NULL) return (off_t)(-1);

    fseek(index, get_index_offset(gh, art_num), 0);
    if (!db_read_offset(index, &data_offset))
	data_offset = (off_t)(-1);

    fclose(index);

    return data_offset;
}


int
db_read_offset(f, offset)
FILE *f;
off_t *offset;
{
#ifdef NETWORK_DATABASE
    net_long temp;

    if (fread((char *)&temp, sizeof(net_long), 1, f) != 1) return 0;

#ifndef NETWORK_BYTE_ORDER
    temp = ntohl(temp);
#endif
    *offset = temp;
#else

    if (fread((char *)offset, sizeof(off_t), 1, f) != 1) return 0;
#endif
    return 1;
}

int
db_write_offset(f, offset)
FILE *f;
off_t *offset;
{
#ifdef NETWORK_DATABASE
    net_long temp;

    temp = *offset;

#ifndef NETWORK_BYTE_ORDER
    temp = htonl(temp);
#endif
    if (fwrite((char *)&temp, sizeof(net_long), 1, f) != 1) return 0;

#else

    if (fwrite((char *)offset, sizeof(off_t), 1, f) != 1) return 0;
#endif
    return 1;
}

#endif NOV


#ifdef NOV

/* These are strictly temporary.  They will go away. */
char *
db_data_path(namebuf, gh, d_or_x)
char *namebuf;
group_header *gh;
int d_or_x;
{
  nn_exitmsg(50, "STUB ROUTINE CALLED: db_data_path\n");
  return NULL;
}

int
db_read_offset(f, offset)
FILE *f;
off_t *offset;
{
  nn_exitmsg(50, "STUB ROUTINE CALLED: db_read_offset\n");
  return -1;
}

void
db_write_group(gh)
group_header *gh;
{
  nn_exitmsg(50, "STUB ROUTINE CALLED: db_write_group\n");
  return;
}

FILE *
open_data_file(gh, d_or_x, mode)
group_header *gh;
int d_or_x;
int mode;
{
  nn_exitmsg(50, "STUB ROUTINE CALLED: open_data_dile\n");
  return NULL;
}

off_t
get_index_offset(gh, art_num)
group_header *gh;
article_number art_num;
{
  nn_exitmsg(50, "STUB ROUTINE CALLED: get_index_offset\n");
  return -1;
}
#endif /* NOV */
