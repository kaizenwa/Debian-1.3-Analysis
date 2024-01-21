/* check.c  -  Check and repair a PC/MS-DOS file system */

/* Written 1993 by Werner Almesberger */


#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <time.h>

#include "common.h"
#include "dosfsck.h"
#include "io.h"
#include "fat.h"
#include "file.h"
#include "check.h"


static DOS_FILE *root;


#define MODIFY(p,i,v) \
  ((p->dir_ent.i = v), \
  fs_write(p->offset+((char *) &p->dir_ent.i-(char *) &p->dir_ent), \
   sizeof(p->dir_ent.i),&p->dir_ent.i))


static char *path_name(DOS_FILE *file)
{
    static char path[PATH_MAX*2];

    if (!file) *path = 0;
    else {
	if (strlen(path_name(file->parent)) > PATH_MAX)
	    die("Path name too long.");
	strcat(path,"/");
	strcpy(strrchr(path,0),file_name(file->dir_ent.name));
    }
    return path;
}


static int day_n[] = { 0,31,59,90,120,151,181,212,243,273,304,334,0,0,0,0 };
		  /* JanFebMarApr May Jun Jul Aug Sep Oct Nov Dec */


/* Convert a MS-DOS time/date pair to a UNIX date (seconds since 1 1 70). */

int date_dos2unix(unsigned short time,unsigned short date)
{
    int month,year,secs;

    month = ((date >> 5) & 15)-1;
    year = date >> 9;
    secs = (time & 31)*2+60*((time >> 5) & 63)+(time >> 11)*3600+86400*
      ((date & 31)-1+day_n[month]+(year/4)+year*365-((year & 3) == 0 &&
      month < 2 ? 1 : 0)+3653);
                       /* days since 1.1.70 plus 80's leap day */
    return secs;
}


static char *file_stat(DOS_FILE *file)
{
    static char temp[100];
    struct tm *tm;
    char tmp[100];
    int date;

    date = date_dos2unix(CF_LE_W(file->dir_ent.time),CF_LE_W(file->
      dir_ent.date));
    tm = localtime(&date);
    strftime(tmp,99,"%H:%M:%S %b %d %Y",tm);
    sprintf(temp,"  Size %u bytes, date %s",CF_LE_L(file->dir_ent.size),tmp);
    return temp;
}


static int bad_name(unsigned char *name)
{
    int bad,i,spc;

    bad = 0;
    for (i = 0; i < MSDOS_NAME; i++) {
	if (name[i] < ' ' || name[i] == 0x7f) bad += 2;
	else if (name[i] > 0x7f) bad++;
	    else if (strchr("*?<>|\"\\/",name[i])) bad += 2;
    }
    spc = 0;
    for (i = 0; i < 8; i++)
	if (name[i] == ' ') spc++;
	else {
	    bad += spc;
	    spc = 0;
	}
    spc = 0;
    for (i = 8; i < 11; i++)
	if (name[i] == ' ') spc++;
	else {
	    bad += spc;
	    spc = 0;
	}
    return bad > 5;
}


static void drop_file(DOS_FS *fs,DOS_FILE *file)
{
    int cluster;

    MODIFY(file,name[0],DELETED_FLAG);
    for (cluster = CF_LE_W(file->dir_ent.start); cluster > 0 && cluster <
      fs->clusters+2; cluster = next_cluster(fs,cluster))
	set_owner(fs,cluster,NULL);
}


static void truncate_file(DOS_FS *fs,DOS_FILE *file,int clusters)
{
    int walk,next,prev,deleting;

    walk = CF_LE_W(file->dir_ent.start);
    prev = 0;
    if (deleting = !clusters) MODIFY(file,start,CT_LE_L(0));
    while (walk > 0) {
	next = next_cluster(fs,walk);
	if (deleting) set_fat(fs,walk,0);
	else if (deleting = !--clusters) set_fat(fs,walk,-1);
	prev = walk;
	walk = next;
    }
}


static void auto_rename(DOS_FILE *file)
{
    DOS_FILE *first,*walk;
    int number;

    first = file->parent ? file->parent->first : root;
    number = 0;
    while (1) {
	sprintf(file->dir_ent.name,"FSCK%04d",number);
	strncpy(file->dir_ent.ext,"REN",3);
	for (walk = first; walk; walk = walk->next)
	    if (walk != file && !strncmp(walk->dir_ent.name,file->dir_ent.
	      name,MSDOS_NAME)) break;
	if (!walk) return;
	number++;
    }
    die("Can't generate a unique name.");
}


static void rename_file(DOS_FILE *file)
{
    unsigned char name[46];
    unsigned char *walk,*here;

    while (1) {
	printf("New name: ");
	fflush(stdout);
	if (fgets(name,45,stdin)) {
	    if (here = strchr(name,'\n')) *here = 0;
	    for (walk = strrchr(name,0); walk >= name && (*walk == ' ' ||
	      *walk == '\t'); walk--);
	    walk[1] = 0;
	    for (walk = name; *walk == ' ' || *walk == '\t'; walk++);
	    if (file_cvt(walk,file->dir_ent.name)) return;
	}
    }
}


static int handle_dot(DOS_FS *fs,DOS_FILE *file,int dots)
{
    char *name;

    name = strncmp(file->dir_ent.name,MSDOS_DOT,MSDOS_NAME) ? ".." : ".";
    if (!(file->dir_ent.attr & ATTR_DIR)) {
	printf("%s\n  Is a non-directory.\n",path_name(file));
	if (interactive)
	    printf("1) Drop it\n2) Auto-rename\n3) Rename\n"
	      "4) Convert to directory\n");
	else printf("  Auto-renaming it.\n");
	switch (interactive ? get_key("1234","?") : '2') {
	    case '1':
		drop_file(fs,file);
		return 1;
	    case '2':
		auto_rename(file);
		printf("  Renamed to %s\n",file_name(file->dir_ent.name));
		return 0;
	    case '3':
		rename_file(file);
		return 0;
	    case '4':
		MODIFY(file,size,CT_LE_L(0));
		MODIFY(file,attr,file->dir_ent.attr | ATTR_DIR);
		break;
	}
    }
    if (!dots) {
	printf("Root contains directory \"%s\". Dropping it.\n",name);
	drop_file(fs,file);
	return 1;
    }
    return 0;
}


static int check_file(DOS_FS *fs,DOS_FILE *file)
{
    DOS_FILE *owner;
    int clusters,prev,curr,this,expect,restart;
    int walk,clusters2;

    if (file->dir_ent.attr & ATTR_DIR) {
	if (CF_LE_L(file->dir_ent.size)) {
	    printf("%s\n  Directory has non-zero size. Fixing it.\n",
	      path_name(file));
	    MODIFY(file,size,CT_LE_L(0));
	}
	if (file->parent && !strncmp(file->dir_ent.name,MSDOS_DOT,MSDOS_NAME)) {
	    expect = CF_LE_W(file->parent->dir_ent.start);
	    if (CF_LE_W(file->dir_ent.start) != expect) {
		printf("%s\n  Start (%d) does not point to parent (%d)\n",
		  path_name(file),CF_LE_W(file->dir_ent.start),expect);
		MODIFY(file,start,CT_LE_W(expect));
	    }
	    return 0;
	}
	if (file->parent && !strncmp(file->dir_ent.name,MSDOS_DOTDOT,
	  MSDOS_NAME)) {
	    expect = file->parent->parent ? CF_LE_W(file->parent->parent->
	      dir_ent.start) : 0;
	    if (CF_LE_W(file->dir_ent.start) != expect) {
		printf("%s\n  Start (%d) does not point to .. (%d)\n",
		  path_name(file),CF_LE_W(file->dir_ent.start),expect);
		MODIFY(file,start,CT_LE_W(expect));
	    }
	    return 0;
	}
    }
    if (CF_LE_W(file->dir_ent.start) >= fs->clusters+2) {
	printf("%s\n  Start cluster beyond limit (%u > %d). Truncating file.\n",
	  path_name(file),CF_LE_W(file->dir_ent.start),fs->clusters+1);
	MODIFY(file,start,CT_LE_W(0));
    }
    clusters = prev = 0;
    for (curr = CF_LE_W(file->dir_ent.start) ? CF_LE_W(file->dir_ent.start) :
      -1; curr != -1; curr = next_cluster(fs,curr)) {
	if (!fs->fat[curr].value || bad_cluster(fs,curr)) {
	    printf("%s\n  Contains a %s cluster (%d). Assuming EOF.\n",
	      path_name(file),fs->fat[curr].value ? "bad" : "free",curr);
	    if (prev) set_fat(fs,prev,-1);
	    else MODIFY(file,start,CT_LE_W(0));
	    break;
	}
	if (!(file->dir_ent.attr & ATTR_DIR) && CF_LE_L(file->dir_ent.size) <=
	  clusters*fs->cluster_size) {
	    printf("%s\n  File size is %u bytes, cluster chain length is > %u "
	      "bytes.\n  Truncating file to %u bytes.\n",path_name(file),
	      CF_LE_L(file->dir_ent.size),clusters*fs->cluster_size,
	      CF_LE_L(file->dir_ent.size));
	    truncate_file(fs,file,clusters);
	    break;
	}
	if (owner = get_owner(fs,curr)) {
	    printf("%s  and\n",path_name(owner));
	    printf("%s\n  share clusters.\n",path_name(file));
	    clusters2 = 0;
	    for (walk = CF_LE_W(owner->dir_ent.start); walk > 0; walk =
	      next_cluster(fs,walk))
		if (walk == curr) break;
		else clusters2++;
	    restart = file->dir_ent.attr & ATTR_DIR;
	    if (interactive)
		printf("1) Truncate first to %u bytes%s\n"
		  "2) Truncate second to %u bytes\n",clusters*fs->cluster_size,
		  restart ? " and restart" : "",clusters2*fs->cluster_size);
	    else printf("  Truncating second to %u bytes.\n",clusters2*
		  fs->cluster_size);
	    if (interactive && get_key("12","?") == '1') {
		prev = 0;
		clusters = 0;
		for (this = CF_LE_W(owner->dir_ent.start); this > 0; this =
		  next_cluster(fs,this)) {
		    if (this == curr) {
			if (prev) set_fat(fs,prev,-1);
			else MODIFY(owner,start,CT_LE_W(0));
			MODIFY(owner,size,CT_LE_L(clusters*fs->cluster_size));
			if (restart) return 1;
			while (this > 0) {
			    set_owner(fs,this,NULL);
			    this = next_cluster(fs,this);
			}
			break;
		    }
		    clusters++;
		    prev = this;
		}
		if (this != curr)
		    die("Internal error: didn't find cluster %d in chain"
		      " starting at %d",curr,CF_LE_W(owner->dir_ent.start));
	    }
	    else {
		if (prev) set_fat(fs,prev,-1);
		else MODIFY(file,start,CT_LE_W(0));
		break;
	    }
	}
	set_owner(fs,curr,file);
	clusters++;
	prev = curr;
    }
    if (!(file->dir_ent.attr & ATTR_DIR) && CF_LE_L(file->dir_ent.size) >
      clusters*fs->cluster_size) {
	printf("%s\n  File size is %u bytes, cluster chain length is %u bytes."
	  "\n  Truncating file to %u bytes.\n",path_name(file),CF_LE_L(file->
	  dir_ent.size),clusters*fs->cluster_size,clusters*fs->cluster_size);
	MODIFY(file,size,CT_LE_L(clusters*fs->cluster_size));
    }
    return 0;
}


static int check_files(DOS_FS *fs,DOS_FILE *start)
{
    while (start) {
	if (check_file(fs,start)) return 1;
	start = start->next;
    }
    return 0;
}


static int check_dir(DOS_FS *fs,DOS_FILE **root,int dots)
{
    DOS_FILE *parent,**walk,**scan;
    int dot,dotdot,skip,redo;
    int good,bad;

    if (!*root) return 0;
    parent = (*root)->parent;
    good = bad = 0;
    for (walk = root; *walk; walk = &(*walk)->next)
	if (bad_name((*walk)->dir_ent.name)) bad++;
	else good++;
    if (*root && parent && good+bad > 4 && bad > good/2) {
	printf("%s\n  Has a large number of bad entries. (%d/%d)\n",
	  path_name(parent),bad,good+bad);
	if (!interactive) printf("  Dropping it.\n");
	if (!interactive || get_key("yn","Drop directory ? (y/n)") == 'y') {
	    truncate_file(fs,parent,0);
	    MODIFY(parent,name[0],DELETED_FLAG);
	    /* buglet: deleted directory stays in the list. */
	    return 1;
	}
    }
    dot = dotdot = redo = 0;
    walk = root;
    while (*walk) {
	if (!strncmp((*walk)->dir_ent.name,MSDOS_DOT,MSDOS_NAME) ||
	  !strncmp((*walk)->dir_ent.name,MSDOS_DOTDOT,MSDOS_NAME)) {
	    if (handle_dot(fs,*walk,dots)) {
		*walk = (*walk)->next;
		continue;
	    }
	    if (!strncmp((*walk)->dir_ent.name,MSDOS_DOT,MSDOS_NAME)) dot++;
	    else dotdot++;
	}
	if (bad_name((*walk)->dir_ent.name)) {
	    printf("%s\n  Bad file name.\n",path_name(*walk));
	    if (interactive)
		printf("1) Drop file\n2) Rename file\n3) Auto-rename\n"
		  "4) Keep it\n");
	    else printf("  Auto-renaming it.\n");
	    switch (interactive ? get_key("1234","?") : '3') {
		case '1':
		    drop_file(fs,*walk);
		    walk = &(*walk)->next;
		    continue;
		case '2':
		    rename_file(*walk);
		    redo = 1;
		    break;
		case '3':
		    auto_rename(*walk);
		    printf("  Renamed to %s\n",file_name((*walk)->dir_ent.
		      name));
		    break;
		case '4':
		    break;
	    }
	}
	skip = 0;
	scan = &(*walk)->next;
	while (*scan && !skip) {
	    if (!strncmp((*walk)->dir_ent.name,(*scan)->dir_ent.name,MSDOS_NAME
	      )) {
		printf("%s\n  Duplicate directory entry.\n  First  %s\n",
		  path_name(*walk),file_stat(*walk));
		printf("  Second %s\n",file_stat(*scan));
		if (interactive)
		    printf("1) Drop first\n2) Drop second\n3) Rename first\n"
		      "4) Rename second\n5) Auto-rename first\n"
		      "6) Auto-rename second\n");
		else printf("  Auto-renaming second.\n");
		switch (interactive ? get_key("123456","?") : '6') {
		    case '1':
			drop_file(fs,*walk);
			*walk = (*walk)->next;
			skip = 1;
			break;
		    case '2':
			drop_file(fs,*scan);
			*scan = (*scan)->next;
			continue;
		    case '3':
			rename_file(*walk);
			printf("  Renamed to %s\n",path_name(*walk));
			redo = 1;
			break;
		    case '4':
			rename_file(*scan);
			printf("  Renamed to %s\n",path_name(*walk));
			redo = 1;
			break;
		    case '5':
			auto_rename(*walk);
			printf("  Renamed to %s\n",file_name((*walk)->dir_ent.
			  name));
			break;
		    case '6':
			auto_rename(*scan);
			printf("  Renamed to %s\n",file_name((*scan)->dir_ent.
			  name));
			break;
		}
	    }
	    scan = &(*scan)->next;
	}
	if (skip) continue;
	if (!redo) walk = &(*walk)->next;
	else {
	    walk = root;
	    dot = dotdot = redo = 0;
	}
    }
    if (dots && !dot)
	printf("%s\n  \".\" is missing. Can't fix this yet.\n",
	  path_name(parent));
    if (dots && !dotdot)
	printf("%s\n  \"..\" is missing. Can't fix this yet.\n",
	  path_name(parent));
    return 0;
}


static void test_file(DOS_FS *fs,DOS_FILE *file,int read_test)
{
    DOS_FILE *owner;
    int walk,prev,clusters;

    prev = clusters = 0;
    for (walk = CF_LE_W(file->dir_ent.start); walk > 0 && walk < fs->clusters+2;
      walk = next_cluster(fs,walk)) {
	if (owner = get_owner(fs,walk)) {
	    if (owner == file) {
		printf("%s\n  Circular cluster chain. Truncating to %d "
		  "cluster%s.\n",path_name(file),clusters,clusters == 1 ? "" :
		  "s");
		if (prev) set_fat(fs,prev,-1);
		else MODIFY(file,start,CT_LE_W(0));
	    }
	    break;
	}
	if (bad_cluster(fs,walk)) break;
	if (read_test)
	    if (fs_test(cluster_start(fs,walk),fs->cluster_size)) {
		prev = walk;
		clusters++;
	    }
	    else {
		printf("%s\n  Cluster %d (%d) is unreadable. Skipping it.\n",
		  path_name(file),clusters,walk);
		if (prev) set_fat(fs,prev,next_cluster(fs,walk));
		else MODIFY(file,start,CT_LE_W(next_cluster(fs,walk)));
		set_fat(fs,walk,-2);
	    }
	set_owner(fs,walk,file);
    }
    for (walk = CF_LE_W(file->dir_ent.start); walk > 0 && walk < fs->clusters+2;
      walk = next_cluster(fs,walk))
	if (bad_cluster(fs,walk)) break;
	else if (get_owner(fs,walk) == file) set_owner(fs,walk,NULL);
	    else break;
}


static void undelete(DOS_FS *fs,DOS_FILE *file)
{
    int clusters,left,prev,walk;

    clusters = left = (CF_LE_L(file->dir_ent.size)+fs->cluster_size-1)/
      fs->cluster_size;
    prev = 0;
    for (walk = CF_LE_W(file->dir_ent.start); left && walk >= 2 && walk <
       fs->clusters+2 && !fs->fat[walk].value; walk++) {
	left--;
	if (prev) set_fat(fs,prev,walk);
	prev = walk;
    }
    if (prev) set_fat(fs,prev,-1);
    else MODIFY(file,start,CT_LE_W(0));
    if (left)
	printf("Warning: Did only undelete %d of %d cluster%s.\n",clusters-left,
	  clusters,clusters == 1 ? "" : "s");
   
}


static void add_file(DOS_FS *fs,DOS_FILE ***chain,DOS_FILE *parent,int offset,
  FDSC **cp)
{
    DOS_FILE *new;
    DIR_ENT de;
    FD_TYPE type;

    fs_read(offset,sizeof(DIR_ENT),&de);
    if ((type = file_type(cp,de.name)) != fdt_none) {
	if (type == fdt_undelete && (de.attr & ATTR_DIR))
	    die("Can't undelete directories.");
	file_modify(cp,de.name);
	fs_write(offset,1,&de);
    }
    if (IS_FREE(de.name)) return;
    new = qalloc(&mem_queue,sizeof(DOS_FILE));
    new->offset = offset;
    memcpy(&new->dir_ent,&de,sizeof(de));
    new->next = new->first = NULL;
    new->parent = parent;
    if (type == fdt_undelete) undelete(fs,new);
    **chain = new;
    *chain = &new->next;
    if (list) printf("Checking file %s\n",path_name(new));
    test_file(fs,new,test);
}


static int subdirs(DOS_FS *fs,DOS_FILE *parent,FDSC **cp);


static int scan_dir(DOS_FS *fs,DOS_FILE *this,FDSC **cp)
{
    DOS_FILE **chain;
    int i,clu_num;

    chain = &this->first;
    i = 0;
    clu_num = CF_LE_W(this->dir_ent.start);
    while (clu_num > 0) {
	add_file(fs,&chain,this,cluster_start(fs,clu_num)+(i % fs->
	  cluster_size),cp);
	i += sizeof(DIR_ENT);
	if (!(i % fs->cluster_size))
	    if ((clu_num = next_cluster(fs,clu_num)) <= 0) break;
    }
    if (check_dir(fs,&this->first,1)) return 0;
    if (check_files(fs,this->first)) return 1;
    return subdirs(fs,this,cp);
}


static int subdirs(DOS_FS *fs,DOS_FILE *parent,FDSC **cp)
{
    DOS_FILE *walk;

    for (walk = parent ? parent->first : root; walk; walk = walk->next)
	if (walk->dir_ent.attr & ATTR_DIR)
	    if (strncmp(walk->dir_ent.name,MSDOS_DOT,MSDOS_NAME) &&
	      strncmp(walk->dir_ent.name,MSDOS_DOTDOT,MSDOS_NAME))
		if (scan_dir(fs,walk,file_cd(cp,walk->dir_ent.name))) return 1;
    return 0;
}


int scan_root(DOS_FS *fs)
{
    DOS_FILE **chain;
    int i;

    root = NULL;
    chain = &root;
    for (i = 0; i < fs->root_entries; i++)
	add_file(fs,&chain,NULL,fs->root_start+i*sizeof(DIR_ENT),&fp_root);
    (void) check_dir(fs,&root,0);
    if (check_files(fs,root)) return 1;
    return subdirs(fs,NULL,&fp_root);
}
