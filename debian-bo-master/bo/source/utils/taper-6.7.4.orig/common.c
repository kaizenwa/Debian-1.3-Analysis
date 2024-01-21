/*
   Time-stamp: <96/07/19 20:12:32 yusuf>

   $Id: common.c,v 1.31 1996/07/27 20:42:06 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: common.c,v 1.31 1996/07/27 20:42:06 yusuf Exp $";
#endif /* lint */



#include "taper.h"

/* Color defaults */
/* The following fours routines keep a track of all ncurses windows opened
   and closed by me. All the currently opened windows are tracked, therefore,
   if we want to close all open windows, we can do so with one function call.
   A maximum of 20 windows can be opened simultaneously */

void paint_main() {
    my_werase(win_main, COLOR_MAIN); wrefresh(win_main);
}

void init_common_vars()
{
    no_vol_details = 0;
    no_in_archive = 0;
    no_sel = 0;
    in_dir = 0;
    total_selected = 0;
    total_compressed = 0;
    total_uncompressed = 0;
    dv = 0;
    log_errors = 0;
    log_warnings = 0;
    *cf = 0;
    memset(&tdh, 0, sizeof(tdh));
    memset(&ifd, 0, sizeof(ifd));
    len_archive_files = 0;
    len_sel_files = 0;
    cur_af_size = 0;
    cur_info_size = 0;
    cur_sf_size = 0;
    write_pid = 0;
    update_tsi = 0;
}


 char *name(struct oa_file_entry *ofe)
{
/* Returns the position of the name in ofe */
    char *s=(char *) ofe;
    
    return s+sizeof(struct oa_file_entry);
}


_errstat add_archive_file_engine(struct oa_file_entry **mem, _u32 *a_len,
			    struct oa_file_entry *ofe, char *name,
			    _s32 *block_len)
{
/* Adds an entry to the archive */
    
    _u32  len;
    char *s;
    struct oa_file_entry ofe1;
    
    ofe1 = *ofe;
    len = (memory_tight) ? *a_len + sizeof(struct oa_file_entry) + ofe->i.name_len 
                         : *a_len + sizeof(struct fixed_oa);
    
    if (len > *block_len) {			 /* need to allocate */
	*mem = my_realloc(*mem, *block_len+MEM_BLOCK_SIZE);   /* some more memory */
	if (*mem == NULL) {
	    *block_len = 0;
	    return do_exit(ERROR_MEMORY);
	}
	*block_len += MEM_BLOCK_SIZE;
    }
    s = (char *) (*mem); s += *a_len;
    memcpy(s, &ofe1, sizeof(struct oa_file_entry));
    s += sizeof(struct oa_file_entry);
    strcpy(s, name);
    *a_len = len;
    return 0;
}	


 _errstat add_archive_file(struct oa_file_entry *ofe, char *name)
{
/* Adds an entry to the archive */
    
    no_in_archive++;
    ofe->end_entry = END_ENTRY;
    return add_archive_file_engine(&archive_files, 
				   &len_archive_files, ofe, name,
				   &cur_af_size);
}	


 _errstat add_sel_file(struct oa_file_entry *ofe, char *name)
{
    return add_archive_file_engine(&sel_files, 
				   &len_sel_files, ofe, name,
				   &cur_sf_size);
}	




    
void advance_ofe(struct oa_file_entry **ofe)
{
    char *s=(char *) *ofe;
    
/* Advances ofe pointer to next entry in archive files */
    if (memory_tight) {
	s += sizeof(struct oa_file_entry);
	s += (*ofe)->i.name_len;
    }
    else
      s += sizeof(struct fixed_oa);
    *ofe = (struct oa_file_entry *) s;
}


struct oa_file_entry *find_entry(struct oa_file_entry *ofe, _s32 entry)
{
/* returns the position of the 'entry' in ofe */
    _s32 c=0;
    struct oa_file_entry *ofe1=ofe;
    char *s;

    if (memory_tight) {
	while (c<entry) {
	    advance_ofe(&ofe1);
	    c++;
	}
	return ofe1;
    }

    s = (char *) ofe;
    s = s + sizeof(struct fixed_oa) * entry;
    return (struct oa_file_entry *) s;
}

      
_errstat make_info_dirs(void)
{
/* Checks to see if info directory exists. If not, it is created 
 * 
 * Returns -1 for error
 *          1 for OK
*/
    
    struct stat buf;
    int  err;
    
    err = stat(taper_info_files, &buf);
    if (err == 0) 				 /* no problems opening */
      return (S_ISDIR(buf.st_mode)) ? 1 : do_exit(ERROR_INFO_ISNT_DIR);
    if (errno != ENOENT)
      return do_exit(ERROR_CREATING_INFO);
    return mkdir(taper_info_files, S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH);
}


void make_info_filename(char *info_file, _u32 archive_id)
{
    strcpy(info_file, taper_info_files);	 /* look for information file */
    if (info_file[strlen(info_file)-1] != '/')	 /* associated with this archive */
      strcat(info_file, "/");
    sprintf(info_file, "%staper_info.%u", info_file, archive_id);
}


_errstat open_info_file(_s8 must_exist, _u32 archive_id)
{
    char    info_file[MAX_FNAME];
    int     fd;

    if (make_info_dirs() == -1) return -1;	 /* make directories */
    make_info_filename(info_file, archive_id);	 /* make info filename */
    
    if (log_level > 2) write_log("Trying to open info file");
    fd = open(info_file, O_RDWR);		 /* open information file */
    if ((fd == -1) && (must_exist))		 /* error if doesn't exist */
      return do_exit(ERROR_NO_INFO);		 /* and it must */

    if (fd == -1) {
	if (log_level > 2) write_log("Creating info file");
	fd = open(info_file, O_RDWR|O_CREAT, S_IWRITE|S_IREAD);
    }
    if (fd == -1)
      return do_exit(ERROR_CREATING_INFO);
    return fd;
}


_errstat make_cf(char *c, _s32 no_in_archive)
{
/* Works out common pathname prefix 
 * c points to first file entry in info file 
 
 * Uses the information in only_vol
 * 
   Returns the maximum path user can strip */
    
    _s32    cs;
    int     cf1;
    char    *lp, *src;
    int     maxstrip;

    c += sizeof(struct file_info);
    src = c;
    if (no_in_archive == 1) {
	strcpy(cf, "/");
	maxstrip = 1;
	return 0;
    }
    strcpy(cf, c);
    PAST_STRING_FI(c);
    for (cs=0; cs<no_in_archive-1; cs++) {	 
	if (only_vol) 				 /* we are volume restricted */
	    if (abs(((struct file_info *) c)->volume) != only_vol) {
		c += sizeof(struct file_info);	 /* doesn't match volume */
		PAST_STRING_FI(c);
		continue;
	    }
	c += sizeof(struct file_info);		 /* move to filename */
	lp = c; cf1 = 0;
	PAST_STRING_FI(c);
	while (*lp && cf[cf1]) {
	    if (*lp != cf[cf1]) {		 /* pathnames differ here */
		while (cf1) {
		    if (cf[cf1] == '/') 
		      break;
		    else
		      cf1--;
		}
		cf[cf1+1] = 0;			 /* terminate common here */
		break;
	    }
	    lp++; cf1++;
	} 
	if (!strcmp(cf, src)) {			 /* filenames are the same */
	    if (!*cf) continue;
	    if (cf[strlen(cf)-1] == '/')
	      cf[strlen(cf)-1] = 0;
	    if (strrchr(cf, '/'))
	      *strrchr(cf, '/') = 0;
	    else
	      *cf = 0;
	}
    }
    if (cf[strlen(cf)-1] != '/')
      strcat(cf, "/");
    maxstrip = 0;
    lp = cf;
    while (*lp) {
	if (*lp == '/') maxstrip++;
	lp++;
    }
    return maxstrip;
}


void make_stripped_cf(void)
{
    char *s;
    int  l;

    *stripped_cf = 0;
    if (!*cf) return;
    if (!lstrip) {
	*(cf+1) = 0;				 /* make cf '/' */
	strcpy(stripped_cf, cf);
	return;
    }

    s = cf;
    l=1;
    while (l<lstrip) {
	s++;
	while (*s && (*s != '/'))  s++;
	l++;
    }
    strcpy(stripped_cf, s+(*s != 0));
}


_errstat put_in_buf(char **cp, char *s, _s32 *cz)
{
/* Puts the string 's' into the buffer at 'cp'. cz is the running size of buffer */

    _s32 offset;

    offset = *cp - vol_details;
    *cz += strlen(s)+1;
    vol_details = my_realloc(vol_details, *cz);
    if (vol_details == NULL) return -1;
    *cp = vol_details + offset;
    strcpy(*cp, s);
    *cp += strlen(s)+1;
    no_vol_details++;
    return 0;
}


_errstat extract_vol_details(void)
{
/*  Puts the volume details in a buffer for use by on_vol window */

    _s32 c, c1, cz=0;
    char *cp, *xp, s[MAX_FNAME];
    struct volume_header vh;
    struct tm *t;
    _s32  xx;

    no_vol_details=0;
    cp = vol_details;
    xp = (char *) vol_headers;
    for (c=0; c<ifd.number_volumes; c++) {
	*s = 0;
	if (put_in_buf(&cp, s, &cz) == -1) return -1;
	memcpy(&vh, xp, sizeof(struct volume_header));/* to ensure aligned */
	xp += sizeof(struct volume_header);
	if (*vh.volume_title)
	  sprintf(s, "Volume %d %s", c+1, vh.volume_title);
	else
	  sprintf(s, "Volume %d <no title>", c+1);
	if (put_in_buf(&cp, s, &cz) == -1) return -1;
	if (vh.volume_magic == VOLUME_MAGIC_INFO) {
	    sprintf(s, "Contains an info file");
	    if (put_in_buf(&cp, s, &cz) == -1) return -1;
	}
	else {
	    sprintf(s, "Contains %d files.", vh.no_in_volume);
	    if (put_in_buf(&cp, s, &cz) == -1) return -1;
	}
	t = localtime(&vh.backup_time);
	sprintf(s, "Backed up at %d/%d/%d %d:%02d", t->tm_year, t->tm_mon+1, t->tm_mday,
		t->tm_hour, t->tm_min);
	if (put_in_buf(&cp, s, &cz) == -1) return -1;
	for (c1=0; c1<vh.no_sels; c1++) {
	    memcpy(&xx, xp, sizeof(_s32)); xp += sizeof(_s32); /* past count byte */
	    put_in_buf(&cp, xp, &cz);
	    xp += xx;				 /* past string */
	    memcpy(&xx, xp, sizeof(_s32)); xp += sizeof(_s32); /* pass count byte */
	    xp += xx;				 /* past filter */
	}
    }
    return 0;
}

	
_errstat get_statinfo(char *fn, struct stat *b)
{
/* Gets the stat info the filename fn. Returns -1 if error */
    
    if (hard_links) {
	if (stat(fn, b) == -1) 
	  if (errno == ENOENT) {
	      if (lstat(fn, b) == -1)
		return -1;
	  }
	  else
	    return -1;
    }
    else
      if (lstat(fn, b) == -1)
	return -1;
    return 0;
}


int fmr_cmp(const void *key, const void *rec)
{
#define ky ((struct fixed_oa *) key)
#define dr ((struct fixed_oa *) rec)
    int x;
    x = strcmp(ky->fn, dr->fn);
    if (x) return x;
    if (ky->f.i.volume == 0) return 0;
    if (abs(ky->f.i.volume) < abs(dr->f.i.volume)) return -1;
    if (abs(ky->f.i.volume) > abs(dr->f.i.volume)) return 1;
    return 0;
#undef ky
#undef dr
}
char *search_file(char *s, _s32 vol)
{
/* Looks for file 's' in archive - s should be full pathname
   starts search at 'starts' 
 
   returns pointer to entry of NULL 
   if vol == 0, then will return address of first
     entry with this name, otherwise looks for volume 

 
 * 
 */
    char *lcp;
    _s32 c=0;
    _s32 v;
    struct fixed_oa fa, *f;


    if ((memory_tight) || (!sort_dir)) {	 /* both cases, must search manually */
	lcp = (char *) archive_files;
	while (c<no_in_archive) {
	    v =  (memory_tight) ? ((struct oa_file_entry *) lcp)->i.volume :
	                          ((struct fixed_oa *) lcp)->f.i.volume;
	    lcp +=  (memory_tight) ? sizeof(struct oa_file_entry) :
	                             sizeof(struct fixed_oa);
	    if (!strcmp(lcp, &s[strlen(cf)])) {
		if ( (vol && (abs(v) == vol)) ||
		    !vol)
		  break;
	    }
	    if (memory_tight)  PAST_STRING_OAFE(lcp);
	    else lcp += sizeof(struct fixed_oa);
	    c++;
	}
	if (c==no_in_archive) return NULL;		 /* not in archive */
	lcp -=  (memory_tight) ? sizeof(struct oa_file_entry) :
	                         sizeof(struct fixed_oa);
	return lcp;
    }

/* Directory is sorted so can use a bsearch to find file */
    strcpy(fa.fn, &s[strlen(cf)]);
    fa.f.i.volume = vol;
    f = bsearch(&fa, archive_files, no_in_archive, sizeof(fa), fmr_cmp);
    if ((vol) || (f==NULL)) return (char *) f;	 /* found volume specific or not found*/
    while (f != (struct fixed_oa *) archive_files)	{/* look for first vol */
	if (strcmp((f-1)->fn, fa.fn)) 
	  return (char *) f;
	f--;
    }
    return (char *) f;
}


_errstat file_more_recent(char *s1, struct stat *b)
{
/* Looks to see if s1 (on the hard disk) is more recent than the file
   in the archive. If s1 is not on the archive, then returns 0 or if
   not as recent. s1 should be a full pathname

   If b == NULL, then reads in file info, otherwise, uses what is in b

   returns -1 if error
*/
    struct file_info  *fi;
    struct stat b1;
    
    if (b == NULL) {
	if (get_statinfo(s1, &b1) == -1)
	  return -1;
    }
    else
      b1 = *b;

    fi = (struct file_info *) search_file(s1, 0);
    if (fi == NULL) return 1;			 /* couldn't find */
    if (b1.st_mtime > fi->mtime)		 /* on disk is more recent */
      return 1;
    return 0;
}


_u8 make4len(char *s)
{
/* Returns the length of s rounded up to the next multiple of 8 bytes 
   I know the function is make4len (and not make8len), but this was initially
   set to 4 bytes (32 bits) for 32 bits machines and then changed for 8 bytes
   (64 bits) for the ALPHA */
    _u8 sl;
    
    sl = strlen(s) + 1  + sizeof(struct file_info);/* make whole thing a multiple of 8 bytes */
    sl = (sl%8) ? 8-sl%8 : 0;
    return strlen(s)+1+sl;
}


_errstat add_one_file_engine(WINDOW *mes_box, struct file_info *fi, char *name)
{
/* Returns 0 for OK, -1 for error     
 
   Assumes that cp is positioned at end of info file 

 * Also, will pad out strings to a length of a multiple of 4 bytes
*/
    _s32 pos;

    fi->name_len = make4len(name);
    ifd.info_file_size += sizeof(struct file_info) + fi->name_len;
    if (ifd.info_file_size > cur_info_size) {	 /* will exceed memory block */
	pos = cp - info;			 /* keep cp position */
	info = my_realloc(info, cur_info_size + MEM_BLOCK_SIZE);
	if (info == NULL) {
	    cur_info_size = 0;
	    return do_exit(ERROR_MEMORY);
	}
	cp = info + pos;
	cur_info_size += MEM_BLOCK_SIZE;
    }
    ifd.no_in_archive++;
    memcpy(cp, fi, sizeof(struct file_info));
    cp += sizeof(struct file_info);
    strcpy(cp, name); cp += fi->name_len;	 /* move pointer */
    return 0;
}


dev_t get_file_info(char *file, struct file_info *fi, _s8 chk, struct stat *ob)
{
/* Gets the file info for 'file'

   Returns 0 if error, otherwise returns device number on
 * which file is
 * 
 * If ob==NULL, gets the statinfo, otherwise uses what's at ob
*/
    
    struct stat b, borg;

    if (ob==NULL)  {
	if (get_statinfo(file, &b) == -1) {
	    if (chk) 
	      do_exit(ERROR_GETINFO);
	    return 0;
	}
    }
    else
      b = *ob;

    stat(file, &borg);				 /* get the original */
    fi->checksum = 0;
    fi->act_size = 0;
    fi->dev = b.st_rdev;
    fi->uid = b.st_uid;
    fi->gid = b.st_gid;
    fi->mode = b.st_mode;
    fi->org_mode = borg.st_mode;
    fi->size = b.st_size;
    fi->atime = b.st_atime;
    fi->mtime = b.st_mtime;
    fi->ctime = b.st_ctime;
    fi->backup_time = time(NULL);		 /* backup time */
    fi->name_len = 0;				 /* will be fixed by add_one_file_engine */
    fi->volume = ifd.number_volumes;		 /* show which volume it belongs to */
    fi->pos_in_archive = 0;			 /* initialise */
    return b.st_dev;
}


#define CP_INFO(x) ((struct file_info *) (x-sizeof(struct file_info)))
#define CP_VOL (abs(CP_INFO(lcp)->volume))
#define OA_INFO(x) ( ( (struct oa_file_entry *) (x-sizeof(struct oa_file_entry)) )->i)
#define OA_VOL (abs(OA_INFO(lcp).volume))
#define VOL ((in_cp) ? CP_VOL : OA_VOL)
#define BEG ((in_cp) ? info + sizeof(struct info_file_header) : (char *) archive_files)
#define ADV(x) x +=  (in_cp) ? sizeof(struct file_info) : sizeof(struct oa_file_entry)
struct s_dirs {
    char name[MAX_FNAME];			 /* must be at beginning */
    _s32  vol;					 /* for qsort */
    struct oa_file_entry *in_af;
};
int admd_cmp(void const *key, void const *s)
{
/* Search engine for bsearch called in add_missing dirs */
    struct s_dirs *d = (struct s_dirs *) s;
    struct s_dirs *k = (struct s_dirs *) key;
    int x;

    x = strcmp(k->name, d->name);
    if (x) return x;
/*    if (!k->vol) return 0; */
    if (abs(k->vol) == abs(d->vol))
      return 0;
    if (abs(k->vol) < abs(d->vol)) 
      return -1;
    return 1;
}


_errstat add_missing_dirs(WINDOW *mes, _s8 in_cp)
{
/* Goes through the archive looking for missing directories.

 * If for example, the user selected the files /x/y/z and /x/y/z1,
   and the common path is /x, directory y is missing so this routine
   would add the y directory.
 * 
 * If in_cp is TRUE, missing entries go into info
 *    in_cp is FALSE, missing entries go into archive_files. Also
 *          directory sizes are calculated
 * 
 * If adding to archive_files, volume is significant - ie.
 * directories must match in both volume & name; if adding
 * to info, then not significant
*/
    
    char *lcp, testf[MAX_FNAME], buf1[MAX_FNAME];
    _s32 c, no_dirs=0, offset, c2;
    struct file_info fi;
    struct oa_file_entry cfe, *old_af;
    struct s_dirs *dirs, *s, prev_buf, ss;
    mode_t mode;

    if (mes) 
      if (in_cp)
        status_box(mes, "Looking for missing directories", 1, FALSE, 1);
      else
        status_box(mes, "Calculating directory sizes", 1, FALSE, 1);
    lcp = BEG;
    if (in_cp)
	lcp = info + sizeof(struct info_file_header); /* start of files */
    dirs = my_malloc(1);
    if (dirs==NULL) return do_exit(ERROR_MEMORY);/* allocate memory for directory list */
    for (c=0; c<ifd.no_in_archive; c++) {	 /* loop through looking for directories */
	mode = (in_cp) ? ((struct file_info *) lcp)->mode : ((struct oa_file_entry *) lcp)->i.mode;
	ADV(lcp);
	if (S_ISDIR(mode)) {			 /* if directory, don't need to check */
	    dirs = my_realloc(dirs, (no_dirs+1)*sizeof(struct s_dirs));/* make room for another directory name */
	    if (dirs==NULL) return do_exit(ERROR_MEMORY);
	    s = dirs+no_dirs;
	    if (in_cp)
	      strcpy(s->name, &lcp[strlen(cf)]);
	    else
	      strcpy(s->name, lcp);
	    s->vol = VOL;
	    if (!in_cp) 
	      s->in_af = (struct oa_file_entry *) (lcp - sizeof(struct oa_file_entry));
	    if (s->name[strlen(s->name)-1] == '/') 
	      s->name[strlen(s->name)-1] = 0;   /* remove trailing '/' */
	    no_dirs++;
	}
	if (in_cp)
	  PAST_STRING_FI(lcp);
	else {
	    if (memory_tight)
	      PAST_STRING_OAFE(lcp);
	    else
	      lcp =lcp +  sizeof(struct fixed_oa) - sizeof(struct oa_file_entry);
	}
    }
    qsort(dirs, no_dirs, sizeof(struct s_dirs), (_s32 (*)(const void *, const void *)) strcmp);
    lcp = BEG;
    prev_buf.name[0] = 0;
    if (in_cp)
      cp = info + ifd.info_file_size;		 /* position cp at end */
    for (c=0; c<ifd.no_in_archive; c++) {	 /* loop through each file in archive */
	ADV(lcp);
	strcpy(testf, &lcp[(in_cp ? strlen(cf) : 0)]);
	if (testf[strlen(testf)-1] == '/') 	 /* remove trailing '/' */
	  testf[strlen(testf)-1] = 0;
	if (strrchr(testf, '/') != NULL)	 /* remove filename */
	  *(strrchr(testf, '/')) = 0;		 
	else
	  *testf = 0;
	if (!*testf) 				 /* was a file/dir in root */
	    goto next_file;
	if (*prev_buf.name)			 /* same as last one compared */
	  if ((!strcmp(prev_buf.name, testf))) 
	      if (in_cp) 			 /* if doing for info, ignore voluem */
		goto next_file;			 /* don't ignore for archive files */
	strcpy(prev_buf.name, testf);		 /* since we have to update directory sizes */
	prev_buf.vol = VOL;

	while (*testf) {
	    strcpy(ss.name, testf);
	    ss.vol = VOL;
	    s = bsearch(&ss, dirs, no_dirs, sizeof(struct s_dirs),
		    admd_cmp);
	    if (s != NULL) {			 /* found entry */
	      if (!in_cp)
		if (!S_ISDIR(OA_INFO(lcp).mode))/* don't add size of directory */
		  s->in_af->dirsize += OA_INFO(lcp).size;/* update size */
	    }
	    else {				 /* not found */
		if (in_cp) {
		    strcpy(buf1, cf);		 /* make full directory name */
		    if (buf1[strlen(cf)-1] != '/')
		      strcat(buf1, "/");
		    strcat(buf1, testf);
		    strcat(buf1, "/");
		    if (get_file_info(buf1, &fi, 1, NULL) == 0) {
			my_free(dirs);
			return -1;
		    }
		    fi.volume = -VOL;
		    offset = lcp - info;		 /* preserve cp counter */
		    if (add_one_file_engine(mes, &fi, buf1) == -1)
		      return -1;
		    lcp = info + offset;
		}
		else {				 /* going into archive_files */
		    cfe.i = (OA_INFO(lcp));
		    cfe.i.volume = -abs(cfe.i.volume);/* indicate it's a false entry */
		    cfe.i.mode = S_IFDIR|S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH;	 /* a directory */
		    cfe.i.pos_in_archive = 0;	 /* not in archive */
		    cfe.selected = 0;
		    cfe.in_archive_files = NULL;
		    cfe.incremental = 0;
		    cfe.dirsize = 0;
		    offset = len_archive_files;
		    old_af = archive_files;
		    strcpy(buf1, testf);
		    strcat(buf1, "/");
		    cfe.i.name_len = make4len(buf1);
		    if (add_archive_file(&cfe, buf1) == -1) {
			my_free(dirs);
			return -1;
		    }			
		    s = dirs;			 /* in case archive files moved in memory */
		    for (c2=0; c2<no_dirs; c2++) {  
			s->in_af = s->in_af - old_af + archive_files;
			s++;
		    }
		    lcp = lcp - (char *) old_af + (char *) archive_files;
		}
		dirs = my_realloc(dirs, (no_dirs+1)*
				  sizeof(struct s_dirs));/* make room for another directory name */
		if (dirs==NULL) return do_exit(ERROR_MEMORY);
		s = dirs+no_dirs;
		strcpy(s->name, testf); 
		s->vol = VOL;
		if (!cp) {
		    s->in_af = (struct oa_file_entry *) ((char *) archive_files + offset);
		    if (!S_ISDIR(OA_INFO(lcp).mode)) /* don't add size of directory */
		      s->in_af->dirsize += OA_INFO(lcp).size;	 /* update size */
		}
		no_dirs++;
		qsort(dirs, no_dirs, sizeof(struct s_dirs), (_s32 (*)(const void *, const void*)) strcmp);   /* resort after addition */
	    }
	    if (strrchr(testf, '/') != NULL)	 /* now take off directory */
	      *(strrchr(testf, '/')) = 0;
	    else
	      *testf = 0;
	}
	
	next_file:
	if (in_cp)
	  PAST_STRING_FI(lcp);
	else {
	    if (memory_tight)
	      PAST_STRING_OAFE(lcp);
	    else
	      lcp =lcp +  sizeof(struct fixed_oa) - sizeof(struct oa_file_entry);
	}
    }
    my_free(dirs);				 /* free up memory */
    return 0;
#undef CP_VOL
#undef CP_INFO
#undef OA_INFO
#undef OA_VOL
#undef VOL
}


int alpha_cmp_fe(const void *cmp1, const void *cmp2)
{
/* Sort order for files in volume directory:
 * 
 * alphabetical order then volume order
*/
    struct fixed_oa *ss1= (struct fixed_oa *) cmp1;
    struct fixed_oa *ss2= (struct fixed_oa *) cmp2;

    int x;
    
    x = strcmp(ss1->fn, ss2->fn);
    if (x) return x;
    if (abs(ss1->f.i.volume) > abs(ss2->f.i.volume))	 /* same name - compare by volume */
      return 1;
    if (abs(ss1->f.i.volume) < abs(ss2->f.i.volume))
      return -1;
    return 0;
}


int num_cmp_fe(const void *cmp1, const void *cmp2)
{
/* Sort order for files in volume directory:
 * 
 * alphabetical order then volume order
*/
    struct fixed_oa *ss1= (struct fixed_oa *) cmp1;
    struct fixed_oa *ss2= (struct fixed_oa *) cmp2;

    if (ss1->f.i.pos_in_archive < ss2->f.i.pos_in_archive)
      return -1;
    if (ss1->f.i.pos_in_archive > ss2->f.i.pos_in_archive)
      return 1;
    return 0;
}


_errstat do_sort_dir(int alpha)
{
/* Sorts the info file 
 * 
 * Basically makes a big memory block with fixed record sizes, copies
 * info file to it, sorts, then copies it back
 * 
 * If the -DMEMORY, then the fixed block is deleted and the files
 * copies back to archive files, otherwise, the variable length
 * archive_files is deleted 
 * 
 * if alpha == 1, sorts into alphabetical order, otherwise
 *   into pos_in_archive order
 * 
 * Returns -1 if error, 0 otherwise
*/
    char *fdir;
    struct fixed_oa *ss1;
    char *cs;
    _s32 c;	
    char *ofa;

    if (memory_tight) {
	fdir = my_malloc(no_in_archive*sizeof(struct fixed_oa));   /* copy archive files */
	if (fdir == NULL) return -1;		 /* to fixed length memory block */
	ofa=(char *) archive_files;
	ss1 = (struct fixed_oa *) fdir;
	for (c=0; c<no_in_archive;c++) {
	    memcpy(&ss1->f, ofa, sizeof(ss1->f));
	    strcpy(ss1->fn, name((struct oa_file_entry *) ofa));
	    advance_ofe((struct oa_file_entry **) &ofa);
	    ss1++;
	}
    }
    else
      fdir = (char *) archive_files;		 /* block - just sort */
    qsort(fdir, no_in_archive, sizeof(struct fixed_oa), 
	  (alpha) ? alpha_cmp_fe : num_cmp_fe);   /* do sort */

    if (memory_tight) {
	cs = (char *) archive_files;		 /* copy back to archive files */
	ss1 = (struct fixed_oa *) fdir;
	for (c=0; c<no_in_archive;c++) {
	    memcpy(cs, &ss1->f, sizeof(ss1->f));
	    cs += sizeof(ss1->f);
	    strcpy(cs, ss1->fn);
	    cs += ss1->f.i.name_len;
	    ss1++;
	}
	my_free(fdir);
    }
    return 0;
}

_errstat do_read_vol_dir(_u32 archive_id, char *tape, int mode, _s8 rvdir, _s8 must_exist) {
/* Closes the backup device after use

   If archive_id == -1, then tries to read the current tape for info file to read.
   If archive_id != -1, then reads the info file associated with this archive

 * If must_exist == FALSE, then if a taper archive is found, the user
 * if prompted whether s/he wishes to append or overwrite
 */    
    struct oa_file_entry *ofe;
    struct oa_file_entry cfe;
    char   *cp;
    _u32    c;
    WINDOW  *mes=NULL;
    int    fd, err, ofd=0;
    char   tmpf[MAX_FNAME];
    struct info_file_header ifd1;

    total_compressed = 0; total_uncompressed = 0;/* now extract info from info file */
    if (!no_windows)
      mes = status_box(mes, "Rewinding tape", 1, TRUE, 1);
    if (archive_id == -1) {
	err = get_tape_header(mes, 1, &tdh);
	tape_close();
	if (err == -1) {close_statusbox(mes); return -1;} 
	if ((err == TAPE_EXIST) & (!must_exist)) {
	    if (!no_windows) {
		append = message_box("Taper archive found", MB_APPENDOVERWRITE);
		touchwin(mes); wrefresh(mes);	 /* update screen */
	    }
	    rvdir = append;
		
	}
	if (!rvdir) {close_statusbox(mes); return err;}  /* don't want to read vol dir */
	if (err != TAPE_EXIST) {close_statusbox(mes); return err;}
    }
    else
      tdh.archive_id = archive_id;

    if (mes)
      mes = status_box(mes, "Constructing archive directory", 1, FALSE, 1);
    if ((fd = open_info_file(TRUE, tdh.archive_id)) == -1) {
	close_statusbox(mes);
	return -1;
    }
    read(fd, (char *) &ifd1, sizeof(struct info_file_header));
    ifd_endianize2mach(&ifd1, &ifd);
    if (ifd.archive_id != tdh.archive_id) {	 /* archive and info files */
	do_exit(ERROR_INFO_MISMATCH);
	goto err;
    }
    if ((ifd.magic != INFO_MAGIC) && (ifd.magic != INFO_MAGIC_COMPRESSED)) {
	if (ifd.magic == tdh.archive_id) 	 /* must be old format of info file */
	  do_exit(ERROR_INFO_OLD);
	else
	  do_exit(ERROR_INFO_MAGIC);
	goto err;
    }
    taper_tmpnam(tmpf);
    if (ifd.magic == INFO_MAGIC_COMPRESSED) {
	if (log_level > 2) write_log("Compressed info file - uncompressing");
	if (log_level > 2) write_log("Creating temp file to uncompress to");
	ofd = creat(tmpf, S_IREAD|S_IWRITE);
	if (ofd == -1) {do_exit(ERROR_COMPRESSING_INFO); goto err;}
	if (log_level > 2) write_log("Uncompress info file");
	read_u32(fd, &c);			 /* reads in size of compressed info file */
	if (unzip(fd, ofd) != 0) {
	    close(ofd); 
	    close(fd);
	    unlink(tmpf);
	    do_exit(ERROR_COMPRESSING_INFO);
	    goto err;
	}
	close(fd); close(ofd); fd = 0; ofd = 0;
	fd = open(tmpf, O_RDONLY);
	if (fd == -1) {
	    do_exit(ERROR_NO_INFO);
	    fd = 0;
	    goto err;
	}
    }
    no_in_archive = 0;
    info = my_realloc(info, ifd.info_file_size); /* allocate memory to read in whole information */
    if (!info) {				 /* can't allocate */
	do_exit(ERROR_MEMORY);
	goto err;
    }
    cur_info_size = ifd.info_file_size;
    if (log_level > 2) write_log("Reading in info file");
    memcpy(info, &ifd, sizeof(struct info_file_header));
    vol_headers = my_realloc(vol_headers, ifd.size_volume_headers);
    if (vol_headers == NULL) {
	do_exit(ERROR_MEMORY);
	goto err;
    }
    vt_info = my_realloc(vt_info, sizeof(struct volume_tape_info)*ifd.number_volumes);
    if (vt_info==NULL) {			 /* get block for volume/tape info */
	do_exit(ERROR_MEMORY);
	goto err;
    }
    if (ifd.number_tsi) {
	tsi = my_realloc(tsi, sizeof(struct tape_size_info) * ifd.number_tsi);
	if (tsi == NULL) {
	    do_exit(ERROR_MEMORY);
	    goto err;
	}
    }
    if (read_info_file(fd) == -1) goto err;
    close(fd);					 /* close info file */
    if (ifd.magic == INFO_MAGIC_COMPRESSED)	 /* remove temporary file */
      unlink(tmpf);
    fd = 0;
    cp = info + sizeof(struct info_file_header);
    
    lstrip = min(make_cf(cp, ifd.no_in_archive), ostrip);/* find common pathname */
    make_stripped_cf();				 /* work out what is the prefix to get right strip level */
    
    ofe = archive_files;
    len_archive_files = 0;
    cp = info + sizeof(struct info_file_header);
    for (c=0; c<ifd.no_in_archive; c++) {
	memcpy(&cfe.i, cp, sizeof(struct file_info)); cp += sizeof(struct file_info);
	cfe.selected = 0;
	cfe.dirsize = 0;
	total_compressed +=  sizeof(struct file_info) + cfe.i.name_len;
	total_uncompressed += sizeof(struct file_info) + cfe.i.name_len;
	if (S_ISREG(cfe.i.mode) || S_ISLNK(cfe.i.mode)) {
	    total_compressed += cfe.i.act_size;
	    total_uncompressed += cfe.i.size;
	}

	if (strlen(cf) < strlen(cp))		 /* only add if not excluded */
	  if (add_archive_file(&cfe, &cp[strlen(cf)]) == -1) goto err;
	PAST_STRING_FI(cp);
    }
    memcpy(info, &ifd, sizeof(struct info_file_header));
    no_in_archive = ifd.no_in_archive;
    if (add_missing_dirs(mes, FALSE) == -1) goto err; 
    if (mes)
      status_box(mes, "Sorting directory", 1, FALSE, 1);
    if (sort_dir) 
      do_sort_dir(1);

    if (mes)
      status_box(mes, "Extracting volume information", 1, FALSE, 1);
    if (extract_vol_details() == -1) goto err;
    
    if (mes) close_statusbox(mes);
    if (fd) close(fd);
    if (ofd) unlink(tmpf);
    return TAPE_EXIST;

    err:;
      if (mes) close_statusbox(mes);
      if (fd) close(fd);
      if (ofd) unlink(tmpf);
      return -1;
}


char *convtime(char *s, time_t t1, time_t t2)
{
    _s32   secs=t2-t1;
    int    hr,min,sc;
    
    hr=secs/3600;
    min=(secs-hr*3600)/60;
    sc=secs-hr*3600-min*60;
    sprintf(s,"%d:%02d:%02d",abs(hr),abs(min),abs(sc));
    return s;
}

_errstat check_device_names()
{
    char s[MAX_FNAME];
    
/* Converts the device name to a full pathname
   Also checks that block size is properly set */
    
    if (*ntape != '/') {
	sprintf(s, "%s/%s", original_cur_dir, ntape);
	strcpy(ntape, s);
    }
    if (*tape != '/') {
	sprintf(s, "%s/%s", original_cur_dir, tape);
	strcpy(tape, s);
    }
    if (block_size < 2048) {
	(no_windows) ? fprintf(stderr, "Block size is too low. Minimum of 2K needed") :
	               message_box("Block size is too low. Minimum of 2K needed", MB_OK);
	return -1;
    }
    if (block_size > DOUBLE_BUFFER_SIZE) {
	(no_windows) ? fprintf(stderr, "Block size can't exceed DOUBLE_BUFFER size") :
	               message_box("Block size can't exceed DOUBLE_BUFFER size", MB_OK);
	return -1;
	
    }
    return 0;
}


_s32 calc_checksum(int fd)
{
/* Calculates a file checksum 
   Returns -1 if error 
 */
    _s32    sz, chk=0, c;
    
    lseek(fd, 0, SEEK_SET);
    while (1) {
	sz = read(fd, (char *) tr_buffer, max_tr_size);/* read in file */
	if (!sz)				 /* ?EOF */
	  break;
	if (sz == -1) return -1;
	for (c=0; c<sz;c++)			 /* add up bytes in */
	  chk += *(tr_buffer+c);		 /* this file */
    }
    lseek(fd, 0, SEEK_SET);			 /* move back to BOF */
    return labs(chk);				 /* return checksum */
}
    
_s32 mem_calc_checksum(char *m, _s32 sz)
{
/* Calculates the checksum of a memory block 'm' of size 'sz'
   Returns -1 if error */
    _s32    chk=0, c;
    
    for (c=0; c<sz;c++)				 /* add up bytes in */
      chk += *(m+c);				 /* this file */
    return chk;					 /* return checksum */
}


_errstat open_logfile(char *prog)
{
    char s[100];
    
    lf = 0;
    log_errors=0;
    log_warnings=0;
    if ((*log_file) && (log_level)) {
	lf = open(log_file, O_RDWR|O_CREAT|O_APPEND, S_IREAD|S_IWRITE);
	if (lf==-1) {lf=0; return do_exit(ERROR_OPENING_LOG);}
	strcpy(s, "Starting "); strcat(s, prog); strcat(s, " v"); strcat(s, CUR_VERSION);
	write_log(s);
    }
    return 0;
}


void close_logfile(char *prog)
{
    struct stat b;
    char  tmpf[MAX_FNAME], s[MAX_FNAME];
    WINDOW *mes=NULL;
    int    tmpfd;
    
    if (lf) {
	fsync(lf);
	if (fstat(lf, &b) != -1) {		 /* check to ensure size not */
	    if ((b.st_size > log_file_size * 1024*1024) &&
	       (log_file_size)) {		 /* log file too big */
		if (!no_windows) {
		    touchwin(win_main); wrefresh(win_main);
		    mes = status_box(mes, "Log file too big - making smaller", 1, TRUE, 1);
		}
		if (log_level > 2) write_log("Log file is too big - making it smaller");
		lseek(lf, b.st_size - (log_file_size*1024*1024), SEEK_SET);
		taper_tmpnam(tmpf);
		tmpfd = open(tmpf, O_RDWR|O_CREAT|O_APPEND, S_IREAD|S_IWRITE);
		if (tmpfd == -1) {
		    if (log_level > 2) write_log("Unable to open temp file in making log file smaller");
		    goto convclose;
		}
		if (my_filecopy(lf, tmpfd) == -1) {
		    close(tmpfd);
		    unlink(tmpf);
		    if (log_level > 2) write_log("Unable to write in making smaller log file");
		    goto convclose;
		}
		close(lf);			 /* close org log file */
		unlink(log_file);		 /* and remove it */
		lf = tmpfd;			 /* this info goes to the temp file */
		sprintf(s, "%s finished.", prog);
		write_log(s);
		write(lf, "\n\n", strlen("\n\n"));
		close(tmpfd);			 /* close temp file */
		lf = 0;
		my_rename(tmpf, log_file);	 /* rename it to log file */
		if (mes) close_statusbox(mes);
		return;
	    }
	}
	convclose:;
	  lseek(lf, 0, SEEK_END);			 /* move to end of file */
	  sprintf(s, "%s finished.", prog);	 /* to write this info */
	  write_log(s);
	  write(lf, "\n\n", strlen("\n\n"));
	  close(lf);				 /* close log file */
	  lf = 0;
	  if (mes) close_statusbox(mes);
    }
}

    
void calc_tape_pos(_s32 vol_no, _s32 *tape, _s32 *pos)
{
/* Works out on what tape and at what position on the tape
 * vol_no is.  
*/ 
    _s32 count;
    struct volume_tape_info *vti;
    
    vti = (struct volume_tape_info *) vt_info;
    for (count=0; count < ifd.number_volumes; count++)/* find which tape it's on */
	if (vti->volume == vol_no) {		 /* found this volume */
	    *tape = vti->start_tape;
	    break;
	}
        else
          vti++;

    vti = vt_info;
    *pos = (*tape == 1) ? 0 : 1;
    for (count=0; count < ifd.number_volumes; count++) {   /* now find out how many volumes are before it */
	if ((vti->start_tape == *tape) && (vti->volume < vol_no))
	  (*pos)++;
	vti++;
    }
}


char *get_vh(struct volume_header *svh, _s32 vol)
{
/* Works out where volume header for volume 'vol' is
 * assuming the volume headerse start at svh */
    
    _s32 c=0, c1, x;
    char *s = (char *) svh;
    struct volume_header v;
    
    while (c<vol-1) {
	memcpy(&v, s, sizeof(struct volume_header));   /* to ensure struct aligned */
	s += sizeof(struct volume_header);	 /* past volume header */
	for (c1=0; c1<v.no_sels;c1++) {	 /* past entries */
	    memcpy(&x, s, sizeof(_s32));
	    s += x + sizeof(_s32);	 /* entry */
	    memcpy(&x, s, sizeof(_s32));
	    s += x + sizeof(_s32);	 /* filter */
	}
	c++;
    }
    return s;
}


_errstat traverse_volume(file_passed_action action, _s32 no_in_vol,
		    time_t t_start, WINDOW *mes_box, _s8 full_traverse,
		    print_status ps, _s32 *cur_in_vol,
		    _s8 use_info, chksum_err ce)
/* Traverses a volume.
 * Returns -1 is user aborted, 0 if finished mid-way, 1 if fully finished
 * 
 * If full_traverse == 1, then traverse completes the volume
 * otherwise, traverse is allowed to finish mid-volume
 * 
 * If use_info == TRUE, then the file_info & filename is obtained
 * from the info file rather than from the tape. The tape file_info
 * is read, and only the fi.pos_in_vol is used to obtain the correct
 * entry in the info file
 * 
 * Action should return
 *    -4 to stop traverse even if full traverse on
 *    -3 to stop traverse - file NOT read] should stop 
 *    -2 to stop traverse - file read    ] should stop
 *    -1 error - abort                   ] 
 *     0 file was not read in
 *     1 file was read in
 * 
 * chksum_err should return
 *     0 to continue traverse
 *    -1 to stop
 * 
 *   if chksum wants traverse to stop, traverse will return 0
 */ 
{
    _s32 tr, x, sz, a;
    struct file_info fi;
    char fn[MAX_FNAME],  scr[100+MAX_FNAME];
    char l[MAX_FNAME*2];
    time_t t_current;
    _s32  chk, info_pos=0;
    struct oa_file_entry *fe=archive_files;

    *cur_in_vol=0;
    while (*cur_in_vol < no_in_vol) {
	*scr = wgetch(mes_box);
	if ((*scr == 'q') || (*scr == 'Q')) {
	    if (message_box("Confirm abort", MB_YESNO))   /* check user wants to abort */
	      return -1;
	    touchwin(mes_box); wrefresh(mes_box);
	}
	(*cur_in_vol)++;				
	sprintf(l, "Reading in file info & filename");
	if (tape_read_fi(&fi) == -1) return -1;
	if (use_info) {				 /* find which info file */
	    while (info_pos++ < no_in_archive) { /* look for this file in volume dir */
		if (fi.pos_in_archive == fe->i.pos_in_archive)
		  break;
		advance_ofe(&fe);
	    }
	    if (info_pos > no_in_archive)	 /* couldn't find it for */
	      return do_exit(ERROR_INFO_FILE);	 /* some reason */
	    fi = fe->i;
	}
	if (fi.checksum == -2) {
	    if (old_archive) {
		if (log_level > 2) write_log("Negative 2 checksum detected from old version of taper - corrected");
		fi.checksum = 2;
	    }
	    else {
		write_log("Previously aborted backup detected");
		break;
	    }
	}
	if (fi.checksum < 0) 
	  if ((fi.checksum != -1) && (fi.checksum != -2)) {
	      fi.checksum = labs(fi.checksum);
	      if (log_level > 2) write_log("Negative checksum detected from old version of taper - corrected");
	  }
	if ((fi.name_len == 0) || (fi.act_size == -1)) {/* need to check due to bug in earlier tapers */
	    write_log("Incorrect file count in volume. Fudged to overcome");   /* and also aborted backups */
	    break;				 /* assume that this is now EOF */
	}
	if (tape_read(fn, fi.name_len) != fi.name_len) /* skip past filename */
	  return -1;
	if (use_info) 				 /* use filename from */
	    strcpy(fn, name(fe));		 /* info file */
	t_current = time(NULL);

	if (ps) {				 
	    touchwin(mes_box);			 /* don't know when read may have hit end of tape */
	    ps(mes_box,  *cur_in_vol, no_in_vol, abs(fi.volume), fi.act_size, fn,
	     t_start, t_current);			 /* let caller print status box */
	}
	a = (action == NULL) ? 0 : action(&fi, fn, fe);
	if (a==-1) return -1;			 /* error */
	if ((a == -2) && (!full_traverse)) 
	  return (*cur_in_vol == no_in_vol);	 /* finish traverse */
	if ((a == -3) && (!full_traverse)) 
	  return (*cur_in_vol == no_in_vol);	 /* finish traverse */
	if (a==-4) return 0;			 /* unconditional finish */
	if ((a==1) || (a==-2))  continue;	 /* file was read in */

	if (fi.checksum == -1) continue;
	if ((!old_archive) && (fi.checksum == -2)) continue;

	sprintf(l, "Passing file %s", fn);
	if (log_level > 1) write_log(l);
	if (S_ISLNK(fi.mode)) {
	    sprintf(l, "Passing link info for %s", fn);
	    if (log_level > 2) write_log(l);
	    if (tape_read_namelen(fn) == -1) return -1;
	    continue;
	}

	if (!S_ISREG(fi.mode)) {		 /* if not regular file, checksum should be 0 */
	    chk = 0;
	    goto chkchksum;
	}

	sz = fi.act_size;			 /* otherwise, assume regular file */
	chk = 0;
	while (1) {				 /* copy file accross */
	    tr = min(sz, max_tr_size); 
	    x = tape_read(tr_buffer, tr);	 /* to device file   */
	    if (!x) break;
	    if (x == -1) 
	      return -1;
	    chk += mem_calc_checksum(tr_buffer, x);
	    sz -= x;
	    if (!sz) break;			 /* read all we need to */
	}
	
      chkchksum:;
	if (labs(chk) != fi.checksum) {
	    sprintf(l, "ERROR:  Checksum error for file %s", fn);
	    write_log(l);
	    log_errors++;
	    if (ce != NULL) {			 /* checksum error handler */
		if (ce(&fi, fn) == -1) 
		    return 0;
		touchwin(mes_box); wrefresh(mes_box);
	    }
	}
	else {
	    sprintf(l, "Checksum OK for file %s", fn);
	    if (log_level > 1) write_log(l);
	}
    }
    return 1;					 /* successful return */
}


void final_message(char *prog)
{
/* Prints the final message, telling how many warnings & how many
 * errors were encountered
 */
    
    char s[4][150];

#ifndef TRIPLE_BUFFER				 /* if not using triple buffering */
    if (!strcmp(prog, "Restore"))  {		 /* ie. not using SYSV_IPC shared memory */
	log_warnings=-1;			 /* these won't have been updated */
	log_errors=-1;
    }
#endif    
    if (no_windows) return;
    my_werase(win_main, COLOR_MAIN); wrefresh(win_main);
    sprintf(s[0], "%s completed", prog);
    strcpy(s[1], "");
    sprintf(s[2], "%d Warnings", log_warnings);
    sprintf(s[3], "%d Errors", log_errors);
    multi_message_box(s, 4, MB_OK);
}


void print_title_line()
{
/* Prints the top title line with the archive ID & archive title */
    char s[200];

    if (no_windows) return;
    my_werase(title, COLOR_TITLE);
    if (*ifd.archive_title)
      sprintf(s, "Archive ID %u. Title '%s'", ifd.archive_id, ifd.archive_title);
    else
      sprintf(s, "Archive ID %u. Title <no title>", ifd.archive_id);
    centre(title, 0, s, COLOR_TITLE);
    wrefresh(title);
}


void backrest_paint(void) {
    touchwin(title); touchwin(files); touchwin(selection); touchwin(on_vol); 
    wrefresh(title); wrefresh(files); wrefresh(selection); wrefresh(on_vol); 
}


_errstat bmessage_box(char *s, int type) {
    int x=message_box(s, type);
    backrest_paint();
    return x;
}


void backrest_init_windows(void) {				 
/*  Initialise windows and print screen edges etc..      */

    int       right_startx;
    
    screen_xlen=win_main->_maxx+1;		 /* Dimensions of screen */
    screen_ylen=win_main->_maxy+1;
    
    screen_ylen_files = screen_ylen/2 + 3;
    screen_ylen_selection = screen_ylen - screen_ylen_files;
    left_width = screen_xlen*6/10;		 /* split 60% */
    right_width = screen_xlen-left_width;	 
    right_startx = left_width;
    
    files = my_newwin(screen_ylen_files, left_width,
		  1, 0);			 /* files hand window */
    on_vol = my_newwin(screen_ylen_files, right_width,
		  1, right_startx);		 /* files hand window */
    selection = my_newwin(screen_ylen_selection, screen_xlen,/* right hand window */
		   screen_ylen_files+1, 0);
    keypad(on_vol, TRUE); 			 
    keypad(selection, TRUE); 			 
    keypad(files, TRUE);			 /* set input modes */
}



void backrest_clear_screen(void)
{
    my_werase(on_vol, COLOR_ONVOL); my_werase(files, COLOR_DIRECTORY); 
    my_werase(selection, COLOR_SELECTED); 
    box(on_vol, ACS_VLINE, ACS_HLINE);
    box(files, ACS_VLINE, ACS_HLINE);		 /* draw boxes around */
    box(selection, ACS_VLINE, ACS_HLINE);	 /* screens */
    wrefresh(on_vol); wrefresh(files); wrefresh(selection);
}


void backrest_kill_windows(void)
{
    if (no_windows) return;
    my_delwin(on_vol);
    my_delwin(selection);
    my_delwin(files);
    my_werase(win_main, COLOR_MAIN);
    wrefresh(win_main);
}

void backrest_free_memory(void)
{
    my_free(sb_directory);
    my_free(directory);
    my_free(dirs);
    my_free(sel_files);
    my_free(vt_info);
    my_free(tsi);
    my_free(archive_files);
    my_free(info);
    my_free(vol_headers);
    my_free(vol_details);
}


_errstat backrest_do_mallocs(void)
{
/* Allocates memory for backup stuff */

    sb_directory = my_malloc(1);
    directory = my_malloc(1);
    dirs = my_malloc(1);
    sel_files = my_malloc(sizeof(*sel_files));
    vt_info = my_malloc(1);
    tsi = my_malloc(1);
    archive_files = my_malloc(1);
    info = my_malloc(sizeof(struct info_file_header));
    vol_headers = my_malloc(sizeof(int));
    vol_details = my_malloc(1);
    if ((dirs == NULL) || (sel_files == NULL) || (vt_info == NULL) ||
	(archive_files == NULL) || (info == NULL) || (vol_headers == NULL)
	|| (directory == NULL) || (vol_details == NULL) || (tsi == NULL) ||
	(sb_directory == NULL))
      return -1;
    return 0;
}
    

void print_on_voldir_line(WINDOW *win, _s32 entry, int line, char ref) {
/*  Prints one entry in a directory at line 'line'  */

    _s32 c=0;
    char *s;
    static char spc[] = {"                                                                                                                                                      "};
    char s1[MAX_FNAME];
    
    s = vol_details;				 /* find appropriate */
    while (c++<entry)				 /* line in vol_details buffer */
      while (*s++);

    strcpy(s1, s);
    s1[win->_maxx-4] = 0;
    if (!strncmp(s, "Volume ", strlen("Volume ")) ||
	!strncmp(s, "Contains ", strlen("Contains ")) ||
	!strncmp(s, "Backed up at ", strlen("Backed up at ")) )
      mvwaddstr(win, line+1, 2, s1);
    else
      mvwaddstr(win, line+1, 4, s1);		 
    if (!*s) {					 /* blank line */
	spc[win->_maxx-2] = 0;
	mvwaddstr(win, line+1, 1, spc);
    }
    centre(bottom, 0, s, COLOR_BOTTOM); wrefresh(bottom);
    if (ref)
      wrefresh(win);
}


void print_on_vol_dir(WINDOW *win, _s32 start, char *p_scroll) {
    int    cur_line=1, cur_ent;
    char   s[100];
    
    my_werase(win, COLOR_ONVOL);
    sprintf(s, "On archive");
    s[win->_maxx-1] = 0;
    wattron(win, A_UNDERLINE);
    mvwaddstr(win, 1, 1, s);
    convert(s, total_compressed);
    mvwaddstr(win, 1, win->_maxx-strlen(s)-1, s);
    wattroff(win, A_UNDERLINE);
    cur_ent = start;
    if (no_vol_details)
      while (cur_line < screen_ylen_files-2) {
	  print_on_voldir_line(win, cur_ent, cur_line, FALSE);/* print line */
	  cur_line++;			 /* increment */
	  cur_ent++;
	  if (start+cur_line > no_vol_details) /* check that not at end of dir */
		break;
      }
    *p_scroll = 0;
    print_scroll_bar(win, no_vol_details, &vol_sd, win->_maxy,
		     win->_maxx, p_scroll);
    wrefresh(win);
}


void print_my_name()
{
    char s[100];
    
    if (no_windows) return;
    sprintf(s, "Taper %s by Yusuf Nagree (yusuf@nagree.u-net.com)", CUR_VERSION);
    centre(bottom, 0, s, COLOR_BOTTOM);
    wrefresh(bottom);
}


void backrest_mfn(struct direntry *x, char *dir_name, char *prefix)
{
    char t[MAX_FNAME];
    
    strcpy(t, "Set ");
    strcat(t, &x->entry.d_name[strlen(prefix)]);
    strcpy(x->entry.d_name, t);
}


FILE *backrest_restore_backupset(char *nm)
{
/* Opens a file set. 
 * 
 * If nm == NULL, user is prompted for backup file set name,
 * otherwise, use nm as filename
 * 
 * Returns File handle of NULL if couldn't */
    FILE *f;
    char s[MAX_FNAME], s1[MAX_FNAME];
      
    if (nm != NULL) {
	strcpy(s, "Set ");
	strcat(s, nm);
    }
    else
      select_file(taper_info_files, "taper_set.", s, backrest_mfn, TRUE);
    strcpy(s1, dir_cur_dir); strcat(s1, "/taper_set."); 
    strcat(s1, &s[4]);
    f=fopen(s1, "r");
    if (f==NULL) 
	do_exit(ERROR_OPENING_SET);
    return f;
}
    

void backrest_save_backupset(int in_backup)
{
/* Save file set */
    char s[MAX_FNAME], s1[MAX_FNAME];
    FILE *f;
    struct oa_file_entry *fe=sel_files;
    int c;
    
    *s =0;
    if (!get_string(win_main, s, MAX_FNAME, "Enter name to give set"))
      return;
    if (!*s) return;
    strcpy(s1, taper_info_files);
    strcat(s1, "/"); strcat(s1, "taper_set."); strcat(s1, s);
    if (make_info_dirs() == -1)			 /* make directories */
      return;
    f = fopen(s1, "r+");
    if (f!=NULL) {
	fclose(f);
	if (bmessage_box("File set exists", MB_APPENDOVERWRITE))
	  f = fopen(s1, "a");
	else
	  f = fopen(s1, "w");
    }
    else
      f = fopen(s1, "w");
    if (f==NULL) {
	do_exit(ERROR_CREATING_SET);
	return;
    }
    for (c=0; c<no_sel; c++) {
	if (!in_backup) {			 
	    fprintf(f, "%s", cf);		 /* doing restore */
	    if (cf[strlen(cf)-1] != '/')	 /* save full path */
	      fprintf(f, "%c", '/');
	}
	fprintf(f, "%s\n", name(fe));
	fprintf(f, "\n");
	advance_ofe(&fe);
    }
    fclose(f);
}


_errstat exclude_dir(char *fn)  
{
/* Sees if directory 'fn' should be excluded */
    
    char *s, *b, tok[MAX_FNAME];
    int fin=0;
    
    if (!*exclude_dirs) return 0;		 /* empty exclusion list */ 
    strcpy(tok, exclude_dirs);
    s=tok;
    while (!fin) {
	b=s;
	while (*s && (*s != ' ')) s++;		 /* look for space or eos */
	if (!*s) fin=1;				 /* end of string */
	*s = 0; 
	if (strlen(b) <= strlen(fn))		 /* b is excluding directory */
	  if (!strncmp(b, fn , strlen(b)))
	      return 1;
	s++;
    }
    return 0;
}


_errstat exclude_archive(char *fn)
{
/* Determines whether to exclude file 'fn' from archive 
 * 
 * returns 1 if should be excluded (ie. in list)
 * returns 0 if shouldn't be excluded
*/
    if (exclude_list(fn, exclude_files) == 1)	 /* remove on basis of suffix */
      return 1;
    if (exclude_dir(fn)) return 1;
    return 0;
}

    
_errstat process_dir(WINDOW *mes, int ln, char *org_dir, char inc, 
		 do_process_dir dpd, _s8 send_dir)
{
/* Goes through the directory given full dir and calls the
 * function dpd with each entry. If send_dir=TRUE, then
 * directory entries are also sent to dpd, otherwise they
 * are not
 * 
 * If org dir is a file, then just does one dpd and returns
 * 
 * The dpd function should return 0 to continue,
 * -1 to stop the process_dir
 * 
 * Returns -1 if user aborted. Errors are ignored - the sizing continues
*/

    char  *dir_list=NULL;
    _s32  len=0;
    DIR  *dir_ptr;
    struct dirent *x;
    struct stat b;
    char   s1[MAX_FNAME], old_dir[MAX_FNAME], cur_dir[MAX_FNAME];
    int    app;
    char   s;
    dev_t  cur_fs=0;

    if (get_statinfo(org_dir, &b) != -1) {/* get file information */
	cur_fs = b.st_dev;
	if (!S_ISDIR(b.st_mode)) {		 /* if org_dir not a directory then do the dpd and finish*/
	    if (!exclude_archive(org_dir)) {	 /* not on exclude list */
		if (inc) {
		    if (file_more_recent(org_dir, &b)) {
			dpd(org_dir, &b);
		    }
		}
		else {
		    dpd(org_dir, &b);
		}
	    }
	    return 0;				 /* don't bother with rest */
	}
    }
    
    if (S_ISDIR(b.st_mode))			 /* exclude this directory */
	if (exclude_dir(org_dir)) return 0;
    
    dir_list = my_malloc(100*MAX_FNAME);
    getcwd(old_dir, sizeof(old_dir));		 /* save old directory */
    strcpy(cur_dir, org_dir);
    
    if (mes) nodelay(mes, TRUE);
    while (1) {
	if (mes) {
	    s = wgetch(mes);
	    if ((s == 'q') || (s == 'Q'))
	      return -1;
	    status_box(mes, cur_dir, ln, FALSE, 1);
	}
	chdir(cur_dir);
	if ((dir_ptr = opendir(cur_dir)) == NULL)/* can't open directory */
	  goto nextdir;
	app = !(cur_dir[strlen(cur_dir)-1] == '/');   /* do we need to append '/'? */
	while (1) {				 /* process directory */
	    x=readdir(dir_ptr);
	    if (x==NULL) break;			 /* end of directory */
	    if (get_statinfo(x->d_name, &b) == -1)/* get file information */
	      continue;				 /* couldn't - ignore file */
	    if ((proc_dev) && (proc_dev == b.st_dev))/* is the /proc directory */
	      continue;
	    if ((ofs) && (cur_fs != b.st_dev))	 /* this directory is not on same filesystem */
	      goto nextdir;
	    strcpy(s1, cur_dir);
	    if (app) strcat(s1, "/");
	    strcat(s1, x->d_name);
	    if (dv) 
	      if (is_regfile(dv))		 /* if backup device is a reg file */
	        if (!strcmp(s1, tape) || !strcmp(s1, ntape))/* don't backup backup device */
	          continue;
	    if (!strcmp(s1, log_file))		 /* don't back up log file */
	      continue;
	    if (S_ISDIR(b.st_mode)) {
		if (!strcmp(x->d_name, "."))	 /* don't worry about these */
		  continue;
		if (!strcmp(x->d_name, ".."))
		  continue;
		if (!exclude_dir(s1)) {		 /* if on exclude list, don't add */
		    len++;			 /* add to list of directories we must process */
		    if (len%100 == 0) 
		      dir_list = my_realloc(dir_list, (len+99)*MAX_FNAME);
		    strcpy((dir_list+(len-1)*MAX_FNAME), s1);
		}
	    }
	    if (!S_ISDIR(b.st_mode) || ((S_ISDIR(b.st_mode) && send_dir))) {
		if (!exclude_archive(s1)) {
		    if (inc) {
			if (file_more_recent(s1, &b)) {
			    if (dpd(s1, &b) == -1) goto finish;
			}
		    }
		    else {
			if (dpd(s1, &b) == -1) goto finish;
		    }
		}
	    }
	}					 /* finished processing directory */
	nextdir:;
	if (dir_ptr) closedir(dir_ptr);
	if (!len) break;			 /* finished */
	len--;
	strcpy(cur_dir, dir_list+len*MAX_FNAME);
    }

    finish:;
    chdir(old_dir);				 /* change back to old directory */
    my_free(dir_list);
    if (mes) nodelay(mes, FALSE);
    return 0;
}


#ifdef TRIPLE_BUFFER
_errstat make_shm_block(int *id, _s32 blksize, _s8 **shm)
{
/* Makes a shared memory block. 
   Returns 0 if OK, -1 if not */
    int lp=0;
    
    while (1) {					 /* shared memory */
	*id = shmget(IPC_PRIVATE, blksize, IPC_CREAT|IPC_EXCL|0666);
	if (*id != -1) break;			 /* made it OK */
	lp++;
	if (lp==100)				 /* only allow 100 goes */
	  return do_exit(ERROR_SETTING_SHARED_MEM);
    }					 

    *shm = (_s8 *) shmat(*id, 0, 0);		 /* try and attach memory */
    if (*shm == (_s8 *) -1) 
	return do_exit(ERROR_SETTING_SHARED_MEM);
    return 0;
}
#endif


_errstat init_buffers(_s8 reall)
{						 
/* Allocates memory for three buffers
 * 
 * 1. Read buffer
 * 2. Write buffer
 * 3. File transfer buffer
 * 
 * If reall = 1, then buffers are realloced, otherwise they
 * are malloced
 * 
 * Note: There will be problems if you are downsizing the buffer and
 *   you have data left in the write_buffer

 These do NOT go through my_malloc, because I know about them and they
 will be be deleted. If they went through my_malloc, they would have to be
 reallocated & deallocated after every call to a subsystem
 */ 
    read_buffer = (reall == 1) ? realloc(read_buffer, block_size) : malloc(block_size);
    if (read_buffer == NULL) goto err;
    if (!reall) {
#ifdef TRIPLE_BUFFER
	if (make_shm_block(&shm_id, sizeof(struct shared_mems), (_s8 **) &shm) == -1)
	  goto err;
	if (make_shm_block(&wb1_shmid, DOUBLE_BUFFER_SIZE, &w_buffer_1) == -1)
	  goto err;
	if (make_shm_block(&wb2_shmid, DOUBLE_BUFFER_SIZE, &w_buffer_2) == -1)
	  goto err;
#else    
	shm = malloc(sizeof(struct shared_mems));
	if (shm == NULL) goto err;
	w_buffer_1 = (reall == 1) ? realloc(w_buffer_1, DOUBLE_BUFFER_SIZE) : malloc(DOUBLE_BUFFER_SIZE);
	if (w_buffer_1 == NULL) goto err;
	w_buffer_2 = (reall == 1) ? realloc(w_buffer_2, DOUBLE_BUFFER_SIZE) : malloc(DOUBLE_BUFFER_SIZE);
	if (w_buffer_2 == NULL) goto err;
#endif	
	tr_buffer = malloc(max_tr_size);
	if (tr_buffer == NULL) {
	    goto err;
	}
	read_offset = -1;
	read_buffer_count = 0;
	write_offset = 0;
	write_buffer_count = 0;
	write_pid = 0;
    }
#ifdef TRIPLE_BUFFER
    else {
	shmctl(wb_shm_id, IPC_RMID, 0);		 /* so get rid of original one */
	shmdt(write_buffer);			 
    }
    if (make_shm_block(&wb_shm_id, block_size, (_s8 **) &write_buffer) == -1)
      goto err;
#else    
    write_buffer = (reall == 1) ? realloc(write_buffer, block_size) : malloc(block_size);
    if (write_buffer == NULL) goto err;
#endif    
    return 0;
    
    err:;					 /* error occurred */
    if (read_buffer) {
	free(read_buffer);		 /* free whatever we allocated */
	read_buffer = NULL;
    }
    if (tr_buffer) {
	free(tr_buffer);
	tr_buffer = NULL;
    }
#ifdef TRIPLE_BUFFER
    if (write_buffer) {
	shmctl(wb_shm_id, IPC_RMID, 0);		 /* so get rid of original one */
	shmdt(write_buffer);			 
    }
    if (w_buffer_1) {
	shmctl(wb1_shmid, IPC_RMID, 0);		 /* so get rid of original one */
	shmdt((char *) w_buffer_1);			 
    }
    if (w_buffer_2) {
	shmctl(wb2_shmid, IPC_RMID, 0);		 /* so get rid of original one */
	shmdt((char *) w_buffer_2);			 
    }
    if (shm) {
	shmctl(shm_id, IPC_RMID, 0);		 /* so get rid of original one */
	shmdt((char *) shm);			 
    }
#else
    if (write_buffer) {
	free(write_buffer);
	write_buffer = NULL;
    }
    if (w_buffer_1) {
	free(w_buffer_1);
	w_buffer_1 = NULL;
    }
    if (w_buffer_2) {
	free(w_buffer_2);
	w_buffer_2 = NULL;
    }
    if (shm) {
	free(shm);
	shm = NULL;
    }
#endif
    return do_exit(ERROR_MEMORY);
}


void free_buffers()
{
    if (tr_buffer) {
	free(tr_buffer);
	tr_buffer = NULL;
    }
    if (read_buffer) {
	free(read_buffer);
	read_buffer = NULL;
    }
#ifdef TRIPLE_BUFFER    
    if (log_level > 3) write_log("Detaching shared buffers");
    shmdt(write_buffer);			 /* remove write buffer */
    shmdt((char *) w_buffer_1);			 
    shmdt((char *) w_buffer_2);			 
    shmdt((char *) shm);			 /* remove write buffer */
    shmctl(shm_id, IPC_RMID, 0);		 /* destroy buffers */
    shmctl(wb_shm_id, IPC_RMID, 0);		 /* and shared mems */
    shmctl(wb1_shmid, IPC_RMID, 0);		 
    shmctl(wb2_shmid, IPC_RMID, 0);		 
#else
    if (write_buffer) {
	free(write_buffer);
	write_buffer = NULL;
    }
    if (w_buffer_1) {
	free(w_buffer_1);
	w_buffer_1 = NULL;
    }
    if (w_buffer_2) {
	free(w_buffer_2);
	w_buffer_2 = NULL;
    }
    if (shm) {
	free(shm);
	shm = NULL;
    }
#endif
}


char *print_kb(char *s1, _u32 bytes)
{
/* Prints bytes as x,xxx KB 
 * The string returned is just the string passed
 */
    char s[100];
    
    sprintf(s1, "%sK", convert(s, (float) bytes/(float) 1024));
    return s1;
}

char *print_mb(char *s1, _u32 bytes)
{
/* Prints bytes as x.xxMB 
 * The string returned is just the string passed
 */
    
    sprintf(s1, "%.2fMb", (float) bytes/(float) 1048576);
    return s1;
}

    
void clear_main()
{
    char s[100];
    
    my_werase(win_main, COLOR_MAIN); 
    strcpy(s, "Taper - Linux Backup for ");
    strcat(s, make_tt(tape_type));
    centre(title, 0, s, COLOR_TITLE);
    print_my_name();
    wrefresh(title); wrefresh(win_main);
}


_errstat read_into_temp(struct file_info *fi, char *tmpf, char *fn)
{
/* Read the file next on the tape to a temporary file 
 
 * Accepts  struct file_info *fi   file info for the file
 *          tmpf                   pointer to file to save file as
 * 
 * Returns -1 if error
 *         -2 if checksum error
 */
    _s32 sz, tr, x, chk;
    _s32 c1;
    int of, ret=0;
    char l[MAX_FNAME];
    
    of = creat(tmpf, S_IREAD|S_IWRITE);		 /* create a temporary file */
    if (of==-1) {				 /* to put data in */
	sprintf(l, "while creating temp file to receive data for %s", fn);
	write_error_log(l);
	return -1;
    }
    						 /* copy file to temporary file */
    sz = fi->act_size;				 /* from tape drive */
    sprintf(l, "Read in data for %s from tape to %s", fn, tmpf);
    if (log_level > 2) write_log(l);
    chk=0;
    c1=0;
    while (1) {					 /* copy file accross */
	tr = min(sz, max_tr_size); 
	x = tape_read(tr_buffer, tr);		 /* from device file   */
	if (!x) break;
	if (x == -1) 
	  return -1;
	chk += mem_calc_checksum(tr_buffer, x);	 /* calculate checksum */
	sz -= x;
	if (c1 != -1) {				 /* if there have been no previous write errors */
	    c1 = write(of, tr_buffer, x);	 /* if there has been a prev write error */
	    if (c1 == -1)			 /* still continue reading to pass by file on device  */
	      write_error_log(l);
	}
	if (!sz) break;				 /* read all we need to */
    }
    if (labs(chk) != fi->checksum) {
	sprintf(l, "ERROR:  Checksum error for file %s - should be %d but found %ld",
		fn, fi->checksum, labs(chk));
	write_log(l);
	log_errors++;
	ret = -2;
    }
    else {
	sprintf(l, "Checksum OK for file %s", fn);
	if (log_level > 1) write_log(l);
    }
    close(of);
    if (c1==-1)  {
	sprintf(l, "ERROR: Error writing data to file %s", tmpf);
	write_log(l); log_errors++;
	unlink(tmpf);
	return -1;
    }
    return ret;
}


void compress_info_file(_u32 archive_id)
{
/* Compresses info file. If can't, uncompressed file is left
 * alone. The info file header is left uncompressed. */
    
    char tmpf[MAX_FNAME], ifile[MAX_FNAME];
    int  ifd=0, ofd=0;
    struct info_file_header id, id1;
    struct stat statb;

    if (log_level > 2) write_log("Compressing info file");
    if (log_level > 2) write_log("Opening original info file");
    make_info_filename(ifile, archive_id);	 /* make info filename */
    ifd = open(ifile, O_RDONLY);		 /* open orig info file */
    if (ifd==-1) {
	write_error_log("opening original info file");
	goto err;
    }
    if (log_level > 2) write_log("Creating temp file");
    taper_tmpnam(tmpf);
    ofd = creat(tmpf, S_IREAD|S_IWRITE);
    if (ofd==-1) {
	write_error_log("creating temp file");
	goto err;
    }
    if (read(ifd, &id, sizeof(id)) != sizeof(id)) {
	write_error_log("reading original info file header");
	goto err;
    }
    ifd_endianize2mach(&id, &id1);
    id1.magic = INFO_MAGIC_COMPRESSED;
    ifd_endianize2little(&id1, &id);
    
    if (write(ofd, &id, sizeof(id)+sizeof(_u32)) != sizeof(id)+sizeof(_u32)) {
	write_error_log("writing info file header to compressed file");
	goto err;
    }
    zip(ifd, ofd);
    fstat(ofd, &statb);				 /* get size of compressed */
    lseek(ofd, sizeof(struct info_file_header), SEEK_SET);   /* and write it out */
    write_u32(ofd, (_u32 *) &statb.st_size);
    close(ifd); close(ofd);
    ifd = 0; ofd = 0;
    unlink(ifile);				 /* remove original */
    my_rename(tmpf, ifile);			 /* rename compressed to original */
    *tmpf = 0;

    err:;
      if (ifd) close(ifd);
      if (ofd) close(ofd);
      if (*tmpf) unlink(tmpf);
}


_errstat read_u32(int fd, _u32 *x)
{
/* Reads an usigned 32 bit integer from the file
 * 
 * Takes into account big/little endian 
 * 
 * Returns -1 if error, 0 otherwise
 */
    char s[sizeof(_u32)];
    
    if (read(fd, s, sizeof(s)) != sizeof(s)) return -1;
    *x = little2machu32((_u32 *) s);
    return 0;
}


_errstat write_u32(int fd, _u32 *x)
{
/* Reads an usigned 32 bit integer from the file
 * 
 * Takes into account big/little endian 
 * 
 * Returns -1 if error, 0 otherwise
 */
    _s32 s;
    
    s = mach2littleu32(x);
    if (write(fd, (char *) &s, sizeof(s)) != sizeof(s)) return -1;
    return 0;
}


_errstat write_out_info_file(char *info_file, _u32 archive_id)
{
/* Writes out info file in memory (pointed to by info_file to 
 * file. File is not opened. Archive Id is passed
 * 
 * Writes in little endian format
 * 
 * Returns -1 if error, 0 otherwise
*/
    int fd;
    struct info_file_header ifd1;
    struct file_info fi, fi1;
    struct volume_header vh, vh1;
    struct volume_tape_info vti, vti1;
    struct tape_size_info xtsi, tsi1;
    char *cp;
    _u32 c;
    
    if (log_level > 2) write_log("Writing info file information");
    if ((fd = open_info_file(FALSE, archive_id)) == -1) return -1;
    ifd_endianize2little(&ifd, &ifd1);
    if (write(fd, &ifd1, sizeof(struct info_file_header)) != sizeof(struct info_file_header)) 
      return do_exit(ERROR_WRITING_INFO);
    cp = info_file + sizeof(struct info_file_header);
    for (c=0; c<ifd.no_in_archive; c++) {
	memcpy(&fi, cp, sizeof(struct file_info));   /* boundary issue */
	fi_endianize2little(&fi, &fi1);
	if (write(fd, &fi1, sizeof(struct file_info)) != sizeof(struct file_info))
	  return do_exit(ERROR_WRITING_INFO);
	cp += sizeof(struct file_info);
	if (write(fd, cp, fi.name_len) != fi.name_len)
	  return do_exit(ERROR_WRITING_INFO);
	PAST_STRING_FI(cp);
    }
    cp = (char *) vol_headers;
    for (c=0; c<ifd.number_volumes; c++) {
	memcpy(&vh, cp, sizeof(struct volume_header));   /* for boundary */
	volheader_endianize2little(vh, &vh1);	 /* write out volume header */
	if (write(fd, &vh1, sizeof(struct volume_header)) != sizeof(struct volume_header))
	  return do_exit(ERROR_WRITING_INFO);
	cp += sizeof(struct volume_header);	 
	if (write(fd, cp, vh.size_header-sizeof(struct volume_header)) != vh.size_header-sizeof(struct volume_header))   /* write out strings */
	  return do_exit(ERROR_WRITING);
	cp += vh.size_header-sizeof(struct volume_header);
    }

    cp = (char *) vt_info;
    for (c=0; c<ifd.number_volumes; c++) {
	memcpy(&vti, cp, sizeof(struct volume_tape_info));/* for boundary */
	vti_endianize2little(&vti, &vti1);
	if (write(fd, &vti1, sizeof(struct volume_tape_info)) != sizeof(struct volume_tape_info))
	  return do_exit(ERROR_WRITING_INFO);
	cp += sizeof(struct volume_tape_info);
    }
    
    if (ifd.number_tsi) {
	cp = (char *) tsi;
	for (c=0; c<ifd.number_tsi; c++) {
	    memcpy(&xtsi, cp, sizeof(struct tape_size_info));   /* boundary issue */
	    tsi_endianize2little(&xtsi,&tsi1);
	    if (write(fd, &tsi1, sizeof(struct tape_size_info)) != sizeof(struct tape_size_info))
	      return do_exit(ERROR_WRITING_INFO);
	    cp += sizeof(struct tape_size_info);
	}
    }
    if (log_level > 2) write_log("Closing info file");
    close(fd);
    return 0;
}


_errstat read_info_file(int fd)
{
/* Reads info file into memory starting at info - assumes
 * that header has already been read and
 * file (already opened) with file descriptor fd.
 * 
 * Writes in little endian format
 * 
 * Returns -1 if error, 0 otherwise
*/
    struct file_info fi, fi1;
    struct volume_header vh, vh1;
    struct volume_tape_info vti, vti1;
    struct tape_size_info xtsi, tsi1;
    char *cp;
    _u32 c;
    
    if (log_level > 2) write_log("Reading info file information");
    cp = info + sizeof(struct info_file_header);
    for (c=0; c<ifd.no_in_archive; c++) {
	if (read(fd, &fi, sizeof(struct file_info)) != sizeof(struct file_info))
	  return do_exit(ERROR_READING_INFO);
	fi_endianize2mach(&fi, &fi1);
	memcpy(cp, &fi1, sizeof(struct file_info));
	cp += sizeof(struct file_info);
	if (read(fd, cp, fi1.name_len) != fi1.name_len)
	  return do_exit(ERROR_READING_INFO);
	PAST_STRING_FI(cp);
    }
    
    cp = (char *) vol_headers;
    for (c=0; c<ifd.number_volumes; c++) {
	if (read(fd, &vh, sizeof(struct volume_header)) != sizeof(struct volume_header))
	  return do_exit(ERROR_READING_INFO);
	volheader_endianize2mach(vh, &vh1);	 /* write out volume header */
	memcpy(cp, &vh1, sizeof(struct volume_header));   /* for boundary */
	cp += sizeof(struct volume_header);
	if (read(fd, cp, vh1.size_header-sizeof(struct volume_header)) != vh1.size_header - sizeof(struct volume_header))
	  return do_exit(ERROR_READING_INFO);
	cp += vh1.size_header-sizeof(struct volume_header);
    }

    cp = (char *) vt_info;
    for (c=0; c<ifd.number_volumes; c++) {
	if (read(fd, &vti, sizeof(struct volume_tape_info)) != sizeof(struct volume_tape_info))
	  return do_exit(ERROR_READING_INFO);
	vti_endianize2mach(&vti, &vti1);
	memcpy(cp, &vti1, sizeof(struct volume_tape_info));/* for boundary */
	cp += sizeof(struct volume_tape_info);
    }
    
    if (ifd.number_tsi) {
	cp = (char *) tsi;
	for (c=0; c<ifd.number_tsi; c++) {
	    if (read(fd, &xtsi, sizeof(struct tape_size_info)) != sizeof(struct tape_size_info))
	      return do_exit(ERROR_READING_INFO);
	    tsi_endianize2mach(&xtsi,&tsi1);
	    memcpy(cp, &tsi1, sizeof(struct tape_size_info));   /* boundary issue */
	    cp += sizeof(struct tape_size_info);
	}
    }
    if (log_level > 2) write_log("Closing info file");
    close(fd);
    return 0;
}


