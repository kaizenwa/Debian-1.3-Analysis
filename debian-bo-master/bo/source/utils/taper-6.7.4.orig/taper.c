/*
   Time-stamp: <96/08/05 19:32:56 yusuf>

   $Id: taper.c,v 1.27 1996/08/05 19:02:03 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: taper.c,v 1.27 1996/08/05 19:02:03 yusuf Exp $";
#endif /* lint */



/* This is the main Taper file.


   Note that this must not allocate memory before calling backup/restore/mkinfo
   and expect that this memory will still be allocated after the call since
   backup/restore/mkinfo may de-allocate it if it runs into an error
*/

#include "taper.h"


PRIVATE void usage(void)
{
/* Prints usage of common options */   
    fprintf(stderr, "\n\n");
    fprintf(stderr, "Taper command line options\n");
    fprintf(stderr, "----------------------------\n");
    fprintf(stderr, "\n\n");
    fprintf(stderr, "-T type (--tape-type)             tape type (f, z, s, l, r) \n");
    fprintf(stderr, "-f file (--rewinding-device)      use file as backup device (rewinding) \n");
    fprintf(stderr, "-n file (--non-rewinding-device)  use file as backup device (non-rewinding) \n");
    fprintf(stderr, "-b file (--both-devices)          use file as rewinding & non-rewinding device\n");
    fprintf(stderr, "+/- a   (--append-[on|off])       turns on/off append\n");
    fprintf(stderr, "-A id   (--archive-diff)          prints differences between archive & disk\n");
    fprintf(stderr, "- c num (--compress-type)         compression type (0 none, 1 e gzip, 2 int comp, 3 int gzip, 4 fast comp\n");
    fprintf(stderr, "+/- C   (--bad-checksum-[on|off]) prompt if encounter bad checksum\n");
    fprintf(stderr, "-d id   (--print-dir)             prints the directory of archive id\n");
    fprintf(stderr, "+/- D   (--auto-descend-[on|off]) automatically ascend/descend directories\n");
    fprintf(stderr, "-F file (--exclude-files)         which files not to include in archive\n");
    fprintf(stderr, "-g name (--volume-title)          title of volume\n");
    fprintf(stderr, "+/-G    (--get-blksize-[on|off])  Get block size after open\n");
    fprintf(stderr, "+/- h   (--hard-links-[on|off])   turns on/off storing of links or actual files\n");
    fprintf(stderr, "-H min  (--comp-head-start]       number of minutes head start for compressor\n");
    fprintf(stderr, "-i path (--info-files)            where the archive information files are\n");
    fprintf(stderr, "+/- I   (--compress-info-[on|off])turns on/off compression of info files\n");
    fprintf(stderr, "-j num  (--proc-device num)       device number of /proc system\n");
/*    fprintf(stderr, "+/- J   (--info-at-end-[on|off])  turns on/off info file at end of archive\n");    not yet */
    fprintf(stderr, "-l file (--log-file)              name of log file\n");
    fprintf(stderr, "-m num  (--log-level)             level of logging (0=none..4=all)\n");
    fprintf(stderr, "+/-M    (--memory-tight-[on|off]) limit memory use\n");
    fprintf(stderr, "-o num  (--overwrite)             level of file overwriting in restore\n");
    fprintf(stderr, "+/-O    (--tape-overwrite-[on|off]) overwrite tape if data exists on it\n");
    fprintf(stderr, "-p file (--preference-file)       name of preferences file\n");
    fprintf(stderr, "-q num  (--only-volume)           limits which volumes to restore from\n");
    fprintf(stderr, "+/-Q    (--fast-fsf-[on|off])     tape drive support fast fsf\n");
    fprintf(stderr, "-r path (--restore-path)          where to restore files to\n");
    fprintf(stderr, "-R list (--exclude-dirs)          which directories to automatically exclude\n");
    fprintf(stderr, "-s num  (--strip-paths)           level of pathname stripping\n");
    fprintf(stderr, "+/-S    (--set-blksize-[on|off])  Set block size before writing\n");
    fprintf(stderr, "-t name (--archive-title)         title of archive\n");
    fprintf(stderr, "+/- u   (--incremental-[on|off])  whether to use incremental backups by default\n");
    fprintf(stderr, "-v      (--version)               prints current version\n");
    fprintf(stderr, "-x num  (--block-size)            size of block to write to tape with\n");
    fprintf(stderr, "-X file (--exclude-compress)      which files not to compress\n");
    fprintf(stderr, "+/- y   (--sort-dirs-[on|off])    turns on or off directory sorting\n");
    fprintf(stderr, "-/+ z   (--prompt-dirs-[on|off])  confirm directory selection\n");
    fprintf(stderr, "-Z num  (--tape-size)             size of tape in MB\n");
    fprintf(stderr, "-U file (--unattended-file)       do an unattended backup with file (set)\n");   /*  */
    fprintf(stderr, "+/- w   (--recent-restore-[on|off]  most recent file restore or selected file\n");
    fprintf(stderr, "-?      (--help)                  help\n");
    do_exit(ERROR_USAGE);
}


PUBLIC char *make_tt(char tt)
{
/* Makes a string of tape type depending on tt. String valid 
 * until next call
*/
    static char s[20];

    switch(tt) {
     case TAPE_TYPE_ZFTAPE:
	strcpy(s, "zftape"); break;
     case TAPE_TYPE_SCSI:
	strcpy(s, "scsi"); break;
     case TAPE_TYPE_FTAPE:
	strcpy(s, "ftape"); break;
     case TAPE_TYPE_FILE:
	strcpy(s, "file"); break;
     case TAPE_TYPE_REMOVABLE:
	strcpy(s, "removable"); break;
     case TAPE_TYPE_IDE:
	strcpy(s, "ide"); break;
    }
    return s;
}


PUBLIC char make_short_tt(char tt)
{
/* Makes a char  of tape type depending on tt. 
*/
    switch(tt) {
     case TAPE_TYPE_ZFTAPE:
	return 'z';
     case TAPE_TYPE_SCSI:
	return 's';
     case TAPE_TYPE_FTAPE:
	return 'f';
     case TAPE_TYPE_FILE:
	return 'l';
     case TAPE_TYPE_REMOVABLE:
	return 'r';
     case TAPE_TYPE_IDE:
	return 'i';
    }
    return 0;
}


PRIVATE _errstat set_tape_type(char *s)
{
/* Sets tape_type depending on what tape is given in s. 
 * Returns error if illegal. 
 */
    if (s==NULL) return -1;
    tape_type = -1;
    if ((tolower(*s) == 'f') ||
	(!strcasecmp(s, make_tt(TAPE_TYPE_FTAPE))))
      tape_type = TAPE_TYPE_FTAPE;
    if ((tolower(*s) == 'z') ||
	(!strcasecmp(s, make_tt(TAPE_TYPE_ZFTAPE))))
      tape_type = TAPE_TYPE_ZFTAPE;
    if ((tolower(*s) == 's') ||
	(!strcasecmp(s, make_tt(TAPE_TYPE_SCSI))))
      tape_type = TAPE_TYPE_SCSI;
    if ((tolower(*s) == 'l') ||
	(!strcasecmp(s, make_tt(TAPE_TYPE_FILE))))
      tape_type = TAPE_TYPE_FILE;
    if ((tolower(*s) == 'r') ||
	(!strcasecmp(s, make_tt(TAPE_TYPE_REMOVABLE))))
      tape_type = TAPE_TYPE_REMOVABLE;
    if ((tolower(*s) == 'i') ||
	(!strcasecmp(s, make_tt(TAPE_TYPE_IDE))))
      tape_type = TAPE_TYPE_IDE;
    return tape_type;
}

PRIVATE void read_environment(void)
{
    if (getenv(TAPE_TYPE_ENVIRON))
      if (set_tape_type(getenv(TAPE_TYPE_ENVIRON)) == -1)/* set tape type from environ */
        do_exit(ERROR_ILLEGAL_TAPE_TYPE);
    if (getenv(TAPE_ENVIRON))
      strcpy(tape, getenv(TAPE_ENVIRON));

    if (getenv(NTAPE_ENVIRON))
      strcpy(tape, getenv(NTAPE_ENVIRON));
    
    strcpy(log_file, getenv("HOME"));
    strcat(log_file, "/"); 
    strcat(log_file, DEFAULT_LOG_FILE);
    if (getenv(LOGFILE_ENVIRON))
      strcpy(log_file, getenv(LOGFILE_ENVIRON));
    
    if (getenv(LOGLEVEL_ENVIRON))
      log_level = atol(getenv(LOGLEVEL_ENVIRON));
    
    strcpy(taper_info_files, getenv("HOME"));
    strcat(taper_info_files, "/"); 
    strcat(taper_info_files, DEFAULT_TAPER_INFO_FILES);
    if (getenv(TAPER_INFO_FILES_ENVIRON))
      strcpy(taper_info_files, getenv(TAPER_INFO_FILES_ENVIRON));
}


PRIVATE char evcol(char *s)
{
/* make color string into color integer */    
    if (!strcasecmp(s, "black")) return COLOR_BLACK;
    if (!strcasecmp(s, "red")) return COLOR_RED;
    if (!strcasecmp(s, "green")) return COLOR_GREEN;
    if (!strcasecmp(s, "yellow")) return COLOR_YELLOW;
    if (!strcasecmp(s, "blue")) return COLOR_BLUE;
    if (!strcasecmp(s, "magenta")) return COLOR_MAGENTA;
    if (!strcasecmp(s, "cyan")) return COLOR_CYAN;
    if (!strcasecmp(s, "white")) return COLOR_WHITE;
    return COLOR_BLACK;				 /* unknown = black */
}

PRIVATE void colev(char *s, char col)
{
    switch(col) {
     case COLOR_BLACK:
	strcpy(s, "black"); break;
     case COLOR_RED:
	strcpy(s, "red"); break;
     case COLOR_GREEN:
	strcpy(s, "green"); break;
     case COLOR_YELLOW:
	strcpy(s, "yellow"); break;
     case COLOR_BLUE:
	strcpy(s, "blue"); break;
     case COLOR_MAGENTA:
	strcpy(s, "magenta"); break;
     case COLOR_CYAN:
	strcpy(s, "cyan"); break;
     case COLOR_WHITE:
	strcpy(s, "white"); break;
    }
}


PUBLIC char *color_string(char fore, char back)
{
/* Makes a color string based of fore, and back
 * The string can be used until the next call */
    
    static char cols[100];
    char s1[20], s2[20];
    
    colev(s1, fore); colev(s2, back);
    sprintf(cols, "%s,%s", s1, s2);
    return cols;
}


PRIVATE _errstat evcol_pair(char *vl, char *fore, char *back)
{
/* extract foreground and background colors */   
    vl = strtok(vl, ",");
    if (vl == NULL) return -1;
    *fore = evcol(vl);
    vl = strtok(NULL, ",");
    if (vl == NULL) return -1;
    *back = evcol(vl);
    return 0;
}


PRIVATE void strip_spaces(char *s)
/* strips spaces from the string in 's'.
 * Spaces are preserved within ' or " only
*/
{
    int in_q = 0;
    int c, c1=0;
    
    while (c1 < strlen(s)) {			 /* remove spaces */
	if (s[c1] == '\'' || s[c1] == '"')
	  in_q = !in_q;
	if ((s[c1] == ' ') && (!in_q))
	  for (c=c1; c<strlen(s); c++) s[c] = s[c+1];
	else
	  c1++;
    }
}


PRIVATE void process_pref_line(char *s)
{
    int c=0;					 /* given a preference line - s */
    char *val;
    
    strip_spaces(s);				 /* get rid of spaces */
    if (*s)
      if (s[strlen(s)-1] == '\n')		 /* remove terminating newline */
      s[strlen(s)-1] = 0;
    if (!(*s))					 /* ignore blank lines */
	return;
    if (strchr(s, '=') == 0)			 /* no equals */
      do_exit(ERROR_PREF_FORMAT);
    val = strchr(s, '=')+1;
    if (*val == '"')				 /* preference is surrounded by quotes */
      val++;
    if (val[strlen(val)-1] == '"')
      val[strlen(val)-1] = 0;
    *strchr(s, '=') = 0;
    while (ap[c].pref_name) {
	if (!ap[c].save_pref) {		 /* this preference may have leading type */
	    if (!strcasecmp(&s[2], &ap[c].pref_name[2])) {   /* matches - must check prefix */
		if (*s == ((tape_type == -1) ? 'f' : make_short_tt(tape_type)))
		  goto found_option;
		else 
		  return;			 /* ignore this line */
	    }
	}
	if (!strcasecmp(s, &ap[c].pref_name[2])) {/* ignore leading '--' */
	found_option:;
	    if (ap[c].option_1 == NULL) return;	 /* ignore NULLs */
	    switch(ap[c].handle) {
	     case 'T':
		if (tape_type == -1)		 /* only set if not already set */
		  if (set_tape_type(val) == -1) 
		    do_exit(ERROR_ILLEGAL_TAPE_TYPE);
		break;
	     case 'S':				 /* a string type */
		strcpy(ap[c].option_1, val);
		if (ap[c].option_2) strcpy(ap[c].option_2, val);
		break;
	     case 'I':				 /* an integer type */
		*((_s32 *) ap[c].option_1) = atoi(val);
		break;
	     case 'Y':				 /* YES/NO */
		if ((*val == 't') || (*val == 'T') || (*val == 'Y') || (*val == 'y'))
		  *((_s32 *) ap[c].option_1) = 1;
		else {
		    if ((*val == 'f') || (*val == 'F') || (*val == 'n') || (*val == 'N')) {
			*((_s32 *) ap[c].option_1) = 0;
		    }
		    else
		      do_exit(ERROR_PREF_FORMAT);
		}
		break;
	     case 'C':				 /* colour */
		if (evcol_pair(val, ap[c].option_1, ap[c].option_2) == -1)
		  do_exit(ERROR_PREF_FORMAT);
		break;
	    }
	    return;
	}
	c++;
    }
    do_exit(ERROR_PREF_FORMAT);		 /* unknown preference line */
}


PRIVATE _errstat check_a(int argc, int *c)
{
/* Increments c and makes sure it is less than argc 
 * 1 if OK, 0 if not */
    (*c)++;
    if (*c == argc) {
	usage();
	return 0;
    }
    return 1;
}


PRIVATE void read_prefs_file(int argc, char *argv[]) 
{
/* Looks for a preferences  file:
 * 
 * First checks to see if there is a -p argument
 * If not, then looks for TAPER_CONFIG environment variable
 * If none, then looks for file .taper_prefs in $HOME
 * If none, then looks for file .taper_prefs in DEFAULT_GLOBAL_PREFS (/usr/local/etc)
 * If not`found, then use internal defaults
 * 
 * Note: Command line options over-ride anything in a file or internal defaults
*/ 
    int c;
    FILE *fd;
    char s[1000];
    int line=0;

    strcpy(pref_file, getenv("HOME")); strcat(pref_file, "/");
    strcat(pref_file, DEFAULT_PREFS);		 /* default preference filename*/
    *pref_file = 0;
    c=1;					 /* check if command line name given */
    while (c < argc) {
	if (!strcasecmp(argv[c], "-p") || !strcasecmp(argv[c], "--preference-file")) {
	    strcpy(pref_file,argv[c+1]);
	    break;
	}
	if (!strcasecmp(argv[c], "-T") || !strcasecmp(argv[c], "--tape-type")) {
	    if (!check_a(argc, &c)) {usage(); do_exit(ERROR_USAGE);}
	    if (set_tape_type(argv[c]) == -1)
	      do_exit(ERROR_ILLEGAL_TAPE_TYPE);
	    continue;
	}
	c++;
    }
    if (!(*pref_file)) 				 /* no command line specified */
      if (getenv(PREFS_ENVIRON))		 /* see if environment specified */
        strcpy(pref_file, getenv(PREFS_ENVIRON));
    if (!*pref_file) {				 /* no environment specified */
	strcpy(pref_file, getenv("HOME"));
	strcat(pref_file, "/"); 
	strcat(pref_file, DEFAULT_PREFS);
	fd = fopen(pref_file, "r");
	if (fd == NULL)	{			 /* couldn't open this */
	    strcpy(pref_file, DEFAULT_GLOBAL_PREFS); /* therefore look in global */
	    strcat(pref_file, "/");
	    strcat(pref_file, DEFAULT_PREFS);
	}
	else
	  fclose(fd);				 /* close what we just opened */
    }
    
    if (!*pref_file) 				 /* if still no filename */
      return;
    
    if ((fd=fopen(pref_file, "r")) == NULL) {	 /* error opening config file */
	if (errno != ENOENT)			 /* allowed to have file not found */
	  do_exit(ERROR_CONFIGURATION);		 /* any other error */
	else {
	    strcpy(pref_file, getenv("HOME"));	 /* make default  */
	    strcat(pref_file, "/"); 		 /* ~/.taper_prefs */
	    strcat(pref_file, DEFAULT_PREFS);
	}
	return;
    }
    while (line++, !feof(fd)) {
	if (fgets(s, sizeof(s), fd) == NULL)
	  break;
	process_pref_line(s);			 /* process line according to template */
    }
    fclose(fd);
}


PRIVATE char on_or_off(char *arg)
{
/* returns 1 if the last part of the string arg is '-on' or returns 0
 * if last part is -off. Returns -1 if neither on/off */
  
    int s;

    s = strlen(arg);				 /* find end of string */
    if (!s) return -1;				 /* NULL string==error */
    while (--s) 				 /* look for - */
      if (arg[s] == '-') break;
    if (!s) return -1;				 /* no dash found */
    if (!strcasecmp(&arg[s], "-off")) return 0;	 
    if (!strcasecmp(&arg[s], "-on")) return 1;
    return -1;					 /* neither on/off */
}


PRIVATE void parse_args(int argc, char *argv[])
{
/* Does parsing of args common to all progs
   Exits if an error occurred
*/
  int c=1, c1;

  while (c < argc) {
      c1=0;
      while (ap[c1].cln != ' ') {
	  if ((ap[c1].cln == argv[c][1]) || 	 /* short command line  */
	      (!strncasecmp(argv[c], ap[c1].pref_name, strlen(ap[c1].pref_name)))) {
	      switch (ap[c1].handle) {		 /* name matched */
	       case 'T':
		  if (!check_a(argc, &c)) goto usage;
		  if (set_tape_type(argv[c]) == -1)
		    do_exit(ERROR_ILLEGAL_TAPE_TYPE);
		  break;
	       case 'Y':
		  if (ap[c1].cln == argv[c][1])	 /* short option */
		    *((char *) ap[c1].option_1) = (argv[c][0] == '-') ? 0 : 1;
		  else
		    *((char *) ap[c1].option_1) = on_or_off(argv[c]);
		  if (*((char *) ap[c1].option_1) == -1) goto usage;
		  break;
	       case 'S':
		  if (!check_a(argc, &c)) goto usage;
		  strcpy((char *) ap[c1].option_1, argv[c]);
		  if (ap[c1].option_2) strcpy((char *) ap[c1].option_2, argv[c]);
		  break;
	       case 'I':
		  if (!check_a(argc, &c)) goto usage;
		  *((_s32 *) ap[c1].option_1) = atoi(argv[c]);
		  break;
	       case 'P':			 /* preference file */
		  if (!check_a(argc, &c)) goto usage;   /* skip past pref file name */
		  break;
	       case 'C':			 /* colour - for future */
		  break;
	       case 'A':			 /* archive difference */
		  if (!check_a(argc, &c)) goto usage;
		  *((_s32 *) ap[c1].option_1) = atol(argv[c]);
		  if (*((_s32 *) ap[c1].option_1) == 0)
		    *((_s32 *) ap[c1].option_1) = -1;   /* autosearch */
		  no_windows = 1;			 /* tell not to open windows */
		  break;
	       case 'd':			 /* print directory */
		  if (!check_a(argc, &c)) goto usage;
		  *((_s32 *) ap[c1].option_1) = atol(argv[c]);
		  if (*((_s32 *) ap[c1].option_1) == 0)
		    *((_s32 *) ap[c1].option_1) = -1;/* autosearch */
		  no_windows = 1;		 /* tell not to open windows */
		  break;
	       case 'U':			 /* Unattended files */
		  if (!check_a(argc, &c)) goto usage;
		  no_windows = 1;
		  unattended = 1;
		  break;
	       case 'v':
		  fprintf(stderr, "Version is %s\n", CUR_VERSION);
#ifdef TRIPLE_BUFFER
		  fprintf(stderr, "Compiled with TRIPLE_BUFFER\n");
#endif
		  no_windows = 1;
		  do_exit(ERROR_NONE);
		  return;
	      }
	      break;
	  }
	  else
	    c1++;
      }
      if (ap[c1].cln == ' ') goto usage;	 /* didn't find option */
      c++;
  }
    return;
    
    usage:
      usage();
      do_exit(ERROR_USAGE);
}


PUBLIC _errstat fill_in_defaults()
{
/* Looks to see if any tape drive dependent defaults have been
 * missed and fills them in according to the default 
 * 
 * Returns -1 if error, 0 otherwise
 
 */
    switch(tape_type) {
     case -1:
	return do_exit(ERROR_NO_TAPE_TYPE);
     case TAPE_TYPE_FTAPE:
	if (erase_tape == -1) erase_tape = 1;
	if (block_size == -1) block_size = FTAPE_DEFAULT_BLOCK_SIZE;
	if (*tape == -1) strcpy(tape, FTAPE_DEFAULT_TAPE);
	if (*ntape == -1) strcpy(ntape, FTAPE_DEFAULT_NTAPE);
	if (fast_fsf == -1) fast_fsf = 0;
	if (can_seek == -1) can_seek = 0;
	if (set_blksize == -1) set_blksize = 0;
	if (get_blksize == -1) get_blksize = 0;
	have_rewind = 1;
	have_fsf = 1;
	break;
     case TAPE_TYPE_ZFTAPE:
	if (erase_tape == -1) erase_tape = 1;
	if (block_size == -1) block_size = ZFTAPE_DEFAULT_BLOCK_SIZE;
	if (*tape == -1) strcpy(tape, ZFTAPE_DEFAULT_TAPE);
	if (*ntape == -1) strcpy(ntape, ZFTAPE_DEFAULT_NTAPE);
	if (fast_fsf == -1) fast_fsf = 1;
	if (set_blksize == -1) set_blksize = 1;
	if (get_blksize == -1) get_blksize = 1;
	if (can_seek == -1) can_seek = 1;
	have_rewind = 1;
	have_fsf = 1;
	break;
     case TAPE_TYPE_SCSI:
	if (erase_tape == -1) erase_tape = 0;
	if (block_size == -1) block_size = SCSI_DEFAULT_BLOCK_SIZE;
	if (*tape == -1) strcpy(tape, SCSI_DEFAULT_TAPE);
	if (*ntape == -1) strcpy(ntape, SCSI_DEFAULT_NTAPE);
	if (fast_fsf == -1) fast_fsf = 1;
	if (can_seek == -1) can_seek = 1;
	if (set_blksize == -1) set_blksize = 0;
	if (get_blksize == -1) get_blksize = 0;
	have_rewind = 1;
	have_fsf = 1;
	break;
     case TAPE_TYPE_FILE:
	erase_tape = 0;
	if (block_size == -1) block_size = FILE_DEFAULT_BLOCK_SIZE;
	if (*tape == -1) strcpy(tape, "");
	if (*ntape == -1) strcpy(ntape, "");
	can_seek = 1;
	fast_fsf = 0;
	set_blksize = 0;
	get_blksize = 0;
	have_rewind = 1;
	have_fsf = 0;
	break;
     case TAPE_TYPE_REMOVABLE:
	if (erase_tape == -1) erase_tape = 0;
	if (block_size == -1) block_size = REMOVABLE_DEFAULT_BLOCK_SIZE;
	if (*tape == -1) strcpy(tape, REMOVABLE_DEFAULT_TAPE);
	if (*ntape == -1) strcpy(ntape, REMOVABLE_DEFAULT_NTAPE);
	fast_fsf = 0;
	if (can_seek == -1) can_seek = 0;
	set_blksize = 0;
	get_blksize = 0;
	have_rewind = 0;
	have_fsf = 0;
	break;
     case TAPE_TYPE_IDE:
	if (erase_tape == -1) erase_tape = 1;
	if (block_size == -1) block_size = IDE_DEFAULT_BLOCK_SIZE;
	if (*tape == -1) strcpy(tape, IDE_DEFAULT_TAPE);
	if (*ntape == -1) strcpy(ntape, IDE_DEFAULT_NTAPE);
	if (fast_fsf == -1) fast_fsf = 1;
	if (can_seek == -1) can_seek = 0;
	if (set_blksize == -1) set_blksize = 0;
	if (get_blksize == -1) get_blksize = 0;
	have_rewind = 1;
	have_fsf = 1;
	break;
    }
    return 0;
}


PRIVATE void setup_colors(void)
{
/* Sets up color */
    
    if (has_colors()) {
	init_pair(COLOR_MAIN, color_main_foreground, color_main_background);
	init_pair(COLOR_DIALOG, color_dialog_foreground, color_dialog_background);
	init_pair(COLOR_STATUS, color_status_foreground, color_status_background);
	init_pair(COLOR_HELP, color_help_foreground, color_help_background);
	init_pair(COLOR_DIRECTORY, color_directory_foreground, color_directory_background);
	init_pair(COLOR_ONVOL, color_onvol_foreground, color_onvol_background);
	init_pair(COLOR_SELECTED, color_selected_foreground, color_selected_background);
	init_pair(COLOR_BOTTOM, color_bottom_foreground, color_bottom_background);
	init_pair(COLOR_TITLE, color_title_foreground, color_title_background);
	init_pair(COLOR_FORM, color_form_foreground, color_form_background);
    }
}


PRIVATE void num_to_string(_s32 num, int rev, int top, int line, char *strings[])
{
/* Converts an option to a string. Num is the string assumed to
 * be selected. Strings points to an array of strings which is
 * NULL terminated. Maximum of 10 options */
    
    int c1, ns=0, stlength=0, pib, c=0;
    int sp[10];
    
    while (strings[ns]) {			 /* count # strings and */
	stlength+= strlen(strings[ns]);		 /* total string lengths */
	ns++;
    }
    pib = min((42 - stlength)/ns, 5);		 /* spaces between each string */
    c=33;
    for (c1=0; c1<ns;c1++) {			 /* calculate pos of each string */
	sp[c1]=c;
	c+=pib+strlen(strings[c1]);
    }
      
    wattroff(win_main, A_REVERSE);
    for (c1=0;c1<ns;c1++) {
	if (num == c1)  {
	    wattron(win_main, A_UNDERLINE);
	    if (rev)
	      wattron(win_main, A_REVERSE);
	}
	mvwaddstr(win_main, top+line*2, sp[c1], strings[c1]);
	wattroff(win_main, A_UNDERLINE);
	wattroff(win_main, A_REVERSE);
    }
}


PRIVATE void print_pref_line(struct pref_info options[], int c, int top, int rev)
{
    char *py[]={"NO", "YES", NULL};
    char *pl[]={"OFF", "ERRORS", "INFO", "VERBOSE", "ALL", NULL};
    char *pc[]={"NONE", "E GZIP", "I COMPRESS", "I GZIP", NULL};
    char *po[]={"OFF", "MORE RECENT", "ALWAYS", NULL};
    char *pt[]={"FTAPE", "ZFTAPE", "SCSI", NULL};
    
    switch(options[c].type) {
       case 'I':				 /* integer */
	if (rev) wattron(win_main, A_REVERSE);
	mvwprintw(win_main, top+c*2, 33, "%02d", *((_s32 *) (options[c].opt))) ; 
	wattroff(win_main, A_REVERSE);
	break;
       case 'B':				 /* block size */
        if (rev) wattron(win_main, A_REVERSE);
	mvwprintw(win_main, top+c*2, 33, "%03dK", *((_s32 *) (options[c].opt))/1024) ; 
	wattroff(win_main, A_REVERSE);
	break;
     case 'Y':				 /* yes/no */
	num_to_string(*((char *) options[c].opt), rev, top, c, py);
	break;
     case 'L':				 /* log level */
	num_to_string(*((char *) options[c].opt), rev, top, c, pl);
	break;
     case 'C':				 /* compression type */
	num_to_string(*((char *) options[c].opt), rev, top, c, pc);
	break;
       case 'O':				 /* overwrite level */
	num_to_string(*((char *) options[c].opt), rev, top, c, po);
	break;
     case 'T':
	 num_to_string(*((char *) options[c].opt), rev, top, c, pt);
	break;
    }
}


PRIVATE void print_option_screen(struct pref_info *options, int top, int count, int allab)
{
    int c;
    
    for (c=0; c<count; c++) {
	wattron(win_main, A_BOLD);
	mvwaddstr(win_main, top+c*2, 30-strlen(options[c].name), options[c].name);
	wattroff(win_main, A_BOLD);
	print_pref_line(options, c, top, 0);
    }
    (allab) ?
      centre(win_main, win_main->_maxy-1, "Arrow keys to move, F5 abort, ESC finish", COLOR_FORM) :
      centre(win_main, win_main->_maxy-1, "Arrow keys to move, ESC when finished", COLOR_FORM);
}
		      

PRIVATE void do_left(void *v, _s32 max)
{
/* Decrements v - if less than zero, sets to max */
    char *c = (char *) v;
    
    (*c)--;
    if (*c<0) *c=max;
}

PRIVATE void change_tt_defaults(struct pref_info *options, int top, _s8 allab, FIELD *fields[])
{
/* See if user wants new defaults based on this tape drive */    
    int c=0, c1;

    while (options[c].name)			 /* count # options */
	c++;
    print_option_screen(options, top, c, allab);
    if (!message_box("Change to type defaults", MB_YESNO))
      return;
    erase_tape = -1; block_size = -1;
    *tape = -1; *ntape = -1;
    c1=0;
    while (c1<c) {
	if (options[c1].opt == (void *) tape)
	  set_field_buffer(fields[c1], 0, options[c1].opt);
	if (options[c1].opt == (void *) ntape)
	  set_field_buffer(fields[c1], 0, options[c1].opt);
	c1++;
    }
    fast_fsf = -1; set_blksize = -1;
    get_blksize = -1;
    fill_in_defaults();				 /* reset defaults */
    print_option_screen(options, top, c, allab);
    touchwin(win_main); wrefresh(win_main);
}


PRIVATE void option_left(struct pref_info *options, _s32 op, int top, _s8 allab, FIELD *fields[])
{
    switch(options[op].type) {
     case 'T':
	do_left(options[op].opt, 3);
	change_tt_defaults(options, top, allab, fields);
	break;
     case 'L':
	do_left(options[op].opt, 5);
	break;
     case 'B':
	block_size -= 1024;
	if (block_size < 0) 
	  block_size = 0;
	break;
     case 'C':
	do_left(options[op].opt, 3);
	break;
     case 'O':
	do_left(options[op].opt, 3);
	break;
     case 'Y':
	do_left(options[op].opt, 2);
	break;
     case 'I':
	*((_s32 *) options[op].opt) = *((_s32 *) options[op].opt) -1;
	if (*((_s32 *) options[op].opt) < 0)
	  *((_s32 *) options[op].opt) = 99;
	break;
    }
    print_pref_line(options, op, top, 1);
}

	  
PRIVATE void do_right(void *v, _s32 max)
{
/* Increments v - if greater than max, sets to 0 */
    char *c = (char *) v;
    
    (*c)++;
    if (*c>max) *c=0;
}
      
PRIVATE void option_right(struct pref_info *options, _s32 op, int top, _s8 allab, FIELD *fields[])
{
    switch(options[op].type) {
     case 'T':
	do_right(options[op].opt, 3);
	change_tt_defaults(options, top, allab, fields);
	break;
     case 'L':
	do_right(options[op].opt, 4);
	break;
     case 'C':
	do_right(options[op].opt, 3);
	break;
     case 'O':
	do_right(options[op].opt, 3);
	break;
     case 'Y':
	do_right(options[op].opt, 2);
	break;
     case 'B':
	block_size += 1024;
	if (block_size < 0) 
	  block_size = 0;
	break;
     case 'I':
	*((_s32 *) options[op].opt) = *((_s32 *) options[op].opt) + 1;
	if (*((_s32 *) options[op].opt) == 100)
	  *((_s32 *) options[op].opt) = 0;
	break;
    }
    print_pref_line(options, op, top, 1);
}

	
PRIVATE int do_form_driver(FORM *form, FIELD *fields[], int c, int cf, int count, struct pref_info *options)
{
/* cf is the field in options we want to edit */
/* Will return -1 if processed key, otherwise returns key */
    int k, ff=0;
    
    for (k=0; k<cf; k++) 			 /* convert cf into field on form */
      if ((options[k].type == 'S') || (options[k].type == 'N'))
        ff++;
    if (!(current_field(form) == fields[ff]))
      set_current_field(form, fields[ff]);
    switch(c) {
     case KEY_RIGHT:
	c = REQ_NEXT_CHAR; break;
     case KEY_LEFT:
	c = REQ_PREV_CHAR; break;
     case KEY_HOME:
	c = REQ_BEG_FIELD; break;
     case KEY_END:
	c = REQ_END_FIELD; break;
     case KEY_NPAGE:
	c = REQ_NEXT_CHOICE; break;
     case '\r':
     case '\n':
     case KEY_DOWN:
	return KEY_DOWN;
     case KEY_UP:
	return KEY_UP;
     case KEY_PPAGE:
	c = REQ_PREV_CHOICE; break;
     case KEY_DC:
	c = REQ_DEL_CHAR; break;
     case KEY_BACKSPACE:
     case 127:
	c = REQ_DEL_PREV; break;
    }
    form_driver(form, c);
    return -1;
}


PUBLIC _errstat taper_change_prefs(struct pref_info *options, _s8 allab)
{
/* allab == 1 --> allow F5 as abort 
 * 
 * returns 1 if OK, -1 if aborted
 
 */
    int count, top, c, k, k1;
    WINDOW *help;
    FIELD *fields[30];
    FORM *form;
    char  tmp[100];
    int   *intp;

    count=0;					 /* count number options */
    while (options[count].name) {
	count++;
    }
    
    top = (win_main->_maxy - (count * 2)) / 2;	 /* print screen template */
    k1=0;
    for (c=0; c<count;c++) {			 /* now setup forms fields */
	if (options[c].type == 'S') {
	    fields[k1] = new_field(1, 40, top+c*2, 33, 15, 0);
	    field_opts_off(fields[k1], O_STATIC); 
	    set_field_back(fields[k1], COLOR_PAIR(COLOR_FORM)|A_REVERSE);
	    set_max_field(fields[k1], 0); 
	    set_field_buffer(fields[k1], 0, options[c].opt);
	    k1++;
	}
	if (options[c].type == 'N') {
	    fields[k1] = new_field(1, 40, top+c*2, 33, 1, 0);
	    field_opts_off(fields[k1], O_STATIC); 
	    set_field_back(fields[k1], COLOR_PAIR(COLOR_FORM)|A_REVERSE);
	    set_max_field(fields[k1], 0); 
	    intp = options[c].opt;		 /* this done to avoid */
	    sprintf(tmp, "%d", *intp);		 /* warnings */
	    set_field_buffer(fields[k1], 0, tmp);
	    k1++;
	}
    }

    fields[k1] = NULL;
    form=new_form(fields);
    set_form_win(form, win_main);  
    my_werase(win_main, COLOR_FORM); 
    post_form(form);
    print_option_screen(options, top, count, allab);
    c=0;
    print_pref_line(options, c, top, 1);
    curs_set( ((options[c].type == 'S') || (options[c].type == 'N')) ? 1: 0);
    wrefresh(win_main);
    wmove(win_main, top+c*2, 33);
    while (1) {
	k = wgetch(win_main);
	if ( ((options[c].type == 'S') || (options[c].type == 'N')) && 
	    (k != EXIT_KEY))
	  if (do_form_driver(form, fields, k, c, count, options) == -1)
	    continue;				 /* processed key */
	print_pref_line(options, c, top, 0);
	switch(k) {
	 case KEY_DOWN:
	 case '\n':
	 case '\r':
	    c++; 
	    break;
	 case KEY_UP:
	    c--; 
	    break;
	 case ABORT_KEY:
	    if (allab) return -1;
	    break;
	 case EXIT_KEY:
	    form_driver(form, REQ_VALIDATION);
	    k=0;
	    for (c=0; c<count;c++) {		 /* copy form */
		switch(options[c].type) {
		 case 'S':
		    my_strcpy(options[c].opt, field_buffer(fields[k], 0), 
			      options[c].length);   /* appropriate vars */
		    if (!strcmp(options[c].opt, " "))/* if space only */
		      *(char *) options[c].opt = 0;  /* make NULL */
		    k++;
		    break;
		 case 'N':
		    my_strcpy(tmp, field_buffer(fields[k], 0), sizeof(tmp));	 /* appropriate vars */
		    intp = options[c].opt;
		    *intp = atoi(tmp);
		    k++;
		    break;
		}
	    }
	    unpost_form(form);
	    free_form(form);
	    for (c=0; c<k1; c++)
	      free_field(fields[c]);
	    curs_set(0);
	    return 1;
	 case KEY_LEFT:
	    option_left(options, c, top, allab, fields); break;
	 case KEY_RIGHT:
	    option_right(options, c, top, allab, fields); break;
	 default:
	      help = my_newwin(13, curscr->_maxx-10,
			       (curscr->_maxy-13)/2,
			       10/2);
	      my_werase(help, COLOR_HELP);
	      box(help, ACS_VLINE, ACS_HLINE);
	      centre(help, 1, "Commands Available", COLOR_HELP);
	      centre(help, 2, "------------------", COLOR_HELP);
	      centre(help, 4, "Up arrow                    move cursor up                     ", COLOR_HELP);
	      centre(help, 5, "Down arrow                  move cursor down                   ", COLOR_HELP);
	      centre(help, 6, "Left arrow/Right arrow      change option                      ", COLOR_HELP);
	      centre(help, 7, "H, h, ?                     print this screen                  ", COLOR_HELP);
	      centre(help, 8, "ESC                         finish changing options            ", COLOR_HELP);
	    if (allab)
	      centre(help, 9, "F5                          abort                              ", COLOR_HELP);
	      centre(help,11, "Press any key...", COLOR_HELP);
	      wrefresh(help);
	      wgetch(help);
	      my_delwin(help);
	      print_option_screen(options, top, count, allab);
	      touchwin(win_main); wrefresh(win_main);
	      break;
	}
	if (c<0) c=count-1;
	if (c==count) c= 0;
	
	curs_set( ((options[c].type == 'S') || (options[c].type == 'N')) ? 1: 0);
	wmove(win_main, top+c*2, 33);
	print_pref_line(options, c, top, 1);
	wrefresh(win_main);
    }
    return -1;                         /* Should never get here */
}


PRIVATE char *tf(_s32 x)
{
    static char s[20];

    strcpy(s, "no");
    if (x) strcpy(s, "yes");
    return s;
}


PRIVATE char *oo(_s32 o)
{
    static char s[20];

    strcpy(s, "-off");
    if (o)
      strcpy(s, "-on");
    strcat(s, "  ");
    return s;
}


PRIVATE void taper_save_preference_file(void)
{
    struct pref_info o[] = { {"Preference file", pref_file, 'S', MAX_FNAME},
			     {NULL, NULL, 0}
		       };
    FILE *pf;
    char s[MAX_FNAME];
    int c=0;
    char tp;
    
    if (taper_change_prefs(o, 1) == -1)
      return;
    if (*pref_file != '/') {
	getcwd(s, sizeof(s));
	strcat(s, "/");
	strcat(s, pref_file);
    }
    else 
      strcpy(s, pref_file);
    pf = fopen(s, "w");
    if (pf == NULL) {
	do_exit(ERROR_CREATING_PREFS);
	return;
    }
    tp = *make_tt(tape_type);
    
    if (ap[c].save_pref) {
	while (ap[c].pref_name) {
	    switch(ap[c].handle) {
	     case 'S':
		if (ap[c].cln != 'b') {
		    if (ap[c].save_pref)
		      fprintf(pf, "%s = \"%s\"\n", &ap[c].pref_name[2], (char *) ap[c].option_1);
		    else
		      fprintf(pf, "%c-%s = \"%s\"\n", tp, &ap[c].pref_name[2], (char *) ap[c].option_1);
		}
		break;
	     case 'I':
	     case 'L':
		if (ap[c].save_pref)
		  fprintf(pf, "%s = %d\n", &ap[c].pref_name[2], *(_s32 *) ap[c].option_1);
		else
		  fprintf(pf, "%c-%s = %d\n", tp, &ap[c].pref_name[2], *(_s32 *) ap[c].option_1);
		break;
	     case 'Y':
		if (ap[c].save_pref) 
		  fprintf(pf, "%s = %s\n", &ap[c].pref_name[2], tf(*(char *) ap[c].option_1));
		else
		  fprintf(pf, "%c-%s = %s\n", tp, &ap[c].pref_name[2], tf(*(char *) ap[c].option_1));
		break;
	     case 'T':
		if (ap[c].save_pref)
		  fprintf(pf, "%s = %s\n", &ap[c].pref_name[2], make_tt(*(char *) ap[c].option_1));
		else
		  fprintf(pf, "%c-%s = %s\n", tp, &ap[c].pref_name[2], make_tt(*(char *) ap[c].option_1));
		break;
	     case 'C':
		if (ap[c].save_pref)
		  fprintf(pf, "%s = %s\n", &ap[c].pref_name[2], color_string(*(char *) ap[c].option_1, *(char *) ap[c].option_2));
		else
		  fprintf(pf, "%c-%s = %s\n", tp, &ap[c].pref_name[2], color_string(*(char *) ap[c].option_1, *(char *) ap[c].option_2));
		break;
	    }
	c++;
	}
    }
    fclose(pf);
}


PRIVATE void taper_save_command_line(void)
{
    char s[MAX_FNAME];
    struct pref_info o[] = { {"Command line file", cf, 'S', MAX_FNAME},
			     {NULL, NULL, 0}
		       };
    FILE *pf;
    int  c=0;

    strcpy(cf, "start_taper");
    if (taper_change_prefs(o, 1) == -1)
      return;
    if (*cf != '/') {
	getcwd(s, sizeof(s));
	strcat(s, "/");
	strcat(s, cf);
    }
    pf = fopen(s, "w");
    if (pf == NULL) {
	do_exit(ERROR_CREATING_COMLINE);
	return;
    }
    fprintf(pf, "taper ");
    while (ap[c].pref_name) {
	switch(ap[c].handle) {
	 case 'S':
	    if (ap[c].cln != 'b')
	      fprintf(pf, "%s \"%s\"  ", ap[c].pref_name, (char *) ap[c].option_1);
	    break;
	 case 'I':
	 case 'L':
	    fprintf(pf, "%s %d  ", ap[c].pref_name, *(int *) ap[c].option_1);
	    break;
	 case 'Y':
	    fprintf(pf, "%s%s  ", ap[c].pref_name, oo(*(char *) ap[c].option_1));
	    break;
	 case 'T':
	    fprintf(pf, "%s %s  ", ap[c].pref_name, make_tt(*(char *) ap[c].option_1));
	    break;
	 case 'C':
	    fprintf(pf, "%s %s  ", ap[c].pref_name, color_string(*(char *) ap[c].option_1, *(char *) ap[c].option_2));
	    break;
	}
	c++;
    }
    fprintf(pf, "\n");
    fclose(pf);
}


PRIVATE void taper_init_windows(void)
{
/* Initialise main window and print screen edges etc..  */

    WINDOW *whole_screen;
    
    whole_screen = initscr();			 /* Initialize the cursor library */
    start_color(); setup_colors();		 /* setup colors */

    raw(); noecho(); cbreak();

    my_werase(stdscr, COLOR_MAIN); wrefresh(stdscr);
    
    screen_xlen=whole_screen->_maxx+1;		 /* Dimensions of screen */
    screen_ylen=whole_screen->_maxy;

    win_main = newwin(screen_ylen-1, screen_xlen, 1, 0); /* main working window */
    title = newwin(1, screen_xlen, 0, 0);	 /* title bar */
    bottom = my_newwin(1, screen_xlen, screen_ylen, 0);
    my_werase(win_main, COLOR_MAIN); my_werase(title, COLOR_TITLE); 
    my_werase(bottom, COLOR_BOTTOM);
    wrefresh(win_main); wrefresh(title); wrefresh(bottom);
    keypad(win_main, TRUE); keypad(title, TRUE);
    curs_set(0);
}

typedef void (*sighandler_t)(int);		 /* makes for clearer def */
    
PRIVATE void my_int_sig(int sig)
/* Handles the INT signal. Basically doesn't allow the user to interrupt -
   interrupting while backups are in progress is just too risky
*/
{
    signal(sig, my_int_sig);			 /* Linux restores orig signal behavious */
}


PRIVATE void my_sf_sig(int sig)
{
/* Handes a seg fault */
    signal(sig, SIG_DFL);			 /* to avoid recursion if further seg faults occur */
    my_werase(win_main, COLOR_MAIN); wmove(win_main, 1,1); wrefresh(win_main);
    endwin();
    curs_set(1);
    if (sig == SIGSEGV)
      fprintf(stderr, "taper: segmentation fault.\nAttempting to clean up\n");
    else
      fprintf(stderr, "taper: abort signal.\nAttempting to clean up\n");
    my_free_all();
    if (shm)
      if (write_pid) kill(write_pid, SIGTERM);	 /* old write child still going */
    if (backup_child) kill(backup_child, SIGTERM);
    if (restore_child) kill(restore_child, SIGTERM);
    free_buffers(); 
    (sig == SIGSEGV) ?  write_log("ERROR: Segmentation fault") :
                       write_log("ERROR: Abort signal");
    write_log("\n\n");
    exit(-1);
}


PRIVATE void preferences_menu(void)
{
    int in_op=0;
    char *menu[] = {
      "Tape drive preferences", "Global preferences", 
	"Backup preferences 1", "Backup preferences 2",
	"Restore/Mkinfo preferences", "Unattended preferences",
	"Back to main menu", NULL};
    struct pref_info tapedrive_options[] = {
/*	{"Tape drive tape", &tape_type, 'T', 0}, */
	{"Rewinding device", tape, 'S', MAX_FNAME},
	{"Non-rewinding device", ntape, 'S', MAX_FNAME},
	{"Have fast fsf", &fast_fsf, 'Y', 0},
	{"Can seek", &can_seek, 'Y', 0},  
	{"Set block size", &set_blksize, 'Y', 0},
	{"Get block size", &get_blksize, 'Y', 0},
	{"Block size", &block_size, 'B', 0},
	{"Need to erase tapes", &erase_tape, 'Y', 0},
	{"Tape size in MB", &tape_size, 'N', 0},
	{NULL, NULL, 0, 0}
    };
    struct pref_info global_options[] = { 	
	{"Memory tight", &memory_tight, 'Y', 0},
	{"Info file directory", taper_info_files, 'S', MAX_ARCHIVETITLE},
	{"Temporary files directory", temp_dir, 'S', MAX_FNAME},  
	{"Log file", log_file, 'S', MAX_FNAME},
	{"Log level", &log_level, 'L', 0},
	{"Max log size", &log_file_size, 'I', 0},
	{"Strip path", &ostrip, 'I', 0},
	{"Prompt directories", &dir_selection, 'Y', 0},
	{"Alpha sort archives", &sort_dir, 'Y', 0},
	{"/proc device", &proc_dev, 'I', 0},
	{NULL, NULL, 0, 0}
    };
    struct pref_info backup_options_1[] = { {"Archive title", archive_title, 'S', MAX_ARCHIVETITLE},
	{"Volume title", volume_title, 'S', MAX_ARCHIVETITLE},
	{"Exclude compress", &exclude_compress, 'S', MAX_FNAME},
	{"Exclude files", &exclude_files, 'S', MAX_FNAME},
        {"Exclude dirs", &exclude_dirs, 'S', MAX_FNAME},
	{"Compress", &compression, 'C', 0},
	{"Compression head start", &comp_head_start, 'I', 0},
	{"Hard links", &hard_links, 'Y', 0},
	{NULL, NULL, 0, 0}
    };
    struct pref_info backup_options_2[] = { 
	{"Compress info files", &compress_info, 'Y', 0},  
	{"Limit to one filesystem", &ofs, 'Y', 0},  
	{"Default incremental backup", &incremental, 'Y', 0},
	{"Free space for compress", &min_free, 'N', 0},  
/*	{"Write info at end of tape", &info_end, 'Y', 0}, not yet */
	{NULL, NULL, 0, 0}
    };
    struct pref_info restore_options[]= { {"Restore path", rel_path, 'S', MAX_FNAME},
	{"Overwrite level", &ovrwrite, 'O', 0},
	{"Only volume", &only_vol, 'I', 0},
	{"Prompt for archives", &prompt_archive, 'Y', 0},
	{"Default recent restore", &most_recent, 'Y', 0},
	{"Auto descend directories", &auto_descend, 'Y', 0},
	{"Prompt if bad checksum", &bad_checksum, 'Y', 0},
	{"Min blocks before seeking", &min_seek, 'I', 0},
	{"Using pre-5.6 archive", &old_archive, 'Y', 0},  
	{NULL, NULL, 0, 0}
    };
    
    struct pref_info unattended_options[]= { {"Append files", &append, 'Y', 0},
	{"Overwrite unrecognized tapes", &tape_overwrite, 'Y', 0},
	{NULL, NULL, 0, 0}
    };
    

    while (1) {
	clear_main();
	init_common_vars();			 /* set all vars */
	switch (select_menu(win_main, menu, &in_op)) {
	 case 0:					 /* global options */
	    taper_change_prefs(tapedrive_options, 0);
	    break;
	 case 1:					 /* global options */
	    taper_change_prefs(global_options, 0);
	    break;
	 case 2:
	    taper_change_prefs(backup_options_1, 0);
	    break;
	 case 3:
	    taper_change_prefs(backup_options_2, 0);
	    break;
	 case 4:
	    taper_change_prefs(restore_options, 0);
	    break;
	 case 5:
	    taper_change_prefs(unattended_options, 0);
	    break;
	 case 6:					 
	    return;
	}
    }
}


void utils_mktape(void);
void utils_testzero(void);
void utils_testfast_fsf(void);
void utils_whereproc(void);
void utils_erasevols(void);
void utils_test_mktape(void);
void utils_test_can_seek(void);
void utils_testlinks(void);
PRIVATE void utilities(void)
{
    char *menu[] = {
	"Make tape",  "Where's proc", 
	  "Test fast fsf", "Test make tape",
	  "Test can seek",
	  "Erase volumes", "Look for recursive links",
	  "Back to main menu", NULL};
    int in_op=0;
    
    while (1) {
	clear_main();
	switch (select_menu(win_main, menu, &in_op)) {
	 case 0:					 /* global options */
	    utils_mktape();
	    break;
	 case 1:
	    utils_whereproc();
	    break;
	 case 2:					 /* global options */
	    utils_testfast_fsf();
	    break;
	 case 3:
	    utils_test_mktape();
	    break;
	 case 4:
	    utils_test_can_seek();
	    break;
	 case 5:
	    utils_erasevols();
	    break;
	 case 6:
	    utils_testlinks();
	 case 7:
	    return;
	}
	my_delwin_all();				 /* delete any open windows */
	my_free_all();				 /* clear any memory still allocated */
    }
}


PRIVATE void save_prefs(void)
{
    char *menu[] = {
	"Save to preference file", "Save to command line file",
	"Return to main menu",  NULL};
    int in_op=0;
    
    while (1) {
	clear_main();
	switch (select_menu(win_main, menu, &in_op)) {
	 case 0:
	    taper_save_preference_file();
	    break;
	 case 1:
	    taper_save_command_line();
	    break;
	 case 2:
	    return;
	}
    }
}

	    
void taper_backup(int, char **);
void taper_restore(void);
void taper_verify(void);
void taper_mkinfo(void);
int main(int argc, char *argv[]) 
{

  int fin=0, in_op=0;
  char *menu[] = {
      "Backup Module", "Restore Module", "Recreate info file",
	"Verify archive", 
	"Utilities", "Change Preferences", "Save preferences", 
	"Exit", NULL};


  read_buffer = NULL;
  tr_buffer = NULL;
  shm = NULL;
  win_main = NULL;

  init_memory();				 /* set up my malloc manager */
  read_environment();				 /* read environment */
  read_prefs_file(argc, argv);			 /* read preferences */
  parse_args(argc, argv);			 /* read in command line args */
  if (fill_in_defaults() == -1) return -1;
  if (init_buffers(0) == -1) return -1;
  org_block_size=block_size;	                 /* save original block size */
  if (!no_windows) taper_init_windows();	 /* Draw windows */
  signal(SIGINT, my_int_sig);			 /* trap INT signal */
  signal(SIGSEGV, my_sf_sig);			 /* trap seg fault signal */
  signal(SIGABRT, my_sf_sig);			 /* trap seg fault signal */
  getcwd(cur_dir, sizeof(cur_dir));		 /* save current directory for backup */
  getcwd(original_cur_dir, sizeof(original_cur_dir)); /* for restore */
  if (no_windows) {
      init_common_vars();
      if ((pr_dir) || (diff_id)) taper_restore();
      if (unattended) taper_backup(argc, argv);
  }
  else {
      my_init_windows();			 /* initialise windows for subsystems */
      while (!fin) {
	  clear_main();
	  init_common_vars();			 /* set all vars */
	  switch (select_menu(win_main, menu, &in_op)) {
	   case 0:					 /* backup */
	      taper_backup(argc, argv);
	      break;
	   case 1:					 /* restore */
	      restore_mode = RESTORE_FULL;
	      taper_restore();
	      break;
	   case 2:					 /* mkinfo */
	      taper_mkinfo();
	      break;
	   case 3:
	      taper_verify();
	      break;
	   case 4:
	      utilities();
	      break;
	   case 5:
	      block_size=org_block_size;	 /* save original block size */
	      preferences_menu();
	      org_block_size = block_size;
	      break;
	   case 6:
	    save_prefs();
	    break;
	   case 7:					 /* exit */
	      fin=1;
	      break;
	  };
	  if (backup_child) {			 /* if there is a child running */
	      kill(backup_child, SIGTERM);	
	      backup_child = 0;
	  }
	  if (restore_child) {			 /* if there is a child running */
	      kill(restore_child, SIGTERM);	
	      restore_child = 0;
	  }
	  my_delwin_all();				 /* delete any open windows */
	  my_free_all();				 /* clear any memory still allocated */
      }
      werase(win_main); wrefresh(win_main);
      werase(title); wrefresh(title);
      werase(bottom); wrefresh(bottom);
      delwin(win_main); delwin(title); delwin(bottom);
      endwin();
      curs_set(1);
  }
    if (shm)
      if (write_pid) kill(write_pid, SIGTERM);	 /* old write child still going */
    free_buffers();
    signal(SIGINT, SIG_DFL);			 /* restore signals */
    signal(SIGSEGV, SIG_DFL);
    signal(SIGABRT, SIG_DFL);
    return 0;			                 /* succesful return */
}
