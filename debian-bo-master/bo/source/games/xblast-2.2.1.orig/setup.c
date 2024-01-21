/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: setup.c 
 * setup for xblast
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Intrinsic.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>
#define _XSETUP_C
#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "maze.h"
#include "resource.h"
#include "defaults.h"
#include "pipe.h"
#include "data.h"
#include "main.h"

/*
 * extern varaiables
 */ 
extern Display *dpy[MAX_DISPLAY];

/*
 * local varaiables
 */
static char *prog_name = NULL;

/*
 * local function pointers
 */
#ifdef __STDC__
static XrmDatabase (*create_player_database)(XBConfig *config);
#else
static XrmDatabase (*create_player_database)();
#endif

/* Databases */
static XrmDatabase default_DB = NULL;
static XrmDatabase arg_DB = NULL;

static XrmDatabase disp_DB[MAX_PLAYER] = { 
  NULL, NULL, NULL, NULL, NULL
};

/*
 * public function dup_string
 * (not all systems have strdup)
 */
#ifdef __STDC__
static char *
dup_string (char* ptr, int length) 
#else
static char *
dup_string (ptr, length)
     char *ptr;
     int length;
#endif
{
  char *result;

  if (NULL == (result = malloc((length+1)*sizeof(char) ) ) ) {
    return NULL;
  }
  strncpy(result, ptr, length);
  result[length] = '\0';
  return result;
}



/*
 * local function sep_string
 * (not all systems have strsep)
 */
#ifdef __STDC__
static char *
sep_string (char *string, 
	    int sep)
#else
static char *
sep_string (string, sep)
     char *string; 
     int sep;
#endif
{
  /* look for separator */
  for (; *string != (char) sep; string++) {
    /* check for end of string */
    if (*string == '\0') {
      return NULL;
    }
  }
  *string = '\0';
  /* return position of second string */
  return ++string;
}



/*
 * local function equal_string 
 */
#ifdef __STDC__
static int
equal_string (char *s1, char *s2) 
#else
static int
equal_string (s1, s2)
     char *s1, *s2;
#endif
{
  if (s1 == NULL) {
    if (s2 == NULL) {
      return 1;
    }
    return 0;
  } 
  if (s2 != NULL) {
    return (0 == strcmp(s1, s2));
  } else {
    return 0;
  }
}


/*
 * local function numeric_string
 */
#ifdef __STDC__
static int
string_to_interval (char *string, 
		    int *left, 
		    int *right) 
#else
static int
string_to_interval (string, left, right)
     char *string;
     int *left, *right;
#endif
{
  char *ptr;
  int flag = FALSE;

  for (ptr = string; *ptr != '\0'; ptr++) {
    if (!isdigit(*ptr)) {
      if (flag || ('-' != *ptr) ) {
	return -1;
      } 
      flag = TRUE;
    }
  }

  if (flag) {
    /* string is an interval */
    sscanf(string, "%d-%d", left,right);
  } else {
    /* string is single number */
    *left = *right = atoi (string);
  }

#if DEBUG
  fprintf(stderr, "Scanned interval [%d,%d]\n",*left, *right);
#endif
  return 0;
}


#if 0
/*
 * local function numeric_string
 */
#ifdef __STDC__
static int
numeric_string (char *ptr) 
#else
static int
numeric_string (ptr)
     char *ptr;
#endif
{
  for (; *ptr != '\0'; ptr++) {
    if (!isdigit(*ptr)) {
      return 0;
    }
  }
  return 1;
}
#endif

/*
 * public function split_string
 */
#ifdef __STDC__
char ** 
split_string (char *string,
	      int *largc)
#else
char ** 
split_string (string, largc)
     char *string;
     int *largc;
#endif
{
  void *ptr;
  char *buf;
  char **argv;
  int i, flag, length, size, argc;

  /* get number of words */
  flag = TRUE;
  *largc = 0;
  size = 0;
  length = strlen(string);
  for (i=0; i<length; i++) {
    if (isspace(string[i])) {
      flag = TRUE;
    } else {
      if (flag) {
	(*largc) ++;
	flag = FALSE;
      }
      size ++;
    }
  }

  /* alloc array */
  if (NULL == (ptr = malloc( ((*largc)+1) * sizeof(char *) 
			    + (size+(*largc)) * sizeof(char) )) ) {
    fprintf(stderr, "string alloc failed\n");
    exit_prg(1);
  }
  
  /* now store strings */
  buf  = (char *) ((char **)ptr + ((*largc)+1));
  argv = (char **) ptr;
  argc = 0;
  flag=TRUE;
  for (i=0; i<length; i++) {
    if (isspace(string[i])) {
      if (!flag) {
	flag = TRUE;
	*buf = '\0';
	buf++;
      }
    } else {
      if (flag) {
	argv[argc]=buf;
	argc++;
	flag = FALSE;
      }
      *buf = string[i];
      buf++;
    }
  }
  *buf='\0';
  argv[argc]=NULL;
  

  return argv;
}



/*
 * local function db_get_bool
 */
#ifdef __STDC__
static int
db_get_bool (XrmDatabase DB, 
	     char *name, 
	     char *class)
#else
static int
db_get_bool (DB, name, class)
     XrmDatabase DB;
     char *name;
     char *class;
#endif
{
  XrmValue value;
  char *str_type[20];

#ifdef DEBUG_XRM
  fprintf (stderr, "Retrieving resource \"%s\": ", name);
#endif
  if (XrmGetResource(DB, name, class, str_type, &value)) {
    if (0 == strcasecmp(value.addr, "true") ) {
#ifdef DEBUG_XRM
      fprintf (stderr, "True\n");
#endif
      return 1;
    }
    if (0 == strcasecmp(value.addr, "on") ) {
#ifdef DEBUG_XRM
      fprintf (stderr, "True\n");
#endif
      return 1;
    }
    if (0 == strcasecmp(value.addr, "1") ) {
#ifdef DEBUG_XRM
      fprintf (stderr, "True\n");
#endif
      return 1;
    }
    if (0 == strcasecmp(value.addr, "yes") ) {
#ifdef DEBUG_XRM
      fprintf (stderr, "True\n");
#endif
      return 1;
    }
  } 
#ifdef DEBUG_XRM
  fprintf (stderr, "False\n");
#endif
  return 0;
}



#ifdef __STDC__
static char *
db_get_string (XrmDatabase DB, 
	       char *name, 
	       char *class)
#else
static char *
db_get_string (DB, name, class)
     XrmDatabase DB;
     char *name;
     char *class;
#endif
{
  XrmValue value;
  static char *str_type[20];

#ifdef DEBUG_XRM
  fprintf (stderr, "Retrieving resource \"%s\": ", name);
#endif
  if (XrmGetResource(DB, name, class, str_type, &value)) {
#ifdef DEBUG_XRM
    fprintf (stderr, "%s\n", value.addr);
#endif
    return dup_string (value.addr, value.size);
  } 
#ifdef DEBUG_XRM
  fprintf (stderr, "(null)\n");
#endif
  return NULL;
}



#ifdef __STDC__
static int
db_get_int (XrmDatabase DB, 
	    int min,
	    int max,
	    char *name, 
	    char *class)
#else
static int
db_get_int (DB, min ,max, name, class)
     XrmDatabase DB;
     int min, max;
     char *name;
     char *class;
#endif
{
  XrmValue value;
  char *str_type[20];
  int result;

#ifdef DEBUG_XRM
  fprintf (stderr, "Retrieving resource \"%s\": ", name);
#endif
  if (True == XrmGetResource(DB, name, class, str_type, &value)) {
#ifdef DEBUG_XRM
    fprintf (stderr, "%s->", value.addr);
#endif
    result = atoi (value.addr);
    if (result <min) {
      result = min;
    }
    if (result >max) {
      result = max;
    }
#ifdef DEBUG_XRM
    fprintf (stderr, "%d\n", result);
#endif
    return result;
  } 
  fprintf (stderr, "-1\n");
  return -1;
}



/*
 * local function query_bool_for_database
 */
#ifdef __STDC__
static int
query_bool_for_database (XrmDatabase *db,
			 char *res_name,
			 char *query_string)
#else
static int
query_bool_for_database (db, res_name, query_string)
     XrmDatabase *db;
     char *res_name;
     char *query_string;
#endif
{
  char zeile[1024];
  char *ptr, *last;
  int length;

  while (1) {
    printf("%s (y/n): ", query_string);
    fgets(zeile, 1024, stdin);
    
    /* check input format */
    length = strlen(zeile);
    last = zeile + length - 1;
    if (*last == '\n') {
      *last = '\0';
    }

    for (ptr = zeile; (ptr<last) && isspace(*ptr) ; ptr++);

    switch (*ptr) {
    case 'y':
    case 'Y':
    case '\0':
    case '\n':
      XrmPutStringResource(db, res_name, "true");
      return 1;

    case 'n':
    case 'N':
      XrmPutStringResource(db, res_name, "false");
      return 0;
    }
    printf ("\n");
  }
}



/*
 * local function query_int_for_database
 */
#ifdef __STDC__
static int
query_int_for_database (XrmDatabase *db,
			char *res_name,
			char *query_string,
			int min,
			int max,
			int def)
#else
static int
query_int_for_database (db, res_name, query_string, min, max, def)
     XrmDatabase *db;
     char *res_name;
     char *query_string;
     int min, max, def;
#endif
{
  char zeile[1024];
  int length, value;
  char *ptr, *start, *last;

  while (1) {
    printf("%s [%d-%d] (default is %d): ", query_string, min ,max ,def);
    fgets(zeile, 1024, stdin);
    /* check input format */
    length = strlen(zeile);
    last = zeile + length - 1;
    if (*last == '\n') {
      *last = '\0';
    }

    /* skip white spaces */
    for (ptr = zeile; ptr <= last && isspace(*ptr); ptr++);
    /* mark start of string */
    start = ptr;
    /* check for digits */
    for (; ptr <= last; ptr++) {
      if (!isdigit(*ptr)) {
	/* if white space or end of string is found */
	if ( (*ptr=='\0') || isspace(*ptr) ) {
	  /* set end of string */
	  *ptr = '\0';
	  /* check if string has non zero length */
	  if (start < ptr) {
	    /* write string to database */
	    value = atoi(start);
	    if ( (value >=min) && (value <=max) ) {
	      XrmPutStringResource (db, res_name, start);
	      return value;
	    }
	  } else {
	    /* use default value */
	    sprintf(zeile, "%d", def);
	    XrmPutStringResource (db, res_name, zeile);
	    return def;
	  }
	}
	ptr = last;
      }
    }
  }
}



/*
 * local function query_string_for_database
 */
#ifdef __STDC__
static char *
query_string_for_database (XrmDatabase *db,
			   char *res_name,
			   char *query_string,
			   char *def)
#else
static char *
query_string_for_database (db, res_name, query_string, def)
     XrmDatabase *db;
     char *res_name;
     char *query_string;
     char *def;
#endif
{
  char zeile[1024];
  int length;
  char *ptr, *last;

  if (def == NULL) {
    printf("%s : ", query_string);
  } else {
    printf("%s (default is \"%s\"): ", query_string, def);
  }
  fgets(zeile, 1024, stdin);
  /* eliminate first white spaces */
  length = strlen(zeile);
  last = zeile + length - 1;
  if (*last == '\n') {
    *last = '\0';
  }
  
  for (ptr = zeile; ptr != last && isspace(*ptr); ptr ++);
  if (ptr != last) {
    /* use input for database */
    XrmPutStringResource(db, res_name, ptr);
    return dup_string(ptr, strlen(ptr));
  } else if (def != NULL) {
    XrmPutStringResource(db, res_name, def);
    return def;
  } else {
    return NULL;
  }
}


/*
 * local function query_list_for_database
 */
#ifdef __STDC__
static char *
query_list_for_database (XrmDatabase *db,
			 char *resname,
			 char *query_string,
			 char **str_list)
#else
static char *
query_list_for_database (db, resname, query_string, str_list)
     XrmDatabase *db;
     char *resname;
     char *query_string;
     char **str_list;
#endif
{
  int nlist;
  char *result;

  printf("%s:\n", query_string);
  for (nlist = 0; str_list[nlist] != NULL; nlist ++) {
    printf(" [%d]\t%s\n", nlist+1, str_list[nlist]);
  }

  result = str_list[query_int_for_database
		    (db, "xblast.ignore", "Your choice", 1, nlist, 1) - 1];

  XrmPutStringResource(db, resname, result);
  
  return result;
}



/*
 * local function print_usage
 */
#ifdef __STDC__
static void
print_usage (char *prog, 
	     char *option)
#else
static void
print_usage (prog, option)
	     char *prog, *option;
#endif
{
  fprintf (stderr, "%s: unknown option \"%s\"\n", prog, option);
  fprintf (stderr, "Usage: %s [player[@display] ...] [options]\n", prog);
  fprintf (stderr, "Type \"man xblast\" for a list of options\n");
  exit_prg(1);
}



/*
 * local function set_defaults
 */
#ifdef __STDC__
static XrmDatabase 
set_defaults (BMEntry *ptr)
#else
static XrmDatabase 
set_defaults (ptr)
     BMEntry *ptr;
#endif
{
  XrmDatabase internal_DB = NULL;
  XrmDatabase appl_DB = NULL;

  /* get hardcoded defaults */
  for (; ptr->name != NULL; ptr++) {
    XrmPutStringResource(&internal_DB, ptr->name, ptr->value);
  }

  /* override with application defaults */
  appl_DB = XrmGetFileDatabase(file_appl_def);
  if (NULL == appl_DB) {
    fprintf(stderr, "Warning: failed to open file \"%s\"\n",file_appl_def);
  }
  XrmMergeDatabases (appl_DB, &internal_DB);

  return internal_DB;
}



/*
 * local function parse player config
 */
#ifdef __STDC__
static XrmDatabase
parse_player_config (int *argc,
		     char *argv[])
#else
static XrmDatabase
parse_player_config (argc, argv) 
     int *argc;
     char *argv[];
#endif
{
  int i,j;
  char *p_name, *d_name;
  char zeile[10];
  XrmDatabase db = NULL;

  for (i=1; (i<*argc) && (argv[i][0] != '-')  && (argv[i][0] != '+') ; i++) {
    if (i >MAX_PLAYER) {
      fprintf (stderr, "%s: Too many players defined. The maximum is %d\n",
	       prog_name, MAX_PLAYER);
      exit_prg(1);
    }
    /* get player and display name */
    p_name = argv[i];
    d_name = sep_string(p_name, '@');
    /* put both into data base */
    if (0 != strlen(p_name) ) {
      XrmPutStringResource (&db, D_Player[i-1], p_name);
    }
    if ( (NULL != d_name) && (0 != strlen(d_name)) ) {
      XrmPutStringResource (&db, D_AtDisplay[i-1], d_name);
    }
  }
  /* set number of players if not zero */
  if (i != 1) {
    sprintf(zeile,"%d", i-1);
#ifdef DEBUG
    printf("# players %d\n", i-1);
#endif
    XrmPutStringResource (&db, N_NumberOfPlayers, zeile);
  }

  /* correct arg list */
  *argc -= i-1;
  for (j=1; j < *argc; j++) {
    argv[j]=argv[i+j-1];
  }

  return db;
}


/*
 * local function parse_level_usage
 */
#ifdef __STDC__
static XrmDatabase 
parse_level_usage (int *argc,
		   char *argv[])
#else
static XrmDatabase 
parse_level_usage (argc, argv)
     int *argc;
     char *argv[];
#endif
{
  XrmDatabase dont_DB = NULL;
  XrmDatabase use_DB = NULL;
  char *flag;
  int i,j;
  int left, right, level;

  if (*argc < 2) {
    return NULL;
  }

  /* check argument */
  if (0 == strcmp("-u", argv[1]) ) {
    XrmPutStringResource(&use_DB, C_UseLevel, "False");
    flag = "True";
  } else if (0 == strcmp("-U", argv[1]) ) {
    for (level = 0; level <levelMax; level ++) {
      XrmPutStringResource(&dont_DB, get_level_res_name(level), "False");
    }
    flag = "True";
  } else if (0 == strcmp("+u", argv[1]) ) {
    XrmPutStringResource(&use_DB, C_UseLevel, "True");
    flag = "False";
  } else if (0 == strcmp("+U", argv[1]) ) {
    for (level = 0; level <levelMax; level ++) {
      XrmPutStringResource(&dont_DB, get_level_res_name(level), "True");
    }
    flag = "False";
  } else {
    return NULL;
  }

  for (i=2; (i<*argc) && ! string_to_interval(argv[i],&left,&right); i++) {
    for (level = left; level <=right; level ++) {
      if ( (level < 0) || (level > levelMax) ) {
	fprintf (stderr, "%s: level %d defined for \"%s\" out of range\n", 
		 prog_name, level, argv[1]);
	continue;
      }
      XrmPutStringResource(&use_DB, get_level_res_name(level), flag);
    }
  }

  if (2 == i) {
    fprintf (stderr,"%s: Warning! Define at least one level for option \"%s\"\n",
	     prog_name, argv[1]);
  }

  /* combine databases */
  XrmCombineDatabase (dont_DB, &use_DB, FALSE);

  /* correct arg list */
  i -= 1;
  for (j=1; j<(*argc-i); j++) {
    argv[j] = argv[j+i];
  }
  *argc -= i;

  return use_DB;
}


/*
 * local function check_create_dir
 */
#ifdef __STDC__
static int
check_create_dir (char *path)
#else
static int
check_create_dir (path)
     char *path;
#endif
{
  struct stat buf;

  /* check if setup dir exits */
  if (stat(path, &buf)) {
#ifdef DEBUG
    fprintf(stderr,"%s: Warning! Directory \"%s\" does not exist\n",
	    prog_name, path);
#endif
    /* create dir */
    if (mkdir(path, 0755)) {
      fprintf (stderr, "%s: Warning! Failed to create directory \"%s\"\n",
	       prog_name, path);
      return 1;
    }
    return 0;
  }
  /* check if buffer is directory (still has to be done) */
  
  return 0;
}



/*
 * local function list_setups
 */
#ifdef __STDC__
static char *
list_setups (void)
#else
static char *
list_setups ()
#endif
{
  char path[1024];
  char *result = NULL;
  char *home;
  DIR *dp;
  struct dirent *dirp;

  /* set path to setup dir */
  path[0] = '\0';
  if (NULL != (home = getenv("HOME") ) ) {
    strcpy(path, home);
  }
  strcat(path, file_setup_dir);
  /* open directory to read */
  if (NULL == (dp = opendir(path) )) {
    fprintf(stderr, "%s: Failed to read directory %s.\n",
	    prog_name, path);
    return NULL;
  }

  /* get list of setups */
  printf("List of named setups:\n");
  while (NULL != (dirp = readdir(dp) ) ) {
    if (dirp->d_name[0] != '.') {
      printf("  %s\n",dirp->d_name); 
      if (NULL == result) {
	result = dup_string(dirp->d_name, strlen(dirp->d_name) );
      }
    }
  }
  /* close directory */
  closedir(dp);

  return result;
}


#ifdef __STDC__
static int 
setup_exists (char *name)
#else
static int 
setup_exists (name)
     char *name;
#endif
{
  char *home;
  char path[1024];
  struct stat buf;
  
  /* set path name emtpy */
  path[0] = '\0';
  if (NULL != (home = getenv("HOME") ) ) {
    strcat(path, home);
  }
  strcat(path, file_setup_dir);
  strcat(path,"/");
  strcat(path,name);

#ifdef DEBUG
  fprintf(stderr, "Checking file \"%s\"\n", path);
#endif

  return (!stat(path, &buf));
}


/*
 * public function load_setup
 */ 
#ifdef __STDC__
static XrmDatabase 
load_setup (XrmDatabase DB)
#else
static XrmDatabase 
load_setup (DB)
     XrmDatabase DB;
#endif
{
  char *home, *fname;
  char path[1024];
  XrmDatabase db;

  /* set path name emtpy */
  path[0] = '\0';

  /* check load mode */
  if (NULL != (fname = db_get_string(DB, N_LoadNamedDefaults, 
				     C_LoadNamedDefaults) ) ) {
    /* load named default */
    if (NULL != (home = getenv("HOME") ) ) {
      strcpy(path, home);
    }
    strcat(path, file_setup_dir);
    if (check_create_dir(path)) {
      fprintf(stderr, "%s: Warning! Failed to load setup \"%s\"\n", 
	      prog_name, fname);
      return NULL;
    }
    strcat(path,"/");
    strcat(path,fname);
  } else if (db_get_bool(DB, N_LoadDefaults, C_LoadDefaults) ) {
    if (NULL != (home = getenv("HOME") ) ) {
      strcpy(path, home);
    }
    strcat(path, file_setup);
  } else {
    return NULL;
  }
#if DEBUG
  fprintf(stderr, "Reading Setup from file \"%s\"\n", path);
#endif
  
  if (NULL == (db = XrmGetFileDatabase(path)) ) {
    fprintf(stderr, "%s: Warning! Failed to read setup file %s\n",
	    prog_name, path);
  }
  return db;
}



/*
 * public function save_setup
 */ 
#ifdef __STDC__
void 
save_setup (XBConfig *config,
	    XBSettings *setup,
	    PlayerStrings *st)
#else
void 
save_setup (config, setup, st)
     XBConfig *config;
     XBSettings *setup;
     PlayerStrings *st;
#endif
{
  char *home;
  char *fname;
  FILE *fp;
  int i, num_player;
  char path[1024];
  static char* int_format = "%s: %d\n";
  static char* str_format = "%s: %s\n";

  /* check if setup has to be saved */
  if (CM_Child != config->com_mode) {
    if (NULL != (fname = db_get_string(default_DB, N_SaveNamedDefaults,
				       C_SaveNamedDefaults)))  {
      if (NULL != (home = getenv("HOME") ) ) {
	strcpy(path, home);
      }
      strcat(path, file_setup_dir);
      if (check_create_dir(path)) {
	fprintf(stderr, "%s: Warning! Failed to create setup dir\"%s\"\n", 
		prog_name, fname);
	return;
      }
      strcat(path,"/");
      strcat(path,fname);
    } else if (db_get_bool(default_DB, N_SaveDefaults, C_SaveDefaults) )  {
      /* set file name */
      if (NULL != (home = getenv("HOME") ) ) {
	strcpy (path, home);
      }
      strcat(path, file_setup);
    } else {
      return;
    }
    
    /* open file for output */
    if (NULL == (fp = fopen(path,"w"))) {
      fprintf (stderr, "%s: warning failed to open setup file \"%s\"\n",
	       prog_name, path);
      return;
    }
    
    /* write config and string resources */
    if (config->team_mode == TM_Double) { 
      num_player = config->num_player / 2;
    } else {
      num_player = config->num_player;
    }
    fprintf(fp, int_format, N_NumberOfPlayers,  num_player);
    for (i=0; i<num_player; i++) {
      if (NULL != config->display[config->pl_at_disp[i]]) {
	fprintf(fp, str_format, N_AtDisplay[i],  
		config->display[config->pl_at_disp[i]]);
      }
      fprintf(fp, str_format, N_Player[i], st[i].name);
    }
    switch (config->team_mode) {
    case TM_Single: fprintf(fp, str_format, N_TeamMode, "None");   break;
    case TM_Team:   fprintf(fp, str_format, N_TeamMode, "Team");   break;
    case TM_Double: fprintf(fp, str_format, N_TeamMode, "Double"); break;
    }
    fprintf(fp, str_format, N_Fork, config->fork ? "True" : "False" ); 

    /* write game setup */
    fprintf(fp, int_format, N_NumberOfVictories, setup->max_victories);
    fprintf(fp, int_format, N_NumberOfLives, setup->max_lives);
    fprintf(fp, int_format, N_StartingLevel, setup->start_level);
    fprintf(fp, str_format, N_RandomLevelOrder, 
	    setup->random_mode ? "True" : "False" );
    fprintf(fp, str_format, N_RandomPlayerPosition, 
	    setup->random_spos ? "True" : "False" );
#ifndef XBLAST_SOUND
    fprintf(fp, str_format, N_BellSound, setup->sound_flag ? "True" : "False" );
#endif
    fprintf(fp, int_format, N_FrameRate, (int)(1000000/setup->frame_time));
    fprintf(fp, str_format, N_AllowColorMode, 
	    setup->color_mode ? "True" : "False" );
    
#ifdef XBLAST_SOUND
    /* sound server setup */
    fprintf(fp, str_format, N_SoundServer, 
	    db_get_string(default_DB, N_SoundServer, C_SoundServer) );
#endif

    /* write level usage */
    for (i=0; i<levelMax; i++)  {
      fprintf(fp, str_format, get_level_res_name(i), 
	      setup->use_level[i] ? "True" : "False");
    }
    
    /* close file */
    fclose(fp);
  }
}



#ifdef DEBUG
/*
 * debug function: print_args
 */
#ifdef __STDC__
static void
print_args (int argc, char *argv[]) 
#else
static void
print_args (argc, argv) 
     int argc; 
     char *argv[];
#endif
{
  int i;
  fprintf (stderr, "Arg list:\n");
  for (i=0; i<argc; i++) {
    fprintf(stderr, "[%2d]: \"%s\"\n", i, argv[i]); 
  }
}
#endif

/*
 * public function parse_commandline
 */
#ifdef __STDC__
void
parse_commandline (int argc,
		   char *argv[])
#else
void
parse_commandline (argc, argv)
     int argc;
     char *argv[];
#endif
{
  XrmDatabase player_DB = NULL;
  XrmDatabase setup_DB = NULL;
  XrmDatabase tmp_DB = NULL;

  /* general init for databases etc */
#ifndef SOLARIS
  XrmInitialize();
#else
  XtToolkitInitialize();
#endif

  /* set porgram name */
  prog_name = argv[0];

  /* initiate default values */
  default_DB = set_defaults(default_settings);
  arg_DB = set_defaults(arg_settings);

  /* parse command line for player config */
#ifdef DEBUG
  print_args(argc, argv);
#endif
  player_DB = parse_player_config (&argc, argv);

  /* parse rest of commandline */
  while (argc != 1) {
#ifdef DEBUG
    print_args(argc, argv);
#endif
    XrmParseCommand(&arg_DB, arg_table, arg_table_entries, "xblast",&argc, argv);
#ifdef DEBUG
    print_args(argc, argv);
#endif
    /* check if any args are left */
    if (argc != 1) {
      if (NULL == (tmp_DB = parse_level_usage(&argc, argv)) ) {
	print_usage(argv[0],argv[1]);
      }
      XrmMergeDatabases(tmp_DB, &arg_DB);
    }
  }

  /* check for terminating options */
  if (db_get_bool(arg_DB, N_PrintHelp, C_PrintHelp )) {
    print_usage (argv[0], "-?");
  }

  /* check if setup is to be loaded */
  setup_DB = load_setup(arg_DB);
  XrmCombineDatabase(setup_DB, &arg_DB, FALSE);

  /* show list of levels now */
  if (db_get_bool(arg_DB, N_PrintLevels, C_PrintLevels )) {
    show_levels();
    exit_prg(0);
  }
  if (db_get_bool(arg_DB, N_PrintLevelsTcl, C_PrintLevelsTcl )) {
    show_levels_tcl();
    exit_prg(0);
  }

  /* merge databases */
  XrmMergeDatabases(player_DB, &arg_DB);
}



/*
 * public function interactive_setup
 */
#ifdef __STDC__
void 
interactive_setup (int num_player) 
#else
void 
interactive_setup (num_player)
     int num_player;
#endif
{
  XrmDatabase inter_DB = NULL;
  int player;
  char zeile[80];

  /* get players and displays */
  for (player = 0; player < num_player; player ++) {
    sprintf(zeile, "Name of player %d", player+1);
    query_string_for_database (&inter_DB, N_Player[player], zeile,
			       db_get_string(default_DB, N_Player[player], 
					     C_Player) );
  }

  /* get games options */
  query_int_for_database (&inter_DB, N_NumberOfLives, "Number of lives", 
			  1, MAX_LIVES,
			  db_get_int(default_DB, 1, MAX_LIVES, 
				     N_NumberOfLives, C_NumberOfLives) );
  query_int_for_database (&inter_DB, N_NumberOfVictories, 
			  "Number of victories", 1, MAX_VICTORIES,
			  db_get_int(default_DB, 1, MAX_VICTORIES, 
				     N_NumberOfVictories,C_NumberOfVictories));
  query_bool_for_database (&inter_DB, N_RandomPlayerPosition , 
			   "Random player position");
  if (!query_bool_for_database (&inter_DB, N_RandomLevelOrder, 
				"Random level order")) {
    query_int_for_database (&inter_DB, N_StartingLevel, "Starting Level", 
			    0, levelMax-1, 
			    db_get_int(default_DB, 0, levelMax-1, 
				       N_StartingLevel, C_StartingLevel) );
  }
  
  if (!query_bool_for_database (&inter_DB, "xblast.ignore", 
			       "Do you want to play all levels")) {
    int level;

    printf("Select levels to play:\n");
    for (level = 0; level <levelMax; level++) {
      query_bool_for_database(&inter_DB, get_level_res_name(level),
			      get_level_name(level));
    }
  }

  query_bool_for_database (&inter_DB, N_AllowColorMode, "Allow color mode");
  query_bool_for_database (&inter_DB, N_ForceOverride, 
			   "Override window managers");
  query_int_for_database (&inter_DB, N_FrameRate, "Frames per second", 2, 100, 
			  db_get_int(default_DB, 2, 100, N_FrameRate, 
				     C_FrameRate) );
#ifndef XBLAST_SOUND
  query_bool_for_database (&inter_DB, N_BellSound , "Bell sound");
#endif
  query_string_for_database (&inter_DB, N_SaveNamedDefaults, 
			     "Enter setup name to save (return to ignore)",
			     NULL);

  /* merge to default database */
  XrmMergeDatabases(inter_DB, &default_DB);
}



/*
 * public function send_setup
 */
#ifdef __STDC__
void 
send_setup (int num_player) 
#else
void 
send_setup (num_player)
     int num_player;
#endif
{
  int level, player;

  /* get players and displays */
  for (player = 0; player < num_player; player ++) {
    string_to_children ( db_get_string (default_DB, N_Player[player], C_Player) );
  }

  /* get games options */
  string_to_children ( db_get_string (default_DB, N_NumberOfLives, C_NumberOfLives) );
  string_to_children ( db_get_string (default_DB, N_NumberOfVictories, C_NumberOfVictories) );
  string_to_children ( db_get_string (default_DB, N_RandomPlayerPosition , C_Random) );
  string_to_children ( db_get_string (default_DB, N_RandomLevelOrder, C_Random) );
  string_to_children ( db_get_string (default_DB, N_StartingLevel, C_StartingLevel) );

  /* level selection */
  for (level = 0; level <levelMax; level++) {
    string_to_children ( db_get_string (default_DB, get_level_res_name(level), C_UseLevel ) );
  }

  string_to_children ( db_get_string (default_DB, N_AllowColorMode, C_AllowColorMode) );
  string_to_children ( db_get_string (default_DB, N_ForceOverride, C_ForceOverride) );
  string_to_children ( db_get_string (default_DB, N_BellSound , C_BellSound) );
}



/*
 * public function send_setup
 */
#ifdef __STDC__
void 
receive_setup (int num_player) 
#else
void 
receive_setup (num_player)
     int num_player;
#endif
{
  int player, level;
  XrmDatabase inter_DB = NULL;
  char buf[1024];

  /* get players and displays */
  for (player = 0; player < num_player; player ++) {
    string_from_parent(buf);
    XrmPutStringResource(&inter_DB, N_Player[player], buf);
  }

  /* get games options */
  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_NumberOfLives, buf);
  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_NumberOfVictories, buf);
  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_RandomPlayerPosition, buf);
  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_RandomLevelOrder, buf);
  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_StartingLevel, buf);

  /* level selection */
  for (level = 0; level <levelMax; level++) {
    string_from_parent(buf);
    XrmPutStringResource(&inter_DB, get_level_res_name(level) , buf);
  }

  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_AllowColorMode, buf);
  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_ForceOverride, buf);
  string_from_parent(buf);
  XrmPutStringResource(&inter_DB, N_BellSound, buf);
  string_to_children ( db_get_string (default_DB, N_BellSound , C_BellSound) );

  /* merge to default database */
  XrmMergeDatabases(inter_DB, &default_DB);
}



/*
 * public function interactive_config
 *   part I one the interactive setup, just queries number of players 
 *   and display names.
 */
static char *team_mode_list[] = {
  "None",
  "Team",
  "Double",
  NULL,
};

#ifdef XBLAST_SOUND
static char *sound_mode_list[] = {
  "stereo",
  "mono",
  "none",
  NULL,
};
#endif

#ifdef __STDC__
int 
interactive_config (char *argv0) 
#else
int 
interactive_config (argv0) 
     char *argv0;
#endif
{
  int player, num_player;
  char zeile[80];
  char *def_setup;

  printf("XBlast - Interactive Setup\n");
  
  prog_name = argv0;

  /* initiate default values */
  default_DB = set_defaults(default_settings);
  arg_DB = set_defaults(arg_settings);

  /* load defaults from file and return */
  if (query_bool_for_database(&arg_DB, "xblast.ignore", "Load an old setup")) {
    /* load a named setup */
    if (NULL != (def_setup = list_setups() ) ) {
      while ( ! setup_exists (query_string_for_database
			      (&arg_DB, N_LoadNamedDefaults,
			       "Name of setup", def_setup) ) ); 
    } else {
      printf("Sorry, no named setup available.\n");
    }
    db_get_string(arg_DB, N_LoadNamedDefaults, C_LoadNamedDefaults);
    arg_DB = load_setup(arg_DB);
    

    return 1;
  }

  num_player = query_int_for_database 
    (&arg_DB, N_NumberOfPlayers,  "Number of players", 2, MAX_PLAYER, 2);

  /* get players and displays */
  for (player = 0; player < num_player; player ++) {
    sprintf(zeile, "Display for player %d", player+1);
    query_string_for_database (&arg_DB, N_AtDisplay[player], zeile, NULL);
  }

  /* get team mode */
  query_list_for_database (&arg_DB, N_TeamMode, "Select team mode", 
			   team_mode_list);

  /* ask for fork mode */
  query_bool_for_database(&arg_DB, N_Fork, "Use fork mode");

#ifdef XBLAST_SOUND
  /* query dsp sound */
  query_list_for_database(&arg_DB, N_SoundServer, "Select DSP Sound",
			  sound_mode_list);
#endif
  return 0;
}



/*
 * public function get config_from_database
 */
#ifdef __STDC__
void 
config_from_database (XBConfig *config)
#else
void 
config_from_database (config)
     XBConfig *config;
#endif
{
  int disp, player;
  char *d_name;
  char *m_name;
  
  /* get number of players */
  config->num_player 
    = db_get_int(arg_DB, 2, MAX_PLAYER, N_NumberOfPlayers, C_NumberOfPlayers);
  /* get players and displays */
  config->num_disp = 0;
  for (player=0; player<config->num_player; player++) {
    d_name = db_get_string(arg_DB, N_AtDisplay[player], C_AtDisplay);
    for (disp=0; (disp<config->num_disp) && 
	 !equal_string(d_name, config->display[disp]); disp++);
    config->display[disp] = d_name;
    config->pl_at_disp[player] = disp;
    if (disp == config->num_disp) {
      config->num_disp ++;
    }
  }

  /* set default displa (not finished) */
  config->default_disp = 0;

  /* fork mode */
  config->fork = db_get_bool(arg_DB, N_Fork, C_Fork);
  
  /* team mode */
  m_name = db_get_string(arg_DB, N_TeamMode, C_TeamMode);
  if (equal_string(m_name, "Team")) {
    config->team_mode = TM_Team;
  } else if (equal_string(m_name, "Double")) {
    config->team_mode = TM_Double;
  } else if (equal_string(m_name, "None")) {
    config->team_mode = TM_Single;
  } else {
    fprintf(stderr, "Unknown team mode \"%s\".\n", m_name);
    exit_prg(1);
  }
}

#ifdef DEBUG
/*
 * public function get config_from_database
 */
#ifdef __STDC__
void 
debug_from_database (XBDebug *debug)
#else
void 
debug_from_database (debug)
     XBDebug *debug;
#endif
{
  debug->no_title = db_get_bool(arg_DB, N_NoTitle, C_NoTitle);
}
#endif

#ifdef XBLAST_SOUND
/*
 * global function sound_server_from_database
 */
#ifdef __STDC__
char *
sound_server_from_database (void)
#else
char *
sound_server_from_database ()
#endif
{
  return db_get_string(arg_DB, N_SoundServer, C_SoundServer);
}
#endif



/*
 * local function: copy_database_entry
 */
#ifdef __STDC__
static void
copy_database_entry (XrmDatabase src_DB,
		     XrmDatabase *dst_DB,
		     char *name,
		     char *class,
		     char *dst_name)
#else
static void
copy_database_entry (src_DB, dst_DB, name, class, dst_name)
     XrmDatabase src_DB;
     XrmDatabase *dst_DB;
     char *name;
     char *class;
     char *dst_name;
#endif
{
  char *result;
  
  /* try to get entry */
  if (NULL != (result = db_get_string(src_DB, name, class) ) ) {
    /* put into destination database */
    XrmPutStringResource(dst_DB, dst_name, result);
  }
}



/*
 * local function: get_database_entry_from_child
 */ 
#ifdef __STDC__
static void 
get_database_entry_from_child (XrmDatabase *db, 
			       char *attr)
#else
static void 
get_database_entry_from_child (db, attr)
     XrmDatabase *db;
     char *attr;
#endif
{
  char buffer[1024];

  /* get string from child */
  string_from_children (buffer);
#ifdef DEBUG_IPC
  fprintf(stderr, "C->P: %s stored in %s\n", buffer, attr);
#endif
  if (0 != strlen(buffer)) {
    /* store string in database */
    XrmPutStringResource(db, attr, buffer);
  }
}



/*
 * local function: send_database_entry_to_children
 */ 
#ifdef __STDC__
static void 
send_database_entry_to_children (XrmDatabase db, 
				 char *name,
				 char *class)
#else
static void 
send_database_entry_to_children (db, name, class)
     XrmDatabase db;
     char *name, *class;
#endif
{
  char *buffer;
  char dummy = '\0';

  if (NULL != (buffer = db_get_string (db, name, class))) {
    string_to_children (buffer);
  } else {
    string_to_children (&dummy);
  }
#ifdef DEBUG_IPC
  fprintf(stderr, "P->C: %s from %s\n", buffer, name);
#endif
}



/*
 * local function: parent_create_player_DB
 */
#ifdef __STDC
static XrmDatabase
parent_create_player_DB (XBConfig *config)
#else
static XrmDatabase
parent_create_player_DB (config)
     XBConfig *config;
#endif
{
  XrmDatabase player_DB = NULL;
  int i, player;
  int src, p1, p2;

  p1 = config->disp_player[0].p1;
  p2 = config->disp_player[0].p2;
  src = (p1==p2) ? SINGLE_PLAYER : RIGHT_PLAYER;
#ifdef DEBUG_IPC
  fprintf(stderr, "Parent CDB %d %d\n", p1, p2);
#endif

  for (player=0; player < config->num_player; player ++) {
#ifdef DEBUG_IPC
  fprintf(stderr, "Parent CDB [%d]\n", player);
#endif
    
    /* retrievs all attributes for one player */
    if (p1 == player) {
      /* get local data for player */
      for (i=0; N_PlayerAttributes[i] != NULL; i++) {
	copy_database_entry(disp_DB[0], &player_DB, 
			    N_PlayerAttributes[i][src], 
			    C_PlayerAttributes[i], 
			    N_PlayerAttributes[i][player]);
      }
      p1 = p2;
      src = LEFT_PLAYER;
    } else {
      /* get remote data for player */
      for (i=0; N_PlayerAttributes[i] != NULL; i++) {
	get_database_entry_from_child (&player_DB,N_PlayerAttributes[i][player]);
      }
    }
    /* send to all children */
    for (i=0; N_PlayerAttributes[i] != NULL; i++) {
      send_database_entry_to_children (player_DB, 
				       N_PlayerAttributes[i][player],
				       C_PlayerAttributes[i]);
    }
  }
  return player_DB;
}



/*
 * local function: send_database_entry_to_parent
 */
#ifdef __STDC__
static void
send_database_entry_to_parent (XrmDatabase db,
			       char *name,
			       char *class)
#else
static void
send_database_entry_to_parent (db, name, class)
     XrmDatabase db;
     char *name, *class;
#endif
{
  char *buffer;
  char dummy = '\0';

  if (NULL != (buffer = db_get_string(db, name, class))) {
    string_to_parent (buffer);
  } else {
    /* send empty string */
    string_to_parent (&dummy);
  }
#ifdef DEBUG_IPC
  fprintf(stderr, "C->P: %s from %s\n", buffer, name);
#endif
}



#ifdef __STDC__
static void 
get_database_entry_from_parent (XrmDatabase *db, 
				char *attr)
#else
static void 
get_database_entry_from_parent (db, attr)
     XrmDatabase *db;
     char *attr;
#endif
{
  char buffer[1024];
  
  /* get string from parent */
  string_from_parent(buffer);
#ifdef DEBUG_IPC
  fprintf(stderr, "P->C: %s stored in %s\n", buffer, attr);
#endif
  if (0 != strlen(buffer)) {
    /* store string in database */
    XrmPutStringResource(db, attr, buffer);
  }
}



/*
 * local function: child_create_player_DB
 */
#ifdef __STDC
static XrmDatabase
child_create_player_DB (XBConfig *config)
#else
static XrmDatabase
child_create_player_DB (config)
     XBConfig *config;
#endif
{
  XrmDatabase player_DB = NULL;
  int i, player, p1, p2, src;

  /* get local players */
  p1 = config->disp_player[0].p1;
  p2 = config->disp_player[0].p2;
  src = (p1 == p2) ? SINGLE_PLAYER : RIGHT_PLAYER;

#ifdef DEBUG_IPC
  fprintf(stderr, "Child CDB %d %d\n", p1, p2);
#endif

  for (player=0; player < config->num_player; player ++) {
#ifdef DEBUG_IPC
  fprintf(stderr, "Child CDB [%d]\n", player);
#endif

    if (player == p1) {
      for (i=0; N_PlayerAttributes[i]!=NULL; i++) {
	send_database_entry_to_parent (disp_DB[0], 
				       N_PlayerAttributes[i][src],
				       C_PlayerAttributes[i]);
      }
      /* set index for possible next player */
      p1 = p2;
      src = LEFT_PLAYER;
    }
    for (i=0; N_PlayerAttributes[i]!=NULL; i++) {
      get_database_entry_from_parent (&player_DB, N_PlayerAttributes[i][player]);
    }
  }
  return player_DB;
}



/*
 * local function: create_player_database
 */
#ifdef __STDC
static XrmDatabase
local_create_player_DB (XBConfig *config)
#else
static XrmDatabase
local_create_player_DB (config)
     XBConfig *config;
#endif
{
  XrmDatabase player_DB = NULL;
  int i, disp, player, src, dst;

  for (disp=0; disp < config->num_disp; disp++) {
    for (player=0; player < config->disp_player[disp].num; player ++) {
      /* get source and destination indices */
      if (0 == player) {
	if (1 == config->disp_player[disp].num) {
	  src = SINGLE_PLAYER;
	} else {
	  src = RIGHT_PLAYER;
	}
	dst = config->disp_player[disp].p1;
      } else {
	/* left player of display */
	src = LEFT_PLAYER;
	dst = config->disp_player[disp].p2;
      }
#ifdef DEBUG
      fprintf(stderr, "Copy resources: Player(%d)@Display(%d) to [player%d]\n",
	      src, disp, dst);
#endif      
      /* copy all player attributes */
      for (i=0; N_PlayerAttributes[i] != NULL; i++) {
	copy_database_entry(disp_DB[disp], &player_DB, 
			    N_PlayerAttributes[i][src], 
			    C_PlayerAttributes[i], 
			    N_PlayerAttributes[i][dst]);
      }
    }
  }

  return player_DB;
}



/*
 * public function: set_setup_communication
 */
#ifdef __STDC__
void
set_setup_communication (int type)
#else
void
set_setup_communication (type)
     int type;
#endif
{
  switch (type) {
  case CM_None:
    create_player_database = local_create_player_DB;
    break;
  case CM_Parent:
    create_player_database = parent_create_player_DB;
    break;
  case CM_Child:
    create_player_database = child_create_player_DB;
    break;
  }
}



/*
 * public function merge_default_databases
 */
#ifdef __STDC__
void
merge_default_databases (XBConfig *config)
#else
void
merge_default_databases (config)
     XBConfig *config;
#endif
{
  if (config->default_disp >= 0) {
    /* merge default display db into default db overriding */
    XrmMergeDatabases(disp_DB[config->default_disp], &default_DB);
    
    /* set display database to new merged one */
    disp_DB[config->default_disp] = default_DB;
  }

  /* ovveride new default_DB with player DB */
  XrmMergeDatabases((*create_player_database)(config), &default_DB);

  /* override new default database with commandline database */
  XrmMergeDatabases(arg_DB, &default_DB);
}


/*
 * public funtion setup_from_database
 */
#ifdef __STDC__
void
setup_from_database (XBSettings *setup) 
#else
void
setup_from_database (setup)
     XBSettings *setup;
#endif
{
  int i;
  /* now the rest from database */
  setup->max_victories = db_get_int(default_DB, 1, MAX_VICTORIES,
				    N_NumberOfVictories, C_NumberOfVictories);
  setup->max_lives = db_get_int(default_DB, 1, MAX_LIVES,
				N_NumberOfLives, C_NumberOfLives);
  setup->random_mode = db_get_bool(default_DB, N_RandomLevelOrder, C_Random);
  setup->start_level = db_get_int(default_DB, 0, levelMax-1, 
				   N_StartingLevel, C_StartingLevel );
  setup->sound_flag = db_get_bool(default_DB, N_BellSound, C_BellSound);
  setup->frame_time = 1000000/db_get_int(default_DB, 2, 100, 
					 N_FrameRate, C_FrameRate);
  setup->random_spos = db_get_bool(default_DB, N_RandomPlayerPosition,
				   C_Random);
  setup->print_stat = db_get_bool(default_DB, N_PrintStat, C_PrintStat);

  setup->color_mode = db_get_bool(default_DB, N_AllowColorMode, 
				  C_AllowColorMode);
  /* use level setzen */
  for (i=0; i<levelMax; i++) {
    setup->use_level[i] = db_get_bool(default_DB, get_level_res_name(i), 
				      C_UseLevel);
  }
}


/*
 * public function keys_from_database
 */ 
#ifdef __STDC__
int
keys_from_database (int disp,
		    int player,
		    KeyPressDefine *keydef,
		    PlayerAction *pa)
#else
int
keys_from_database (disp, player, keydef, pa)
     int disp, player;
     KeyPressDefine *keydef;
     PlayerAction *pa;
#endif
{
  int offset = 0;
  player -= MAX_PLAYER;

#ifdef DEBUG
  fprintf(stderr, "keys_from_database %d %d\n",disp,player);
#endif

  /* pause key */
  keydef[offset].keysym = db_get_string(disp_DB[disp], N_PlayerPauseKey[player],
					C_PlayerPauseKey);
  keydef[offset].addr   = &(pa->pause);
  keydef[offset].value  = TRUE;
  offset++;
  /* go up */
  keydef[offset].keysym = db_get_string(disp_DB[disp], N_PlayerUpKey[player],
					C_PlayerUpKey);
  keydef[offset].addr   = &(pa->dir);
  keydef[offset].value  = GoUp;
  offset++;
  /* go right */
  keydef[offset].keysym = db_get_string(disp_DB[disp], N_PlayerRightKey[player],
					C_PlayerRightKey);
  keydef[offset].addr   = &(pa->dir);
  keydef[offset].value  = GoRight;
  offset++;
  /* go down */
  keydef[offset].keysym = db_get_string(disp_DB[disp], N_PlayerDownKey[player],
					C_PlayerDownKey);
  keydef[offset].addr   = &(pa->dir);
  keydef[offset].value  = GoDown;
  offset++;
  /* go left */
  keydef[offset].keysym = db_get_string(disp_DB[disp], N_PlayerLeftKey[player],
					C_PlayerLeftKey);
  keydef[offset].addr   = &(pa->dir);
  keydef[offset].value  = GoLeft;
  offset++;
  /* go stop */
  keydef[offset].keysym = db_get_string(disp_DB[disp],N_PlayerStopKey[player],
					C_PlayerStopKey);
  keydef[offset].addr   = &(pa->dir);
  keydef[offset].value  = GoStop;
  offset++;
  /* drop bomb */
  keydef[offset].keysym = db_get_string(disp_DB[disp],N_PlayerBombKey[player],
					C_PlayerBombKey);
  keydef[offset].addr   = &(pa->bomb);
  keydef[offset].value  = TRUE;
  offset++;
  /* special key */
  keydef[offset].keysym = db_get_string(disp_DB[disp],N_PlayerSpecialKey[player],
					C_PlayerSpecialKey);
  keydef[offset].addr   = &(pa->special);
  keydef[offset].value  = TRUE;
  offset++;
  /* abort key */
  keydef[offset].keysym = db_get_string(disp_DB[disp], N_PlayerAbortKey[player],
					C_PlayerAbortKey);
  keydef[offset].addr   = &(pa->abort);
  keydef[offset].value  = ABORT_TRUE;
  offset++;
  /* abort cancel key */
  keydef[offset].keysym = db_get_string(disp_DB[disp], 
					N_PlayerAbortCancelKey[player],
					C_PlayerAbortCancelKey);
  keydef[offset].addr   = &(pa->abort);
  keydef[offset].value  = ABORT_CANCEL;
  offset++;
  
  return offset;
}



/*
 * public function color_mode_from_database
 */
#ifdef __STDC__
int 
color_mode_from_database (int disp)
#else
int 
color_mode_from_database (disp)
     int disp;
#endif
{
  return ( db_get_bool(default_DB, N_AllowColorMode, C_AllowColorMode)
	  && db_get_bool(disp_DB[disp], N_ColorMode, C_ColorMode) );
}


/*
 * public function get_override_flag
 */
#ifdef __STDC__
int
override_from_database (int disp)
#else
int
override_from_database (disp)
     int disp;
#endif
{
  return (db_get_bool(default_DB, N_ForceOverride, C_ForceOverride)
	  || db_get_bool(disp_DB[disp], N_Override, C_Override) );
}



/*
 * public player_strings_from_database
 */
#ifdef __STDC__
void
player_strings_from_database (PlayerStrings *st, 
			      XBConfig *config)
#else
void
player_strings_from_database (st, config)
     PlayerStrings *st;
     XBConfig *config;
#endif
{
  int player;
  XrmDatabase string_DB = NULL;
  char line[1024];

  /* first get names for all players */
  for (player=0; player<MAX_PLAYER; player++) {
    st[player].tag = db_get_string(default_DB, N_Player[player], C_Player);
  }
  /* if in team mode combine names */
  switch (config->team_mode) {
  case TM_Single:
    for (player=0; player<MAX_PLAYER; player ++) {
      st[player].name = st[player].tag;
    }
    break;
  case TM_Team:
    for (player=0; player<MAX_PLAYER; player += 2) {
      sprintf(line, "%s & %s", st[player].tag, st[player+1].tag);
      st[player].name = st[player+1].name = dup_string(line, strlen(line)); 
    }
    break;
  case TM_Double:
    for (player=0; player<MAX_PLAYER; player ++) {
      st[player + config->num_player/2].name 
	= st[player + config->num_player/2].tag 
	= st[player].name = st[player].tag;
    }
    break;
  } 

  /* set pause string */
  for (player=0; player<MAX_PLAYER; player++) {
    sprintf(line, "Game paused by %s", st[player].tag);
    st[player].pause = dup_string(line, strlen(line));
  }

  /* now create database with default strings (part I)*/
  for (player=0; player<MAX_PLAYER; player++) {
    if (config->team_mode == TM_Single) {
      sprintf(line, "%s wins", st[player].name);
    } else {
      sprintf(line, "%s win", st[player].name);
    }
    XrmPutStringResource(&string_DB, D_PlayerWinLevel[player],line);
    sprintf(line, "Abort requested by %s", st[player].tag);
    XrmPutStringResource(&string_DB, D_PlayerAbort[player], line);
    sprintf(line, "%s cancels abort", st[player].tag);
    XrmPutStringResource(&string_DB, D_PlayerAbortCancel[player], line);
  }


  /* merge with default database */
  XrmCombineDatabase (string_DB, &default_DB, FALSE);

  /* now get all the player strings */
  for (player=0; player<MAX_PLAYER; player++) {
    st[player].winlevel 
      = db_get_string(default_DB, N_PlayerWinLevel[player], C_PlayerWinLevel);
    st[player].wingame 
      = db_get_string(default_DB, N_PlayerWinGame[player], C_PlayerWinGame);
    st[player].loselevel 
      = db_get_string(default_DB, N_PlayerLoseLevel[player], 
		      C_PlayerLoseLevel);
    st[player].loselife 
      = db_get_string(default_DB, N_PlayerLoseLife[player], C_PlayerLoseLife);
    st[player].gloat 
      = db_get_string(default_DB, N_PlayerGloat[player], C_PlayerGloat);
    st[player].welcome 
      = db_get_string(default_DB, N_PlayerWelcome[player], C_PlayerWelcome);
    st[player].abort 
      = db_get_string(default_DB, N_PlayerAbort[player], C_PlayerAbort);
    st[player].abortcancel 
      = db_get_string(default_DB, N_PlayerAbortCancel[player], 
		      C_PlayerAbortCancel);
  }
}


/*
 * public function
 */
#ifdef __STDC__
void
create_display_database (int disp)
#else 
void
create_display_database (disp)
     int disp;
#endif
{
  XrmDatabase disp_def_DB = NULL;
  BMEntry *ptr;

  /* get defaults for display data base */
  for (ptr = disp_default_setting; ptr->name != NULL; ptr++) {
    XrmPutStringResource(&disp_def_DB, ptr->name, ptr->value);
  }

  /* get default from applications default file */
  XrmMergeDatabases (XrmGetFileDatabase(file_appl_def), &disp_def_DB);

  /* get database from server created by xrdb */
  if (XResourceManagerString(dpy[disp]) != NULL) {
#ifdef DEBUG
    fprintf(stderr, "initializing database for Display %d.\n", disp);
#endif
    disp_DB[disp] = XrmGetStringDatabase(XResourceManagerString(dpy[disp]));
  }

  /* merge both databases */
  XrmMergeDatabases (disp_def_DB, &(disp_DB[disp]) );
}






/*
 * public function get_font_resources
 */
#ifdef __STDC__
void
get_font_resources (int disp, 
		    char **font_name)
#else
void
get_font_resources (disp, font_name) 
     int disp;
     char **font_name;
#endif
{
  font_name[0] = db_get_string(disp_DB[disp], N_LargeFont, C_LargeFont);
  font_name[1] = db_get_string(disp_DB[disp], N_MediumFont, C_MediumFont);
  font_name[2] = db_get_string(disp_DB[disp], N_SmallFont, C_SmallFont);
}



/*
 * public funtion get_color_resources
 */
#ifdef __STDC__
void
get_color_resources (int disp, 
		     DisplayColor *dc)
#else
void
get_color_resources (disp, dc) 
     int disp;
     DisplayColor *dc;
#endif
{
  dc->title1     = db_get_string(disp_DB[disp], N_TitleColor1, C_TitleColor1);
  dc->title2     = db_get_string(disp_DB[disp], N_TitleColor2, C_TitleColor2);
  dc->lighttext1 = db_get_string(disp_DB[disp], N_LightTextColor1, 
				 C_LightTextColor);
  dc->lighttext2 = db_get_string(disp_DB[disp], N_LightTextColor2, 
				 C_LightTextColor);
  dc->darktext1  = db_get_string(disp_DB[disp], N_DarkTextColor1, 
				C_DarkTextColor);
  dc->darktext2  = db_get_string(disp_DB[disp], N_DarkTextColor2, 
				C_DarkTextColor);
  dc->statusled  = db_get_string(disp_DB[disp], N_StatusLedColor, 
				C_StatusLedColor);
  dc->statusfg   = db_get_string(disp_DB[disp], N_StatusForeground, 
			       C_StatusForeground);
  dc->statusbg   = db_get_string(disp_DB[disp], N_StatusBackground, 
			       C_StatusBackground);
  dc->expl1      = db_get_string(disp_DB[disp], N_ExplosionColor1, 
				 C_ExplosionColor1);
  dc->expl2      = db_get_string(disp_DB[disp], N_ExplosionColor2, 
			    C_ExplosionColor2);
  dc->expl3      = db_get_string(disp_DB[disp], N_ExplosionColor3, 
				 C_ExplosionColor3);
  dc->bomb       = db_get_string(disp_DB[disp], N_BombColor, C_BombColor);
}



/*
 * public function get_player_color_resources
 */
#ifdef __STDC__
void 
get_player_color_resources (PlayerColor *pc)
#else
void 
get_player_color_resources (pc)
     PlayerColor *pc;
#endif
{
  int player;

  for (player=0; player<MAX_PLAYER; player++, pc++) {
    pc->helmet     = db_get_string (default_DB, N_PlayerHelmetColor[player],
				    C_PlayerColor);
    pc->face       = db_get_string (default_DB, N_PlayerFaceColor[player],
				    C_PlayerColor);
    pc->body       = db_get_string (default_DB, N_PlayerBodyColor[player],
				    C_PlayerColor);
    pc->hands_feet = db_get_string (default_DB, N_PlayerHandsFeetColor[player],
				    C_PlayerColor);
    pc->arms_legs  = db_get_string (default_DB, N_PlayerArmsLegsColor[player],
				    C_PlayerColor);
    pc->backpack   = db_get_string (default_DB, N_PlayerBackpackColor[player],
				    C_PlayerColor);
  }
}


/*
 * public function destroy_databases
 */
#ifdef __STDC__
void
delete_databases (int num_disp)
#else
void
delete_databases (num_disp)
     int num_disp;
#endif
{
  int disp;
  
  for (disp = 0; disp < num_disp; disp ++) {
    XrmDestroyDatabase(disp_DB[disp]);
    disp_DB[disp] = NULL;
  }
  default_DB = NULL;
}


/*
 * end of file xsetup.c
 */
