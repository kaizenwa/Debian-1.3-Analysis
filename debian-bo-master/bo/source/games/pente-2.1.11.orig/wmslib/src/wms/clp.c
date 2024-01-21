/*
 * wmslib/src/wms/clp.c, part of wmslib (Library functions)
 * Copyright (C) 1995-1996 William Shubert.
 * See "configure.h.in" for more copyright information.
 *
 * Command Line Parse.
 */

#include <wms.h>
#include <stdio.h>
#include <string.h>
#include <wms/str.h>

#if  STDC_HEADERS
#include <stdlib.h>
#endif  /* STDC_HEADERS */

#include "clp.h"


const char  *wms_progname = "UNKNOWN";

static bool  showErrs = TRUE;


static ClpEntry  *clp_iLookup(Clp *clp, const char *key, bool *bval);
static void  clpEntry_free(ClpEntry *ci);
static void  strip(char *argv[]);
static void  clp_help(Clp *clp);


Clp  *clp_create(const ClpSetup vars[])  {
  int  numVars, i;
  Clp  *newClp;

  newClp = wms_malloc(sizeof(Clp));
  MAGIC_SET(newClp);
  for (numVars = 0;  !(vars[numVars].flags & CLPSETUP_ENDFLAG);  ++numVars);
  newClp->numInfos = numVars + 2;
  newClp->infos = wms_malloc((numVars + 2) * sizeof(ClpEntry));
  for (i = 0;  i < numVars;  ++i)  {
    MAGIC_SET(&newClp->infos[i]);
    newClp->infos[i].name = vars[i].name;
    newClp->infos[i].desc = vars[i].desc;
    newClp->infos[i].flags = vars[i].flags;
    newClp->infos[i].test = vars[i].test;
    if (vars[i].name == NULL)
      newClp->infos[i].flags |= CLPSETUP_NOSAVE;
    newClp->infos[i].where = clpWhere_unset;
    newClp->infos[i].type = clpDtype_string;
    if (vars[i].defVal)  {
      newClp->infos[i].storage.sval = wms_malloc(strlen(vars[i].defVal) + 1);
      strcpy(newClp->infos[i].storage.sval, vars[i].defVal);
    } else  {
      newClp->infos[i].storage.sval = wms_malloc(1);
      newClp->infos[i].storage.sval[0] = '\0';
    }
  }
  MAGIC_SET(&newClp->infos[i]);
  newClp->infos[i].name = NULL;
  newClp->infos[i].desc = "";
  newClp->infos[i].flags = CLPSETUP_BOOL|CLPSETUP_NOSAVE;
  newClp->infos[i].where = clpWhere_unset;
  newClp->infos[i].type = clpDtype_bool;
  newClp->infos[i].test = NULL;
  
  ++i;
  MAGIC_SET(&newClp->infos[i]);
  newClp->infos[i].name = "help,-help";
  newClp->infos[i].desc = "Show this message";
  newClp->infos[i].flags = CLPSETUP_BOOL|CLPSETUP_NOSAVE|CLPSETUP_HELP;
  newClp->infos[i].where = clpWhere_unset;
  newClp->infos[i].type = clpDtype_bool;
  newClp->infos[i].test = NULL;
  return(newClp);
}


void  clp_destroy(Clp *clp)  {
  int  i;

  assert(MAGIC(clp));
  for (i = 0;  i < clp->numInfos;  ++i)
    clpEntry_free(&clp->infos[i]);
  wms_free(clp->infos);
  wms_free(clp);
}


int   clp_rCmdline(Clp *clp, char *argv[])  {
  int  numArgs;
  ClpEntry  *ce;
  bool  bval;

  assert(MAGIC(clp));
  wms_progname = argv[0];
  strip(argv);
  for (numArgs = 0;  argv[numArgs];)  {
    if (argv[numArgs][0] == '-')  {
      /* It's a switch. */
      ce = clp_iLookup(clp, argv[numArgs]+1, &bval);
      ce->where = clpWhere_cmdline;
      if (ce->flags & CLPSETUP_BOOL)  {
	if (bval && (ce->flags & CLPSETUP_HELP))  {
	  clp_help(clp);
	  exit(1);
	}
	clpEntry_setBool(ce, bval);
      } else  {
	if (argv[numArgs+1])
	  clpEntry_setStr(ce, argv[numArgs+1]);
	else  {
	  fprintf(stderr, "%s: Switch \"%s\" requires an argument.\n",
		  wms_progname, argv[numArgs]);
	  exit(1);
	}
	strip(argv + numArgs);
      }
      strip(argv + numArgs);
    } else
      ++numArgs;
  }
  return(numArgs);
}


/*
 * clp_rFile returns FALSE if it can't find the file at all.
 */
bool  clp_rFile(Clp *clp, const char *fname)  {
  FILE  *ifile;
  char  line_in[1024], *arg;
  Str  full_fn;
  char  *home, *key_start;
  int  i, ch_in;
  bool  retval = FALSE;
  ClpEntry  *ci;

  assert(MAGIC(clp));
  str_initChars(&full_fn, fname);
  showErrs = FALSE;
  if (fname[0] == '~')  {
    /* Prepend "$HOME" */
    home = getenv("HOME");
    if (home == NULL)  {
      fprintf(stderr,
	      "%s: Error: Could not find environment variable \"HOME\".\n",
	      wms_progname);
      showErrs = TRUE;
      str_deinit(&full_fn);
      return(FALSE);
    }
    str_print(&full_fn, "%s/%s", home, fname+1);
  }
  ifile = fopen(str_chars(&full_fn), "r");
  if (ifile != NULL)  {
    retval = TRUE;
    do  {
      ch_in = getc(ifile);
      for (i = 0;  (i < 1023) && (ch_in != '\n') && (ch_in != EOF);  ++i)  {
	line_in[i] = ch_in;
	ch_in = getc(ifile);
      }
      while ((ch_in != '\n') && (ch_in != EOF))
	ch_in = getc(ifile);
      line_in[i] = '\0';
      if ((line_in[0] == '#') || (line_in[0] == '\0'))
	continue;
      key_start = strchr(line_in, '.') + 1;
      if (key_start != NULL)  {
	arg = strchr(line_in, ':');
	*arg = '\0';
	arg += 2;
	ci = clp_iLookup(clp, key_start, NULL);
	if (ci != NULL)  {
	  if ((ci->where == clpWhere_unset) ||
	      (ci->where == clpWhere_xdefs))  {
	    ci->where = clpWhere_rcfile;
	    clpEntry_setStr(ci, arg);
	  }
	}
      }
    } while (ch_in != EOF);
    fclose(ifile);
  }
  str_deinit(&full_fn);
  showErrs = TRUE;
  return(retval);
}


void  clp_wFile(Clp *clp, const char *fname, const char *pname)  {
  FILE *ofile;
  Str  full_fn;
  char  *home;
  int  i, prevMask;
  ClpEntry  *ci;

  str_initChars(&full_fn, fname);
  if (fname[0] == '~')  {
    /* Prepend "$HOME" */
    home = getenv("HOME");
    if (home == NULL)  {
      fprintf(stderr,
	      "%s: Error: Could not find environment variable \"HOME\".\n",
	      wms_progname);
      str_deinit(&full_fn);
      return;
    }
    str_print(&full_fn, "%s/%s", home, fname+1);
  }
  prevMask = umask(0177);
  ofile = fopen(str_chars(&full_fn), "w");
  umask(prevMask);
  if (ofile == NULL)  {
    fprintf(stderr, "%s: Error: Could not open file \"%s\" for writing.\n",
	    wms_progname, fname);
    perror(wms_progname);
    exit(1);
  }
  fprintf(ofile,
	  "# NOTICE: Please do not edit this file.\n"
	  "# It was automatically generated by \"%s\".  If you want to\n"
	  "#   change one of these values, please use command line switches,\n"
	  "#   X defaults, or a setup window.\n"
	  "# As a last resort you may simply delete this file.\n\n",
	  wms_progname);
  for (i = 0;  i < clp->numInfos;  ++i)  {
    ci = &clp->infos[i];
    if (!(ci->flags & CLPSETUP_NOSAVE))  {
      switch(ci->type)  {
      case clpDtype_int:
	fprintf(ofile, "%s.%s: %d\n", pname, ci->name, ci->storage.ival);
	break;
      case clpDtype_double:
	fprintf(ofile, "%s.%s: %f\n", pname, ci->name, ci->storage.dval);
	break;
      case clpDtype_bool:
	if (ci->storage.bval)
	  fprintf(ofile, "%s.%s: y\n", pname, ci->name);
	else
	  fprintf(ofile, "%s.%s: n\n", pname, ci->name);
	break;
      case clpDtype_string:
	fprintf(ofile, "%s.%s: %s\n", pname, ci->name, ci->storage.sval);
	break;
      default:
	/* Should never reach here. */
	break;
      }
    }
  }
  fclose(ofile);
  str_deinit(&full_fn);
}


int  clpEntry_iGetInt(ClpEntry *ce, bool *err)  {
  switch(ce->type)  {
  case clpDtype_int:
    if (err)
      *err = FALSE;
    return(ce->storage.ival);
    break;
  case clpDtype_string:
    return(wms_atoi(ce->storage.sval, err));
    break;
  default:
    if (err)
      *err = TRUE;
    return(0);
  }
}


double  clpEntry_iGetDouble(ClpEntry *ce, bool *err)  {
  switch(ce->type)  {
  case clpDtype_double:
    if (err)
      *err = FALSE;
    return(ce->storage.dval);
    break;
  case clpDtype_string:
    return(wms_atof(ce->storage.sval, err));
    break;
  default:
    if (err)
      *err = TRUE;
    return(0.0);
  }
}


char  *clpEntry_iGetStr(ClpEntry *ce, bool *err)  {
  if (ce->type == clpDtype_string)  {
    if (err)
      *err = FALSE;
    return(ce->storage.sval);
  } else  {
    if (err)
      *err = TRUE;
    return(NULL);
  }
}

  
bool  clpEntry_iGetBool(ClpEntry *ce, bool *err)  {
  switch(ce->type)  {
  case clpDtype_bool:
    if (err)
      *err = FALSE;
    return(ce->storage.bval);
    break;
  case clpDtype_string:
    if ((!strcmp(ce->storage.sval, "1")) ||
	(!strcmp(ce->storage.sval, "t")) ||
	(!strcmp(ce->storage.sval, "T")) ||
	(!strcmp(ce->storage.sval, "y")) ||
	(!strcmp(ce->storage.sval, "Y")) ||
	(!strcmp(ce->storage.sval, "true")) ||
	(!strcmp(ce->storage.sval, "True")) ||
	(!strcmp(ce->storage.sval, "TRUE")) ||
	(!strcmp(ce->storage.sval, "yes")) ||
	(!strcmp(ce->storage.sval, "Yes")) ||
	(!strcmp(ce->storage.sval, "YES")))
      return(TRUE);
    if ((!strcmp(ce->storage.sval, "0")) ||
	(!strcmp(ce->storage.sval, "f")) ||
	(!strcmp(ce->storage.sval, "F")) ||
	(!strcmp(ce->storage.sval, "n")) ||
	(!strcmp(ce->storage.sval, "N")) ||
	(!strcmp(ce->storage.sval, "false")) ||
	(!strcmp(ce->storage.sval, "False")) ||
	(!strcmp(ce->storage.sval, "FALSE")) ||
	(!strcmp(ce->storage.sval, "no")) ||
	(!strcmp(ce->storage.sval, "No")) ||
	(!strcmp(ce->storage.sval, "NO")))
      return(FALSE);
    if (err)
      *err = TRUE;
    return(FALSE);
    break;
  default:
    if (err)
      *err = TRUE;
    return(FALSE);
  }
}


bool  clpEntry_setInt(ClpEntry *ce, int val)  {
  ClpEntry  newCe;

  MAGIC_SET(&newCe);
  if (ce->test)  {
    newCe.type = clpDtype_int;
    newCe.storage.ival = val;
    if (!ce->test(&newCe))
      return(FALSE);
  }
  clpEntry_free(ce);
  ce->type = clpDtype_int;
  ce->storage.ival = val;
  MAGIC_UNSET(&newCe);
  return(TRUE);
}


bool  clpEntry_setDouble(ClpEntry *ce, double val)  {
  ClpEntry  newCe;

  MAGIC_SET(&newCe);
  if (ce->test)  {
    newCe.type = clpDtype_double;
    newCe.storage.dval = val;
    if (!ce->test(&newCe))
      return(FALSE);
  }
  clpEntry_free(ce);
  ce->type = clpDtype_double;
  ce->storage.dval = val;
  MAGIC_UNSET(&newCe);
  return(TRUE);
}


bool  clpEntry_setStr(ClpEntry *ce, const char *val)  {
  ClpEntry  newCe;

  MAGIC_SET(&newCe);
  if (ce->test)  {
    newCe.type = clpDtype_string;
    newCe.storage.sval = wms_malloc(strlen(val) + 1);
    strcpy(newCe.storage.sval, val);
    if (!ce->test(&newCe))
      return(FALSE);
  }
  clpEntry_free(ce);
  ce->type = clpDtype_string;
  ce->storage.sval = wms_malloc(strlen(val) + 1);
  strcpy(ce->storage.sval, val);
  MAGIC_UNSET(&newCe);
  return(TRUE);
}


bool  clpEntry_setBool(ClpEntry *ce, bool val)  {
  ClpEntry  newCe;

  MAGIC_SET(&newCe);
  if (ce->test)  {
    newCe.type = clpDtype_bool;
    newCe.storage.bval = val;
    if (!ce->test(&newCe))
      return(FALSE);
  }
  clpEntry_free(ce);
  ce->type = clpDtype_bool;
  ce->storage.bval = val;
  MAGIC_UNSET(&newCe);
  return(TRUE);
}


ClpEntry  *clp_lookup(Clp *clp, const char *key)  {
  assert(MAGIC(clp));
  return(clp_iLookup(clp, key, NULL));
}


static ClpEntry  *clp_iLookup(Clp *clp, const char *key, bool *bval)  {
  ClpEntry  *ci;
  int  i, j, keyLen;
  bool  reversed;

  assert(MAGIC(clp));
  keyLen = strlen(key);
  reversed = !strncmp(key, "no", 2);
  if (bval)
    *bval = TRUE;
  for (i = 0;  i < clp->numInfos;  ++i)  {
    ci = &clp->infos[i];
    if (ci->name)  {
      for (j = 0;  ci->name[j];)  {
	if (!strncmp(ci->name+j, key, keyLen))  {
	  if ((ci->name[j + keyLen] == '\0') ||
	      (ci->name[j + keyLen] == ','))
	    return(ci);
	}
	if (reversed && (ci->flags & CLPSETUP_BOOL) &&
	    !strncmp(ci->name, key+2, keyLen - 2))  {
	  if (bval)
	    *bval = FALSE;
	  return(ci);
	}
	while (ci->name[j] && (ci->name[j] != ','))
	  ++j;
	if (ci->name[j])
	  ++j;
      }
    }
  }
  if (showErrs)  {
    fprintf(stderr, "%s: \"-%s\" is not a valid flag.\n"
	    "   Use \"%s -help\" for a list of valid flags.\n",
	    wms_progname, key, wms_progname);
    exit(1);
  } else
    return(NULL);
}


static void  strip(char *argv[])  {
  do  {
    argv[0] = argv[1];
    ++argv;
  } while (argv[0] != NULL);
}


static void  clpEntry_free(ClpEntry *ci)  {
  assert(MAGIC(ci));
  if ((ci->type == clpDtype_string) &&
      ci->storage.sval)  {
    wms_free(ci->storage.sval);
  }
}


static void  clp_help(Clp *clp)  {
  int  i, j;
  char  name[40];
  const char  *prestr;

  assert(MAGIC(clp));
  for (i = 0;  i < clp->numInfos;  ++i)  {
    if (clp->infos[i].desc)  {
      if (clp->infos[i].name)  {
	name[0] = '\0';
	if (clp->infos[i].flags & CLPSETUP_SHOWBOOL)
	  prestr = "-[no]";
	else
	  prestr = "-";
	strcpy(name, prestr);
	for (j = 0;  clp->infos[i].name[j];  ++j)  {
	  strncat(name, clp->infos[i].name+j, 1);
	  if (clp->infos[i].name[j] == ',')
	    strcat(name, prestr);
	}
	fprintf(stderr, "  %20s %s\n", name, clp->infos[i].desc);
      } else
	fprintf(stderr, "  %s\n", clp->infos[i].desc);
    }
  }
}
