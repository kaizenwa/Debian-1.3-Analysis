/*
    CODGER.C
    An automatic code generator for Yorick.

    $Id: codger.c,v 1.1 1993/08/27 18:32:09 munro Exp $

    Codger generates the code Yorick requires to initialize BuiltIn
    functions, so that they are "visible" to the Yorick interpreter.
    Codger can also generate simple wrapper routines, so that a
    package writer does not necessarily need to learn how to write
    a function of the Yorick BuiltIn data type defined in ydata.h.

    To do this, Codger reads all of the package.i files for the
    built-in function packages which are to be loaded with this
    version of Yorick, plus the std.i package (which defines things
    like simple math functions, e.g.- log and exp, and indipensible
    Yorick utility functions, e.g.- dimsof and array).  Codger then
    writes the ycode.c file containing the required initialization
    code, and any wrapper routines which have been requested.

    The output file ycode.c is intended to be compiled TWICE --
    once with YINIT_C defined to produce yinit.o, the initialization
    code, and once with YWRAP_C defined to produce ywrap.o, the
    wrapper code.  This separation allows the ywrap.o part to be
    included in an object library to be loaded with other versions
    of Yorick, while providing the specific yinit.o file required
    for this version of Yorick.
    Command line:
       codger [-Df_linkage] Y_HOME Y_SITE [pkg.i [pkg2.i ...]]
              [- other.i [other2.i ...]]

    where:
       Y_HOME         the directory where Maketmpl can be found
                           --this doesn't matter at yorick runtime
       Y_SITE         the directory where the startup/ directory can be
                           found.  This directory also contains
			   a subdirectory include/ where distribution
			   include files are kept.
                           Codger records this directory in a data
                           statement.  However, Yorick will ignore the
                           compiled in value if a soft link called
			   yorick.site is present in the directory
			   in which its executable resides.
       pkg.i [pkg2.i ...]  the package include file(s) for the code in
                           this development directory (see PKG-note)
       other.i [other2.i ... ]  package include file(s) for other
                           packages you would like built into this
			   version of Yorick.
       f_linkage
              FORTRAN linkage options.  Override the default FORTRAN
	      linkage convention on this platform.  Possible values are
	      f_linkage (lower case), F_LINKAGE (upper case), f_linkage_
	      (lower case, trailing underscore), or F_LINKAGE_
	      (upper case, trailing underscore).  This should be avoided,
	      but your platform may have multiple linkage conventions.

    In addition to initializing the built-in functions, the Codger-
    generated data in ycode.c initializes the default path Yorick will
    search for include files.  There are two search paths; the first is
    used when Yorick starts, in order to find std.i, pkg.i, other.i, and
    stdx.i.  After Yorick has been initialized, it switches to a second,
    search path for finding interactively included files.

    The startup search path is

       "Y_LAUNCH:Y_SITE/startup:Y_SITE/contrib"

    where Y_LAUNCH is the directory containing the executable (determined
    at runtime), and Y_SITE is the directory specified to Codger at
    compile time.  A soft link

       Y_LAUNCH/yorick.site

    to another directory will override the compiled-in Y_SITE.  Also,
    if Y_LAUNCH/../Yorick/startup/std.i exists and there was no yorick.site
    link, then Y_SITE will become Y_LAUNCH/../Yorick overriding the
    compiled-in value.

    After startup, the search path becomes:

       ".:~/Yorick:Y_SITE/include:Y_SITE/contrib"

    This default may be overridden by placing an interpreted instruction
    to reset the search path in the custom.i startup file.

    The names pkg.i should be the pathnames RELATIVE to one of the
    directories on the initial include search path.  A similar
    remark holds for the other.i files.  Using an absolute path name
    is legal, but will result in a code which will depend on the
    precise search directory pathnames.

    Codger searches the current working directory for the std.i
    pkg.i, and other.i, before looking in Y_SITE and its
    contrib subdirectory.  That is, the current working directory
    in which Codger is running plays the role of the Y_LAUNCH
    directory when Yorick is running.

    When Yorick starts, the std.i file will be included first,
    followed by the other.i files in reverse order, followed by
    the pkg.i files in reverse order, then finally, the stdx.i
    file which takes care of any final cleanup before custom.i.
*/
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include <stdio.h>
extern void exit(int status);

/* UNIX directory separator is '/', DOS is '\', MacIntosh is ':' */
#ifndef DIR_SEP
#define DIR_SEP '/'
#endif
/* Similar mods necessary for /contrib and /include names */
#define CONTRIB_DIR "/contrib"
#define INCLUDE_DIR "/include"
#define STARTUP_DIR "/startup"
#define FALLBACK_STARTUP "../Yorick/startup"
/* Search paths are lists of directory names -- the : separator which
   is UNIX standard for such objects cannot be used on the MacIntosh.  */
#define PATH_SEP ":"
/* The first two directories on the default path are . and ~/Yorick
   -- the whole notion of search paths must be modified for MacIntosh,
   and probably for DOS also... */
#ifndef USER_PATH
#define USER_PATH ".:~/Yorick"
#endif

/* Don't bother to try to include <stdlib.h>; it might not exist.  */
extern void *malloc(long size);
extern void free(void *p);
extern void *realloc(void *p, long size);

#include "defstr.h"
/* (codger only uses string functions from <string.h> ANSI header) */

/* Process, OpenInput, ScanInput, and WriteInit are defined in this file.  */
extern void Process(char *name, int wrap);
extern FILE *OpenInput(char *dir, char *name);
extern void ScanInput(FILE *file, int wrap);
extern void WriteInit(void);

static int FindString(char **list, int lList, char *string);
static int AddString(char ***plist, int *plList, char *string);

/*--------------------------------------------------------------------------*/

#define MAX_PACKAGES 8
#define MAX_OTHERS 64
#define MAX_PATHS 64

#define MAX_LINE 1024

char *yorick_home= 0;
char *yorick_site= 0;
char *pkg_name[MAX_PACKAGES];
char *other_name[MAX_OTHERS];
int n_pkg=0, n_other=0;

FILE *ycode_file;  /* the output file, ycode.c */

char lineBuf[MAX_LINE];

char yorick_include[MAX_LINE];
char yorick_contrib[MAX_LINE];
char yorick_startup[MAX_LINE];

/*--------------------------------------------------------------------------*/

/* FORTRAN linkage can be all uppercase, all lowercase, with or
   without trailing underscore.  */

extern char *LCFortran(char *);
extern char *LC_Fortran(char *);
extern char *UCFortran(char *);
extern char *UC_Fortran(char *);

#define FORTRAN_LINKAGE Bad_Codger_Fortran_Linkage

#ifdef f_linkage
#undef FORTRAN_LINKAGE
#define FORTRAN_LINKAGE LCFortran
#endif

#ifdef f_linkage_
#undef FORTRAN_LINKAGE
#define FORTRAN_LINKAGE LC_Fortran
#endif

#ifdef F_LINKAGE
#undef FORTRAN_LINKAGE
#define FORTRAN_LINKAGE UCFortran
#endif

#ifdef F_LINKAGE_
#undef FORTRAN_LINKAGE
#define FORTRAN_LINKAGE UC_Fortran
#endif

char *(*FortranID)(char *)= &FORTRAN_LINKAGE;

/*--------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
  char *arg;
  int do_other= 0;
  int i;

  /* crack the command line */
  if (argc) { argc--; argv++; }
  while (argc-- && (arg= *argv++)) {
    if (arg[0]=='-') {
      arg++;
      if (!arg[0]) {
	if (!yorick_site || !yorick_home) {
	  fprintf(stderr, "codger: Y_HOME Y_SITE must precede - option\n");
	  exit(1);
	}
	do_other= 1;

      } else if (arg[0]=='D') {
	arg++;
	if (strcmp(arg, "f_linkage")==0) FortranID= &LCFortran;
	else if (strcmp(arg, "f_linkage_")==0) FortranID= &LC_Fortran;
	else if (strcmp(arg, "F_LINKAGE")==0) FortranID= &UCFortran;
	else if (strcmp(arg, "F_LINKAGE_")==0) FortranID= &UC_Fortran;
	else {
	  fprintf(stderr,
		  "codger: unrecognized FORTRAN linkage -F%s\n", arg);
	  exit(1);
	}

      } else {
	fprintf(stderr, "codger: unrecognized option -%s\n", arg);
	exit(1);
      }

    } else if (!yorick_home) {
      yorick_home= arg;

    } else if (!yorick_site) {
      yorick_site= arg;

    } else if (!do_other) {
      if (n_pkg >= MAX_PACKAGES-1) {
	fprintf(stderr,
		"codger: exceeded limit of %d pkg.i files\n", MAX_PACKAGES);
	exit(1);
      }
      /* this hack simplifies the Yorick Makefile */
      if (strcmp(arg,"std.i") && strcmp(arg,"graph.i"))
	pkg_name[n_pkg++]= arg;

    } else {
      if (n_other >= MAX_OTHERS-1) {
	fprintf(stderr,
		"codger: exceeded limit of %d other.i files\n", MAX_OTHERS);
	exit(1);
      }
      other_name[n_other++]= arg;
    }
  }
  if (!yorick_site || !yorick_home) {
    fprintf(stderr, "codger: Usage\n"
	   "        codger [-Df_linkage] home site [pkg.i [pkg2.i ...]]\n"
           "               [- other.i [other2.i ...]]\n");
    exit(1);
  }

  /* initial search path always .:Y_SITE:Y_SITE/contrib */
  i= strlen(yorick_site);
  if (yorick_site[i-1]==DIR_SEP && i>1) yorick_site[--i]= '\0';
  strcpy(yorick_include, yorick_site);
  strcpy(&yorick_include[i], INCLUDE_DIR);
  strcpy(yorick_contrib, yorick_site);
  strcpy(&yorick_contrib[i], CONTRIB_DIR);
  strcpy(yorick_startup, yorick_site);
  strcpy(&yorick_startup[i], STARTUP_DIR);

  /* create ycode.c and write the first few lines of it */
  ycode_file= fopen("ycode.c", "w");
  if (!ycode_file) {
    fprintf(stderr, "codger: unable to create ycode.c output file\n");
    exit(1);
  }
  fprintf(ycode_file,
	  "/* YCODE.C   generated by Codger to initialize Yorick */\n\n");
  fprintf(ycode_file,
	  "/* Package(s) specific to this version of Yorick:\n");
  for (i=0 ; i<n_pkg ; i++)
    fprintf(ycode_file, "     (%d)  %s\n", i+1, pkg_name[i]);
  fprintf(ycode_file, " */\n");
  fprintf(ycode_file,
	  "/* Other package(s) loaded into this version of Yorick:\n");
  for (i=0 ; i<n_other ; i++)
    fprintf(ycode_file, "     (%d)  %s\n", i+1, other_name[i]);
  fprintf(ycode_file, " */\n");
  fprintf(ycode_file,
	  "/* Yorick default Y_HOME directory:\n");
  fprintf(ycode_file, "     %s\n */\n\n", yorick_home);
  fprintf(ycode_file,
	  "/* Yorick default Y_SITE directory:\n");
  fprintf(ycode_file, "     %s\n */\n\n", yorick_site);
  fprintf(ycode_file, "#include <ydata.h>\n\n");

  /* scan std.i file */
  Process("std.i", 0);

  /* scan the other.i files */
  for (i=0 ; i<n_other ; i++) Process(other_name[i], 0);

  /* scan the pkg.i files */
  for (i=0 ; i<n_pkg ; i++) Process(pkg_name[i], 1);

  /* (stdx.i file does not need to be scanned) */

  WriteInit();

  return 0;
}

/*--------------------------------------------------------------------------*/

static char fortranID[80];

char *LCFortran(char *id)
{
  char c;
  strcpy(fortranID, id);
  id= fortranID;
  for (c=*id ; c ; c=*(++id)) if (c>='A' && c<='Z') *id^= ('A'^'a');
  return fortranID;
}

char *LC_Fortran(char *id)
{
  char c;
  strcpy(fortranID, id);
  id= fortranID;
  for (c=*id ; c ; c=*(++id)) if (c>='A' && c<='Z') *id^= ('A'^'a');
  *id++= '_';
  *id= '\0';
  return fortranID;
}

char *UCFortran(char *id)
{
  char c;
  strcpy(fortranID, id);
  id= fortranID;
  for (c=*id ; c ; c=*(++id)) if (c>='a' && c<='z') *id^= ('A'^'a');
  return fortranID;
}

char *UC_Fortran(char *id)
{
  char c;
  strcpy(fortranID, id);
  id= fortranID;
  for (c=*id ; c ; c=*(++id)) if (c>='a' && c<='z') *id^= ('A'^'a');
  *id++= '_';
  *id= '\0';
  return fortranID;
}

/*--------------------------------------------------------------------------*/

void Process(char *name, int wrap)
{
  FILE *file= 0;
  if (name[0]==DIR_SEP) {
    file= OpenInput((char *)0, name);
  } else {
    file= OpenInput((char *)0, name);
    if (!file) {
      file= OpenInput(yorick_startup, name);
      if (!file) {
	file= OpenInput(yorick_contrib, name);
	if (!file) file= OpenInput(FALLBACK_STARTUP, name);
      }
    }
  }
  if (!file) {
    fprintf(stderr, "codger: %s not found, not installed?\n", name);
    exit(1);
  }
  ScanInput(file, wrap);
}

/*--------------------------------------------------------------------------*/

static char **includeList= 0;
static int lIncludeList= 0;

FILE *OpenInput(char *dir, char *name)
{
  long len;
  FILE *file;

  if (dir && name[0]!=DIR_SEP) {
    len= strlen(dir);
    strcpy(lineBuf, dir);
    if (len && lineBuf[len-1]!=DIR_SEP) {
      lineBuf[len++]= DIR_SEP;
      lineBuf[len]= '\0';
    }
  } else {
    len= 0;
    lineBuf[0]= '\0';
  }

  strcpy(&lineBuf[len], name);
  file= fopen(lineBuf, "r");

  if (file) {
    fprintf(ycode_file, "\n"
"/*-----------------------------------------------------------------------*/"
"\n/* Source: %s */\n\n", lineBuf);
    AddString(&includeList, &lIncludeList, name);

    fprintf(stdout, "...scanning %s", name);
    if (name[0]==DIR_SEP) fprintf(stdout, "\n");
    else if (!dir) fprintf(stdout, " (found in current directory)\n");
    else fprintf(stdout, " (found in %s)\n", dir);
    fflush(stdout);
  }

  return file;
}

/*--------------------------------------------------------------------------*/

static char currentName[256];

static char **typeList= 0;
static int lTypeList= 0;
static char **dimsList= 0;
static int lDimsList= 0;

static char **funcList= 0;
static int lFuncList= 0;
static char **varList= 0;
static char **varList2= 0;
static int lVarList= 0;

static char *SkipWhite(char *line);
static char *SkipComments(char *line, FILE *file);
static void SetNewName(char *line);
static void WriteExtern(int typeIndex, int dimsIndex);
static int MakeWrapper(char *line, FILE *file); /* returns 1 if EOF reached */
static void MakeExternal(char *line);
static char *NextProtok(char *line, FILE *file);
static short TypeToken(char *token, FILE *file, int isArg);
static int MakeStruct(char *line, FILE *file);  /* returns 1 if EOF reached */
static char *PutDimlist(char *line, FILE *file);
static void MakeLValue(char *line);

static int AddBasic(char ***plist, int *plList, char *string);
static int ReverseDims(char *dims);

void ScanInput(FILE *file, int wrap)
{
  char *line;

  /* Each "extern" statement triggers generation of the C-extern line
     in ycode.c corresponding to the PREVIOUS name (if any), then sets
     the new name.  */
  currentName[0]= '\0';

  while ((line= fgets(lineBuf, MAX_LINE, file))) {
    line= SkipWhite(line);
    if (strncmp(line, "extern", 6L)==0) {
      /* "extern currentName" will cause the BuiltIn function Y_currentName
	 to be declared in ycode.c, unless a subsequent reshape overrides
	 this default behavior before the next extern */
      line+= 6;
      if (line[0]==' ' || line[0]=='\t') SetNewName(line);

    } else if (strncmp(line, "struct", 6L)==0) {
      line+= 6;
      if (line[0]!=' ' && line[0]!='\t') continue;
      if (MakeStruct(line, file)) break;

    } else if (currentName[0]) {
      if (line[0]=='/' && line[1]=='*') {
	line= SkipWhite(line+2);
	if (wrap && strncmp(line, "PROTOTYPE", 9L)==0) {
	  /* special PROTOTYPE comment generates wrapper code */
	  if (MakeWrapper(line+9, file)) break;
	} else if (strncmp(line, "EXTERNAL", 8L)==0) {
	  MakeExternal(line+8);
	}

      } else if (strncmp(line, "reshape", 7L)==0) {
	long len;
	line= SkipWhite(line+7);
	if (line[0]!=',') continue;
	line= SkipWhite(line+1);
	len= strlen(currentName);
	if (strncmp(line, currentName, len)==0) {
	  /* "reshape, currentName, type, dims" causes the variable
	     y_currentName to be declared in ycode.c */
	  line= SkipWhite(line+len);
	  if (line[0]!=',') continue;
	  MakeLValue(line+1);
	}
      }
    }
  }

  /* Write final C-extern statement, if necessary.  */
  WriteExtern(-1, -1);
  fclose(file);
}

static char *SkipWhite(char *line)
{
  while (*line==' ' || *line=='\t' || *line=='\n') line++;
  return line;
}

static void SetNewName(char *line)
{
  char *name, c;

  /* Be sure a legal name token exists.  */
  line= name= SkipWhite(line);
  c= *line++;
  if ((c>'z' || c<'a') && (c>'Z' || c<'A') && c!='_') return;

  /* Write the C-extern statement for the previous currentName, if any,
     before clobbering it.  */
  WriteExtern(-1, -1);

  /* Find the end of the name token, then copy it to currentName.  */
  while ((c= *line) &&
	 ((c>='a' && c<='z') || (c>='A' && c<='Z') ||
	  (c>='0' && c<='9') || c=='_')) line++;
  *line= '\0';
  strcpy(currentName, name);
}

static void WriteExtern(int typeIndex, int dimsIndex)
{
  if (!currentName[0]) return;

  if (typeIndex < 0) {
    /* The name represents a built-in function.
       extern BuiltIn Y_currentName;                  */
    fprintf(ycode_file, "\nextern BuiltIn Y_%s;\n", currentName);
    AddString(&funcList, &lFuncList, currentName);

  } else {
    /* The name represents a global variable.
       extern typeList[typeIndex] currentName dimsList[dimsIndex];   */
    int lvl= lVarList;
    fprintf(ycode_file, "\nextern %s %s%s;\n",
	    typeList[typeIndex], currentName,
	    dimsIndex>=0? dimsList[dimsIndex] : "");
    AddString(&varList, &lvl, currentName);
    AddString(&varList2, &lVarList, currentName);
  }
}

void WriteInit(void)
{
  int i;
  fprintf(ycode_file, "\n"
"/*-----------------------------------------------------------------------*/"
"\n#ifdef YINIT_C\n");

  fprintf(ycode_file, "BuiltIn *yBuiltIns[]= {\n");
  for (i=0 ; i<lFuncList ; i++)
    fprintf(ycode_file, "  &Y_%s,\n", funcList[i]);
  fprintf(ycode_file, "  0 };\n\n");

  fprintf(ycode_file, "void *yGlobals[]= {\n");
  for (i=0 ; i<lVarList ; i++)
    fprintf(ycode_file, "  &%s,\n", varList2[i]);
  fprintf(ycode_file, "  0 };\n\n");

  fprintf(ycode_file, "char *yLinkNames[]= {\n");
  for (i=0 ; i<lFuncList ; i++)
    fprintf(ycode_file, "  \"%s\",\n", funcList[i]);
  for (i=0 ; i<lVarList ; i++)
    fprintf(ycode_file, "  \"%s\",\n", varList[i]);
  fprintf(ycode_file, "  0 };\n\n");

  fprintf(ycode_file, "char *initialIncludes[]= {\n");
  fprintf(ycode_file, "  \"stdx.i\",\n");
  for (i=lIncludeList-1 ; i>=0 ; i--)
    fprintf(ycode_file, "  \"%s\",\n", includeList[i]);
  fprintf(ycode_file, "  0 };\n\n");

  fprintf(ycode_file, "extern char *yHomeDir;\n");
  fprintf(ycode_file, "char *yHomeDir= \"%s\";\n", yorick_home);

  fprintf(ycode_file, "extern char *ySiteDir;\n");
  fprintf(ycode_file, "char *ySiteDir= \"%s\";\n", yorick_site);

  fprintf(ycode_file, "extern char *yUserPath;\n");
  fprintf(ycode_file, "char *yUserPath= \"%s\";\n", USER_PATH);

  fprintf(ycode_file, "#endif\n");
}

/*--------------------------------------------------------------------------*/

static int AddString(char ***plist, int *plList, char *string)
{
  char **list= *plist;
  int lList= *plList;
  int i= FindString(list, lList, string);
  if (i>=0) return i;

  if (!(lList&7)) {
    if (!list) list= malloc(sizeof(char *)*(lList+8));
    else list= realloc(list, sizeof(char *)*(lList+8));
    if (!list) {
    fail:
      fprintf(stderr, "codger: memory manager failed in AddString\n");
      exit(1);
    }
    *plist= list;
  }
  if (!(list[lList]= malloc(strlen(string)+1))) goto fail;
  strcpy(list[lList], string);

  *plList= lList+1;
  return lList;
}

static int FindString(char **list, int lList, char *string)
{
  int i;
  for (i=0 ; i<lList ; i++) if (strcmp(list[i], string)==0) break;
  return i<lList? i : -1;
}

#define N_BASIC 9
#define I_COMPLEX 6
#define I_STRING 7
#define I_POINTER 8
static char *basicTypes[N_BASIC]= {
  "char", "short", "int", "long", "float", "double", "complex",
  "string", "pointer" };

static int AddBasic(char ***plist, int *plList, char *string)
{
  int i, iType;

  for (i=0 ; i<N_BASIC ; i++) if (strcmp(string, basicTypes[i])==0) break;
  if (i<N_BASIC) {
    iType= FindString(*plist, *plList, string);
    if (iType<0) {
      iType= AddString(plist, plList, string);
      if (iType==I_COMPLEX) {
	fprintf(ycode_file,
		"typedef struct complex {double re, im;} complex;\n");
      } else if (iType==I_STRING) {
	fprintf(ycode_file, "typedef char *string;\n");
      } else if (iType==I_POINTER) {
	fprintf(ycode_file, "typedef void *pointer;\n");
      }
    }
  } else {
    iType= -1;
  }

  return iType;
}

/*--------------------------------------------------------------------------*/

static char dims[80];

static void MakeLValue(char *line)
{
  /* Currently at location marked by ^ in source line:
        reshape, currentName,^ type, dims;               */
  int iType, iDims;
  if (!(line= strtok(line, " \t\n,;")) ||
      ((iType= FindString(typeList, lTypeList, line))<0 &&
       (iType= AddBasic(&typeList, &lTypeList, line))<0)) {
    fprintf(stderr,
	    "codger: **WARNING** variable %s has undefined type %s\n",
	    currentName, line? line : "<garbled reshape>");
    return;
  }

  dims[0]= '\0';
  if (ReverseDims(dims)) iDims= AddString(&dimsList, &lDimsList, dims);
  else iDims= -1;

  WriteExtern(iType, iDims);
  currentName[0]= '\0';
}

static int ReverseDims(char *dims)
{
  char *token= strtok((char *)0, " \t\n,;");
  if (token && token[0]!='/') {  /* terminates at open comment */
    ReverseDims(dims);
    sprintf(dims+strlen(dims), "[%s]", token);
    return 1;
  } else {
    return 0;
  }
}

/*--------------------------------------------------------------------------*/

/* Transcribe Yorick struct definition into C struct definition.  */
static int MakeStruct(char *line, FILE *file)
{
  /* returns 1 if EOF reached */
  char *mark, *token= SkipComments(line, file);
  char type[80], name[80];
  if (!token) return 1;

  /* get struct name */
  for (mark=token ; *mark && *mark!=' ' && *mark!='\t' && *mark!='\n' &&
                    *mark!='{' && *mark!='/' ; mark++);
  name[0]= '\0';
  strncat(name, token, mark-token);
  line= SkipComments(mark, file);

  if (line && line[0]=='{') {
    AddString(&typeList, &lTypeList, name);
    fprintf(ycode_file, "\ntypedef struct %s %s;\nstruct %s {\n",
	    name, name, name);
    line= SkipComments(line+1, file);

    while (line && line[0]!='}') {
      /* get the data type for this member or member group */
      for (mark=line ;
	   *mark && *mark!=' ' && *mark!='\t' && *mark!='\n' ; mark++);
      *mark= '\0';
      if (strcmp(line, "string")==0) strcpy(type, "char*");
      else if (strcmp(line, "pointer")==0) strcpy(type, "void*");
      else { type[0]= '\0'; strncat(type, line, 79L); }
      line= SkipWhite(mark+1);

      for (;;) {
	/* get the name of this member */
	for (mark=line ;
	     *mark && *mark!=' ' && *mark!='\t' && *mark!='\n' &&
	     *mark!=',' && *mark!=';' && *mark!='(' ; mark++);
	name[0]= '\0';
	strncat(name, line, mark-line);
	fprintf(ycode_file, "  %s %s", type, name); /* start member */
	line= SkipWhite(mark);
	if (*line=='(') line= PutDimlist(line+1, file);
	fprintf(ycode_file, ";\n");                 /* finish member */
	if (!line || !(line= SkipComments(line, file)) ||
	    line[0]!=',') break;
	line= SkipComments(line+1, file);
      }

      /* skip semicolon after member declaration, if present */
      if (line && line[0]==';') line++;
      line= SkipComments(line, file);
    }

    fprintf(ycode_file, "};\n\n");
  }
  return !line;
}

static char *PutDimlist(char *line, FILE *file)
{
  long len;

  line= SkipComments(line, file);
  if (!line) return line;   /* actually an error */

  len= strtol(line, (char **)0, 0);
  while (*line && *line!=',' && *line!=')' && *line!=':' &&
	 *line!='/') line++;
  if (!*line || *line=='/') line= SkipComments(line, file);

  if (line && *line==':') {
    line= SkipComments(line+1, file);
    if (!line) return line;   /* actually an error */
    len= strtol(line, (char **)0, 0) - len + 1;
    while (*line && *line!=',' && *line!=')' && *line!='/') line++;
    if (!*line || *line=='/') line= SkipComments(line, file);
  }

  /* Either recurse to reverse dimension list order, or halt recursion,
     skipping te closing parenthesis.  */
  if (*line==',') line= PutDimlist(line+1, file);
  else if (*line==')') line++;

  fprintf(ycode_file, "[%ld]", len);
  return line;
}

static char *SkipComments(char *line, FILE *file)
{
  for (;;) {
    /* skip to first non-white character */
    for (;;) {
      line= SkipWhite(line);
      if (line[0]) break;
      line= fgets(lineBuf, MAX_LINE, file);
      if (!line) return 0;
    }

    if (line[0]=='/' && line[1]=='*') {
      /* first token begins comment -- skip to matching end */
      line+= 2;
      for (;;) {
	while (!*line) {
	  line= fgets(lineBuf, MAX_LINE, file);
	  if (!line) return 0;
	}
	if (*line++=='*') {
	  if (*line=='/') break;
	}
      }
      line++;

    } else {
      /* we have a non-comment token -- return */
      break;
    }
  }
  return line;
}

/*--------------------------------------------------------------------------*/

static void MakeExternal(char *line)
{
  char namewk[80], *name;
  int isFortran, lvl= lVarList;

  /* if comment begins "EXTERNAL FORTRAN", translate name */
  line= SkipWhite(line);
  if (strncmp(line, "FORTRAN", 7L)==0) {
    line= SkipWhite(line+7);
    isFortran= 1;
  } else {
    isFortran= 0;
  }

  name= strtok(line, " \t\n*;");
  if (isFortran) {
    namewk[0]= '\0';
    strncat(namewk, name, 78L);
    name= FortranID(namewk);
  }

  fprintf(ycode_file, "\nextern /*unknown type*/char %s;\n", name);
  AddString(&varList, &lvl, currentName);
  AddString(&varList2, &lVarList, name);

  currentName[0]= '\0';
}

/*--------------------------------------------------------------------------*/

static short argType[256];
static char prevDelimiter;

static char *pushRoutine[7]= { "",
  "PushIntValue((int)(unsigned char)",
  "PushIntValue((int)", "PushIntValue(", "PushLongValue(",
  "PushDoubleValue((double)", "PushDoubleValue(" };

static char *argNames[26]= {
  "void", "char", "short", "int", "long", "float", "double", "char*",
  "void*", "complex", "", "", "", "", "", "", "", "char*", "short*",
  "int*", "long*", "float*", "double*", "char**", "void**", "double*" };

static char *argFormats[26]= { "",
  "(char)YGetInteger(sp-%d)", "(short)YGetInteger(sp-%d)",
  "(int)YGetInteger(sp-%d)", "YGetInteger(sp-%d)",
  "(float)YGetReal(sp-%d)", "YGetReal(sp-%d)", "YGetString(sp-%d)",
  "*YGet_P(sp-%d,0,0)", "", "", "", "", "", "", "", "",
  "YGet_C(sp-%d,0,0)", "YGet_S(sp-%d,0,0)", "YGet_I(sp-%d,0,0)",
  "YGet_L(sp-%d,0,0)", "YGet_F(sp-%d,0,0)", "YGet_D(sp-%d,0,0)",
  "YGet_Q(sp-%d,0,0)", "YGet_P(sp-%d,0,0)", "YGet_Z(sp-%d,0,0)" };

static int MakeWrapper(char *line, FILE *file)
{
  int isFortran, nArgs, i;
  char namewk[80], *name, *token;
  short type;

  /* if comment begins "PROTOTYPE FORTRAN", translate name */
  line= SkipWhite(line);
  if (strncmp(line, "FORTRAN", 7L)==0) {
    line+= 7;
    isFortran= 1;
  } else {
    isFortran= 0;
  }

  /* get the data type for the function */
  type= TypeToken(NextProtok(line, file), file, 0);
  if (type>6 ||
      prevDelimiter=='(' || prevDelimiter==',' || prevDelimiter==')') {
  fail:
    fprintf(stderr,
	    "codger: garbled PROTOTYPE comment for %s\n", currentName);
    exit(1);
  }

  /* get the function name */
  token= NextProtok((char *)0, file);
  if (prevDelimiter==',' || prevDelimiter==')') goto fail;
  namewk[0]= '\0';
  strncat(namewk, token, 78L);
  if (isFortran) name= FortranID(namewk);
  else name= namewk;

  /* get introduction to parameter list */
  if (prevDelimiter!='(') {
    token= NextProtok((char *)0, file);
    if (token[0] || prevDelimiter!='(') goto fail;
  }

  nArgs= 0;
  while (prevDelimiter!=')') {
    argType[nArgs]= TypeToken(NextProtok((char *)0, file), file, 1);
    if (argType[nArgs]==0 && nArgs>0) goto fail;
    nArgs++;
  }
  if (nArgs==1 && argType[0]==0) nArgs= 0;

  /* declare the wrapper routine Y_currentName */
  WriteExtern(-1, -1);

  fprintf(ycode_file, "\n#ifdef YWRAP_C\n");

  /* declare the compiled routine */
  fprintf(ycode_file, "extern %s %s(", argNames[type], name);
  for (i=0 ; i<nArgs ; i++) {
    if ((i+1)%6 == 0) fprintf(ycode_file, "\n            ");
    fprintf(ycode_file, argNames[argType[i]]);
    if (i<nArgs-1) fprintf(ycode_file, ",");
  }
  if (nArgs==0) fprintf(ycode_file, "void");
  fprintf(ycode_file, ");\n");

  /* define the wrapper routine */
  if (nArgs>0) {
    fprintf(ycode_file, "void Y_%s(int nArgs)\n{\n  "
	    "if (nArgs!=%d) YError(\"%s takes exactly %d arguments\");\n",
	    currentName, nArgs, currentName, nArgs);
  } else {
    fprintf(ycode_file, "void Y_%s(int nArgs)\n{\n  "
	    "if (nArgs>1) YError(\"%s takes at most one nil argument\");\n",
	    currentName, currentName);
  }
  if (type>0) fprintf(ycode_file, "  %s\n", pushRoutine[type]);
  fprintf(ycode_file, "  %s(", name);
  for (i=0 ; i<nArgs ; i++) {
    if ((i+1)%3 == 0) fprintf(ycode_file, "\n    ");
    fprintf(ycode_file, argFormats[argType[i]], nArgs-1-i);
    if (i<nArgs-1) fprintf(ycode_file, ",");
  }
  if (type>0) fprintf(ycode_file, "));\n  PopTo(sp-nArgs-1);\n");
  else fprintf(ycode_file, ");\n");
  fprintf(ycode_file, "  Drop(nArgs);\n}\n");

  fprintf(ycode_file, "#endif\n");

  /* returns 1 if EOF reached */
  currentName[0]= '\0';
  return !line;
}

static char *beginScan= 0;

static char *NextProtok(char *line, FILE *file)
{
  long len;
  char *token;

  if (line) beginScan= line;  /* initialize on 1st call (like strtok) */

  beginScan= SkipWhite(beginScan);
  while (!*beginScan) {
    beginScan= fgets(lineBuf, MAX_LINE, file);
    if (!beginScan) break;
    beginScan= SkipWhite(beginScan);
  }
  if (!beginScan || strncmp(beginScan, "*/", 2L)==0) {
    fprintf(stderr, "codger: PROTOTYPE comment for %s ends too soon\n",
	    currentName);
    exit(1);
  }

  token= beginScan;
  len= strcspn(token, "(,) \t\n");
  beginScan+= len;
  prevDelimiter= *beginScan;
  beginScan[0]= '\0';
  if (prevDelimiter) beginScan++;
  return token;
}

static short TypeToken(char *token, FILE *file, int isArg)
{
  short type;
  int gotname;

  if (strcmp(token, "void")==0) type= 0;
  else if (strcmp(token, "char")==0) type= 1;
  else if (strcmp(token, "short")==0) type= 2;
  else if (strcmp(token, "int")==0) type= 3;
  else if (strcmp(token, "long")==0) type= 4;
  else if (strcmp(token, "float")==0) type= 5;
  else if (strcmp(token, "double")==0) type= 6;
  else if (strcmp(token, "string")==0) type= 7;
  else if (strcmp(token, "pointer")==0) type= 8;
  else if (strcmp(token, "complex")==0) type= 9;
  else {
    /* Note:
       Could handle arbitrary types someday by passing typename */
  badt:
    fprintf(stderr, "codger: PROTOTYPE comment for %s uses illegal type\n",
	    currentName);
    exit(1);
  }

  if (!isArg) return type;

  gotname= 0;
  while (prevDelimiter!=',' && prevDelimiter!=')') {
    token= NextProtok((char *)0, file);
    if (token[0]) {
      if (strcmp(token, "array")==0) {
	if (type==0) goto badt;  /* void array illegal */
	if (type&16) goto badt;
	type|= 16;
      } else {
	if (gotname) goto badt;
	gotname= 1;
      }
    } else {
      break;
    }
  }
  if (prevDelimiter!=',' && prevDelimiter!=')') goto badt;
  if (type==9) goto badt;   /* scalar complex illegal */

  return type;
}

/*--------------------------------------------------------------------------*/
