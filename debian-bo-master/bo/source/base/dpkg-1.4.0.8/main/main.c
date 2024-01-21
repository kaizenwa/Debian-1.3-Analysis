/*
 * dpkg - main program for package management
 * main.c - main program
 *
 * Copyright (C) 1994,1995 Ian Jackson <iwj10@cus.cam.ac.uk>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 * This is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with dpkg; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <unistd.h>
#include <dirent.h>
#include <limits.h>
#include <ctype.h>

#include "config.h"
#include "dpkg.h"
#include "dpkg-db.h"
#include "version.h"
#include "myopt.h"

#include "main.h"

static void printversion(void) {
  if (!fputs("Debian Linux `" DPKG "' package management program version "
              DPKG_VERSION_ARCH ".\n"
             "Copyright 1994-1996 Ian Jackson, Bruce Perens.  This is free software;\n"
             "see the GNU General Public Licence version 2 or later for copying\n"
             "conditions.  There is NO warranty.  See dpkg --licence for details.\n",
             stdout)) werr("stdout");
}
/*
   options that need fixing:
  " DPKG " --yet-to-unpack                 \n\
  */
static void usage(void) {
  if (!fputs("\
Usage: \n\
  " DPKG " -i|--install      <.deb file name> ... | -R|--recursive <dir> ...\n\
  " DPKG " --unpack          <.deb file name> ... | -R|--recursive <dir> ...\n\
  " DPKG " -A|--record-avail <.deb file name> ... | -R|--recursive <dir> ...\n\
  " DPKG " --configure           <package name> ... | -a|--pending\n\
  " DPKG " -r|--remove | --purge <package name> ... | -a|--pending\n\
  " DPKG " --get-selections [<pattern> ...]   get list of selections to stdout\n\
  " DPKG " --set-selections                   set package selections from stdin\n\
  " DPKG " --update-avail <Packages-file>     replace available packages info\n\
  " DPKG " --merge-avail <Packages-file>      merge with info from file\n\
  " DPKG " --clear-avail                      erase existing available info\n\
  " DPKG " --forget-old-unavail               forget uninstalled unavailable pkgs\n\
  " DPKG " -s|--status <package-name> ...     display package status details\n\
  " DPKG " --print-avail <package-name> ...   display available version details\n\
  " DPKG " -L|--listfiles <package-name> ...  list files `owned' by package(s)\n\
  " DPKG " -l|--list [<pattern> ...]          list packages concisely\n\
  " DPKG " -S|--search <pattern> ...          find package(s) owning file(s)\n\
  " DPKG " -C|--audit                         check for broken package(s)\n\
  " DPKG " --print-architecture               print target architecture (uses GCC)\n\
  " DPKG " --print-gnu-build-architecture     print GNU version of target arch\n\
  " DPKG " --print-installation-architecture  print host architecture (for inst'n)\n\
  " DPKG " --compare-versions <a> <rel> <b>   compare version numbers - see below\n\
  " DPKG " --help | --version                 show this help / version number\n\
  " DPKG " --force-help | -Dh|--debug=help    help on forcing resp. debugging\n\
  " DPKG " --licence                          print copyright licencing terms\n\
\n\
Use " DPKG " -b|--build|-c|--contents|-e|--control|-I|--info|-f|--field|\n\
 -x|--extract|-X|--vextract|--fsys-tarfile  on archives (type " BACKEND " --help.)\n\
\n\
For internal use: " DPKG " --assert-support-predepends | --predep-package |\n\
  --assert-working-epoch\n\
\n\
Options:
  --admindir=<directory>     Use <directory> instead of " ADMINDIR "\n\
  --root=<directory>         Install on alternative system rooted elsewhere\n\
  --instdir=<directory>      Change inst'n root without changing admin dir\n\
  -O|--selected-only         Skip packages not selected for install/upgrade\n\
  -E|--skip-same-version     Skip packages whose same version is installed\n\
  -G=--refuse-downgrade      Skip packages with earlier version than installed\n\
  -B|--auto-deconfigure      Install even if it would break some other package\n\
  --largemem | --smallmem    Optimise for large (>4Mb) or small (<4Mb) RAM use\n\
  --no-act                   Just say what we would do - don't do it\n\
  -D|--debug=<octal>         Enable debugging - see -Dhelp or --debug=help
  --ignore-depends=<package>,... Ignore dependencies involving <package>\n\
  --force-...                    Override problems - see --force-help\n\
  --no-force-...|--refuse-...    Stop when problems encountered\n\
\n\
Comparison operators for --compare-versions are:\n\
 lt le eq ne ge gt       (treat no version as earlier than any version);\n\
 lt-nl le-nl ge-nl gt-nl (treat no version as later than any version);\n\
 < << <= = >= >> >       (only for compatibility with control file syntax).\n\
\n\
Use `" DSELECT "' for user-friendly package management.\n",
             stdout)) werr("stdout");
}

const char thisname[]= DPKG;
const char architecture[]= ARCHITECTURE;
const char printforhelp[]= "\
Type " DPKG " --help for help about installing and deinstalling packages [*];\n\
Use " DSELECT " for user-friendly package management;\n\
Type " DPKG " -Dhelp for a list of " DPKG " debug flag values;\n\
Type " DPKG " --force-help for a list of forcing options;\n\
Type " BACKEND " --help for help about manipulating *.deb files;\n\
Type " DPKG " --licence for copyright licence and lack of warranty (GNU GPL) [*].\n\
\n\
Options marked [*] produce a lot of output - pipe it through `less' or `more' !";

const struct cmdinfo *cipaction= 0;
int f_pending=0, f_recursive=0, f_alsoselect=1, f_skipsame=0, f_noact=0;
int f_autodeconf=0, f_largemem=0;
unsigned long f_debug=0;
int fc_downgrade=1, fc_configureany=0, fc_hold=0, fc_removereinstreq=0, fc_overwrite=1;
int fc_removeessential=0, fc_conflicts=0, fc_depends=0, fc_dependsversion=0;
int fc_autoselect=1, fc_badpath=0, fc_overwritediverted=0, fc_architecture=0;
int fc_nonroot=0, fc_overwritedir=0;

const char *admindir= ADMINDIR;
const char *instdir= "";
struct packageinlist *ignoredependss=0;

static const struct forceinfo {
  const char *name;
  int *opt;
} forceinfos[]= {
  { "downgrade",           &fc_downgrade                },
  { "configure-any",       &fc_configureany             },
  { "hold",                &fc_hold                     },
  { "remove-reinstreq",    &fc_removereinstreq          },
  { "remove-essential",    &fc_removeessential          },
  { "conflicts",           &fc_conflicts                },
  { "depends",             &fc_depends                  },
  { "depends-version",     &fc_dependsversion           },
  { "auto-select",         &fc_autoselect               },
  { "bad-path",            &fc_badpath                  },
  { "not-root",            &fc_nonroot                  },
  { "overwrite",           &fc_overwrite                },
  { "overwrite-diverted",  &fc_overwritediverted        },
  { "overwrite-dir",       &fc_overwritedir             },
  { "architecture",        &fc_architecture             },
  {  0                                                  }
};

static void helponly(const struct cmdinfo *cip, const char *value) {
  usage(); exit(0);
}
static void versiononly(const struct cmdinfo *cip, const char *value) {
  printversion(); exit(0);
}

static void setaction(const struct cmdinfo *cip, const char *value) {
  if (cipaction)
    badusage("conflicting actions --%s and --%s",cip->olong,cipaction->olong);
  cipaction= cip;
}

static void setdebug(const struct cmdinfo *cpi, const char *value) {
  char *endp;

  if (*value == 'h') {
    if (!fputs(
DPKG " debugging option, --debug=<octal> or -D<octal>:\n\n\
 number  ref. in source   description\n\
      1   general           Generally helpful progress information\n\
      2   scripts           Invocation and status of maintainer scripts\n\
     10   eachfile          Output for each file processed\n\
    100   eachfiledetail    Lots of output for each file processed\n\
     20   conff             Output for each configuration file\n\
    200   conffdetail       Lots of output for each configuration file\n\
     40   depcon            Dependencies and conflicts\n\
    400   depcondetail      Lots of dependencies/conflicts output\n\
   1000   veryverbose       Lots of drivel about eg the dpkg/info directory\n\
   2000   stupidlyverbose   Insane amounts of drivel\n\n\
Debugging options are be mixed using bitwise-or.\n\
Note that the meanings and values are subject to change.\n",
             stderr)) werr("stderr");
    exit(0);
  }
  
  f_debug= strtoul(value,&endp,8);
  if (*endp) badusage("--debug requires an octal argument");
}

static void setroot(const struct cmdinfo *cip, const char *value) {
  char *p;
  instdir= value;
  p= m_malloc(strlen(value) + sizeof(ADMINDIR));
  strcpy(p,value);
  strcat(p,ADMINDIR);
  admindir= p;
}

static void ignoredepends(const struct cmdinfo *cip, const char *value) {
  char *copy, *p;
  const char *pnerr;
  struct packageinlist *ni;

  copy= m_malloc(strlen(value)+2);
  strcpy(copy,value);
  copy[strlen(value)+1]= 0;
  for (p=copy; *p; p++) {
    if (*p != ',') continue;
    *p++= 0;
    if (!*p || *p==',' || p==copy+1)
      badusage("null package name in --ignore-depends comma-separated list `%.250s'",
               value);
  }
  p= copy;
  while (*p) {
    pnerr= illegal_packagename(value,0);
    if (pnerr) ohshite("--ignore-depends requires a legal package name. "
                       "`%.250s' is not; %s", value, pnerr);
    ni= m_malloc(sizeof(struct packageinlist));
    ni->pkg= findpackage(value);
    ni->next= ignoredependss;
    ignoredependss= ni;
    p+= strlen(p)+1;
  }
}

static void setforce(const struct cmdinfo *cip, const char *value) {
  const char *comma;
  int l;
  const struct forceinfo *fip;

  if (!strcmp(value,"help")) {
    if (!fputs(
DPKG " forcing options - control behaviour when problems found:\n\
  warn but continue:  --force-<thing>,<thing>,...\n\
  stop with error:    --refuse-<thing>,<thing>,... | --no-force-<thing>,...\n\
 Forcing things:\n\
  auto-select [*]        (De)select packages to install (remove) them\n\
  dowgrade [*]           Replace a package with a lower version\n\
  configure-any          Configure any package which may help this one\n\
  hold                   Process incidental packages even when on hold\n\
  bad-path               PATH is missing important programs, problems likely\n\
  not-root               Try to (de)install things even when not root\n\
  overwrite [*]          Overwrite a file from one package with another\n\
  overwrite-diverted     Overwrite a diverted file with an undiverted version\n\
  depends-version [!]    Turn dependency version problems into warnings\n\
  depends [!]            Turn all dependency problems into warnings\n\
  conflicts [!]          Allow installation of conflicting packages\n\
  architecture [!]       Process even packages with wrong architecture\n\
  overwrite-dir [!]      Overwrite one package's directory with another's file\n\
  remove-reinstreq [!]   Remove packages which require installation\n\
  remove-essential [!]   Remove an essential package\n\
\n\
WARNING - use of options marked [!] can seriously damage your installation.\n\
Forcing options marked [*] are enabled by default.\n",
               stderr)) werr("stderr");
    exit(0);
  }

  for (;;) {
    comma= strchr(value,',');
    l= comma ? (int)(comma-value) : strlen(value);
    for (fip=forceinfos; fip->name; fip++)
      if (!strncmp(fip->name,value,l) && strlen(fip->name)==l) break;
    if (!fip->name)
      badusage("unknown force/refuse option `%.*s'", l<250 ? l : 250, value);
    *fip->opt= cip->arg;
    if (!comma) break;
    value= ++comma;
  }
}

static const char *const passlongopts[]= {
  "build", "contents", "control", "info", "field", "extract", "new", "old",
  "vextract", "fsys-tarfile", 0
};

static const char passshortopts[]= "bceIfxX";
static const char okpassshortopts[]= "D";

static const struct cmdinfo cmdinfos[]= {
  /* This table has both the action entries in it and the normal options.
   * The action entries are made with the ACTION macro, as they all
   * have a very similar structure.
   */
#define ACTION(longopt,shortopt,code,function) \
 { longopt, shortopt, 0,0,0, setaction, code, 0, (voidfnp)function }
  ACTION( "install",                        'i', act_install,       archivefiles    ),
  ACTION( "unpack",                          0,  act_unpack,        archivefiles    ),
  ACTION( "record-avail",                   'A', act_avail,         archivefiles    ),
  ACTION( "configure",                       0,  act_configure,     packages        ),
  ACTION( "remove",                         'r', act_remove,        packages        ),
  ACTION( "purge",                           0,  act_purge,         packages        ),
  ACTION( "listfiles",                      'L', act_listfiles,     enqperpackage   ),
  ACTION( "status",                         's', act_status,        enqperpackage   ),
  ACTION( "get-selections",                  0,  act_getselections, getselections   ),
  ACTION( "set-selections",                  0,  act_setselections, setselections   ),
  ACTION( "print-avail",                     0,  act_printavail,    enqperpackage   ),
  ACTION( "update-avail",                    0,  act_avreplace,     updateavailable ),
  ACTION( "merge-avail",                     0,  act_avmerge,       updateavailable ),
  ACTION( "clear-avail",                     0,  act_avclear,       updateavailable ),
  ACTION( "forget-old-unavail",              0,  act_forgetold,     forgetold       ),
  ACTION( "audit",                          'C', act_audit,         audit           ),
  ACTION( "yet-to-unpack",                   0,  act_unpackchk,     unpackchk       ),
  ACTION( "list",                           'l', act_listpackages,  listpackages    ),
  ACTION( "search",                         'S', act_searchfiles,   searchfiles     ),
  ACTION( "print-architecture",              0,  act_printarch,     printarch       ),
  ACTION( "print-gnu-build-architecture",    0,  act_printgnuarch,  printarch       ),
  ACTION( "assert-support-predepends",       0,  act_assertpredep,  assertpredep    ),
  ACTION( "assert-working-epoch",            0,  act_assertepoch,   assertepoch     ),
  ACTION( "print-installation-architecture", 0,  act_printinstarch, printinstarch   ),
  ACTION( "predep-package",                  0,  act_predeppackage, predeppackage   ),
  ACTION( "compare-versions",                0,  act_cmpversions,   cmpversions     ),
  
  { "pending",           'a',  0,  &f_pending,     0,  0,             1              },
  { "recursive",         'R',  0,  &f_recursive,   0,  0,             1              },
  { "no-act",             0,   0,  &f_noact,       0,  0,             1              },
  {  0,                  'G',  0,  &fc_downgrade,  0,  0, /* alias for --refuse */ 0 },
  { "selected-only",     'O',  0,  &f_alsoselect,  0,  0,             0              },
  { "no-also-select",    'N',  0,  &f_alsoselect,  0,0,0 /* fixme: remove sometime */ },
  { "skip-same-version", 'E',  0,  &f_skipsame,    0,  0,             1              },
  { "auto-deconfigure",  'B',  0,  &f_autodeconf,  0,  0,             1              },
  { "largemem",           0,   0,  &f_largemem,    0,  0,             1              },
  { "smallmem",           0,   0,  &f_largemem,    0,  0,            -1              },
  { "root",               0,   1,  0, 0,               setroot                       },
  { "admindir",           0,   1,  0, &admindir,       0                             },
  { "instdir",            0,   1,  0, &instdir,        0                             },
  { "ignore-depends",     0,   1,  0, 0,               ignoredepends                 },
  { "force",              0,   2,  0, 0,               setforce,      1              },
  { "refuse",             0,   2,  0, 0,               setforce,      0              },
  { "no-force",           0,   2,  0, 0,               setforce,      0              },
  { "debug",             'D',  1,  0, 0,               setdebug                      },
  { "help",              'h',  0,  0, 0,               helponly                      },
  { "version",            0,   0,  0, 0,               versiononly                   },
  { "licence",/* UK spelling */ 0,0,0,0,               showcopyright                 },
  { "license",/* US spelling */ 0,0,0,0,               showcopyright                 },
  {  0,                   0                                                          }
};

static void execbackend(int argc, const char *const *argv) {
  execvp(BACKEND, (char* const*) argv);
  ohshite("failed to exec " BACKEND);
}

int main(int argc, const char *const *argv) {
  jmp_buf ejbuf;
  int c;
  const char *argp, *const *blongopts, *const *argvs;
  static void (*actionfunction)(const char *const *argv);

  if (setjmp(ejbuf)) { /* expect warning about possible clobbering of argv */
    error_unwind(ehflag_bombout); exit(2);
  }
  push_error_handler(&ejbuf,print_error_fatal,0);

  umask(022); /* Make sure all our status databases are readable. */
  
  for (argvs=argv+1; (argp= *argvs) && *argp++=='-'; argvs++) {
    if (*argp++=='-') {
      if (!strcmp(argp,"-")) break;
      for (blongopts=passlongopts; *blongopts; blongopts++) {
        if (!strcmp(argp,*blongopts)) execbackend(argc,argv);
      }
    } else {
      if (!*--argp) break;
      while ((c= *argp++)) {
        if (strchr(passshortopts,c)) execbackend(argc,argv);
        if (!strchr(okpassshortopts,c)) break;
      }
    }
  }

  myopt(&argv,cmdinfos);
  if (!cipaction) badusage("need an action option");

  setvbuf(stdout,0,_IONBF,0);
  filesdbinit();

  actionfunction= (void (*)(const char* const*))cipaction->farg;

  actionfunction(argv);

  set_error_display(0,0);
  error_unwind(ehflag_normaltidy);

  return reportbroken_retexitstatus();
}
