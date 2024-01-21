/*
  kpsexpand, kpsepath, kpsewhich: interface program to the Kpathsea library.

  Author:
    Thomas Esser <te@informatik.uni-hannover.de>: ideas, first version
 */

#include <kpathsea/config.h>
#include <kpathsea/c-limits.h>
#include <kpathsea/c-memstr.h>
#include <kpathsea/magstep.h>
#include <kpathsea/proginit.h>
#include <kpathsea/progname.h>
#include <kpathsea/tex-glyph.h>
#include <kpathsea/expand.h>

/* which action to perform ? */
#define KPSEWHICH 0
#define KPSEPATH  1
#define KPSEXPAND 2

static void typehelp(void);
static kpse_file_format_type format_init P1H(char *);

void kpsexpand P2H(int,  char **);
void kpsepath P2H(int, char **);
void kpsewhich P2H(int, char **);
void opt_help(void);

static string progname;

int
main P2C(int, argc, char **, argv)
{
  string mode = "";
  string virtname;
  int action;
  char opt;

  progname=(string) basename(argv[0]);
  virtname=progname; /* may be changed by "-n" option */

  if (strcmp(progname, "kpsepath") == 0)
    action = KPSEPATH;
  else if (strcmp(progname, "kpsewhich") == 0)
    action = KPSEWHICH;
  else
    action = KPSEXPAND;

  while (--argc > 0 && (*++argv)[0] == '-')
    {
      switch (opt = argv[0][1])
        {
          case 'n':
            virtname = (++argv)[0];
            --argc;
            break;
          case 'm':
            mode = (++argv)[0];
            --argc;
            break;
          case 'w':
            action = KPSEWHICH;
            break;
          case 'p':
            action = KPSEPATH;
            break;
          case 'v':
            action = KPSEXPAND;
            break;
          default:
            fprintf(stderr, "%s: unknown option '%c' ignored.\n", progname, opt);
        }
    }

  kpse_set_progname (virtname);
  kpse_init_prog ("KPSETOOL", 300, mode, false, "cmr10");

  switch(action)
    {
      case KPSEXPAND:
        kpsexpand(argc, argv);
        break;
      case KPSEPATH:
        kpsepath(argc, argv);
        break;
      case KPSEWHICH:
        kpsewhich(argc,argv);
        break;
    }
    return 0;
}

/* kpsexpand string, options are parsed by the main function */
void kpsexpand P2C(int, argc, char **, argv)
{
  if (argc != 1)
    {
      fprintf(stderr, "Usage: kpsexpand: [options] string\n");
      opt_help();
      exit(1);
    }
  printf("%s\n", kpse_expand(argv[0]));
  exit(0);
}

/* kpsepath pathtype, options are parsed by the main function */
void kpsepath P2C(int, argc, char **, argv)
{
  kpse_file_format_type file_format;
  if (argc != 1)
    {
      fprintf(stderr, "Usage: kpsepath: [options] pathtype\n");
      opt_help();
      exit(1);
    }
  file_format = format_init(argv[0]);
  printf("%s\n", kpse_format_info[file_format].path);
}

/* kpsewhich pathtype filename, options are parsed by the main function */
void kpsewhich P2C(int, argc, char **, argv)
{
  kpse_file_format_type file_format;
  char *found;

  if (argc != 2)
    {
      fprintf(stderr, "Usage: kpsewhich: [options] pathtype filename\n");
      opt_help();
      exit(1);
    }

  file_format = format_init(argv[0]);
  if ((found = kpse_find_file (argv[1], file_format, true)) != NULL)
    {
      printf("%s\n", found);
      exit(0);
    }
  else
    {
      fprintf(stderr, "%s: %s not found.\n", progname, argv[1]);
      exit(1);
    }
}


static kpse_file_format_type format_init P1C(char *, pathtype)
{

  kpse_file_format_type file_format;

  /* this is a bit ugly, but that way, we do not need to
     know the order in tex-file.h */
  if (strcmp("gf", pathtype) == 0)
    file_format = kpse_gf_format;
  else if (strcmp("pk", pathtype) == 0)
    file_format = kpse_pk_format;
  else if (strcmp("glyph", pathtype) == 0)
    file_format = kpse_any_glyph_format;
  else if (strcmp("base", pathtype) == 0)
    file_format = kpse_base_format;
  else if (strcmp("bib", pathtype) == 0)
    file_format = kpse_bib_format;
  else if (strcmp("bst", pathtype) == 0)
    file_format = kpse_bst_format;
  else if (strcmp("cnf", pathtype) == 0)
    file_format = kpse_cnf_format;
  else if (strcmp("fmt", pathtype) == 0)
    file_format = kpse_fmt_format;
  else if (strcmp("mem", pathtype) == 0)
    file_format = kpse_mem_format;
  else if (strcmp("mf", pathtype) == 0)
    file_format = kpse_mf_format;
  else if (strcmp("mfpool", pathtype) == 0)
    file_format = kpse_mfpool_format;
  else if (strcmp("mp", pathtype) == 0)
    file_format = kpse_mp_format;
  else if (strcmp("mppool", pathtype) == 0)
    file_format = kpse_mppool_format;
  else if (strcmp("mpsupport", pathtype) == 0)
    file_format = kpse_mpsupport_format;
  else if (strcmp("pict", pathtype) == 0)
    file_format = kpse_pict_format;
  else if (strcmp("tex", pathtype) == 0)
    file_format = kpse_tex_format;
  else if (strcmp("texpool", pathtype) == 0)
    file_format = kpse_texpool_format;
  else if (strcmp("tfm", pathtype) == 0)
    file_format = kpse_tfm_format;
  else if (strcmp("vf", pathtype) == 0)
    file_format = kpse_vf_format;
  else if (strcmp("dvips_config", pathtype) == 0)
    file_format = kpse_dvips_config_format;
  else if (strcmp("dvips_header", pathtype) == 0)
    file_format = kpse_dvips_header_format;
  else if (strcmp("troff_font", pathtype) == 0)
    file_format = kpse_troff_font_format;
  else
    {
      fprintf(stderr, "%s: unknown pathtype '%s'. Aborted.\n\n", progname, pathtype);
      typehelp();
      exit(1);
    }
  kpse_init_format(file_format);
  kpse_format_info[file_format].suffix_search_only = false;
  return(file_format);
}

/* help entries stolen from kpathsea.texi :-) */
static char *typehelparr[] = {
"Valid pathtypes are:",
"  gf            : generic font bitmap",
"  pk            : packed bitmap font",
"  base          : Metafont memory dump",
"  bib           : BibTeX bibliography source",
"  bst           : BibTeX style files",
"  cnf           : Kpathsea runtime configuration files",
"  fmt           : TeX memory dump",
"  mem           : MetaPost memory dump",
"  mf            : Metafont source",
"  mfpool        : Metafont program strings",
"  mp            : MetaPost source",
"  mppool        : MetaPost program strings",
"  mpsupport     : MetaPost support files",
"  pict          : Other kinds of figures",
"  tex           : TeX source",
"  texpool       : TeX program strings",
"  tfm           : TeX font metrics",
"  vf            : virtual font",
"  dvips_config  : dvips config files",
"  dvips_header  : dvips header files",
"  troff_font    : troff fonts", 0};

/* idea stolen from dvips.c */
static void typehelp()
{
   char **p ;

   for (p=typehelparr; *p; p++)
      fprintf(stderr, "%s\n", *p) ;
}

void opt_help()
{
	fprintf(stderr, "\nValid options are the following:\n");
	fprintf(stderr, "  -n progname  : pretend to be progname to kpathsea\n");
	fprintf(stderr, "  -m mode      : set Metafont mode\n");
	fprintf(stderr, "  -w           : act like kpsewhich\n");
	fprintf(stderr, "  -p           : act like kpsepath\n");
	fprintf(stderr, "  -v           : act like kpsexpand\n\n");
}
