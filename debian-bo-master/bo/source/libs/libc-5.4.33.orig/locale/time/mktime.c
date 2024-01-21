#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <locale.h>
#include <localeinfo.h>

unsigned char *codename;

struct time_info tinfo;

unsigned char **lines[] =
{
  (unsigned char **) &tinfo.abbrev_wkday[0],
  (unsigned char **) &tinfo.abbrev_wkday[1],
  (unsigned char **) &tinfo.abbrev_wkday[2],
  (unsigned char **) &tinfo.abbrev_wkday[3],
  (unsigned char **) &tinfo.abbrev_wkday[4],
  (unsigned char **) &tinfo.abbrev_wkday[5],
  (unsigned char **) &tinfo.abbrev_wkday[6],
  (unsigned char **) &tinfo.full_wkday[0],
  (unsigned char **) &tinfo.full_wkday[1],
  (unsigned char **) &tinfo.full_wkday[2],
  (unsigned char **) &tinfo.full_wkday[3],
  (unsigned char **) &tinfo.full_wkday[4],
  (unsigned char **) &tinfo.full_wkday[5],
  (unsigned char **) &tinfo.full_wkday[6],
  (unsigned char **) &tinfo.abbrev_month[0],
  (unsigned char **) &tinfo.abbrev_month[1],
  (unsigned char **) &tinfo.abbrev_month[2],
  (unsigned char **) &tinfo.abbrev_month[3],
  (unsigned char **) &tinfo.abbrev_month[4],
  (unsigned char **) &tinfo.abbrev_month[5],
  (unsigned char **) &tinfo.abbrev_month[6],
  (unsigned char **) &tinfo.abbrev_month[7],
  (unsigned char **) &tinfo.abbrev_month[8],
  (unsigned char **) &tinfo.abbrev_month[9],
  (unsigned char **) &tinfo.abbrev_month[10],
  (unsigned char **) &tinfo.abbrev_month[11],
  (unsigned char **) &tinfo.full_month[0],
  (unsigned char **) &tinfo.full_month[1],
  (unsigned char **) &tinfo.full_month[2],
  (unsigned char **) &tinfo.full_month[3],
  (unsigned char **) &tinfo.full_month[4],
  (unsigned char **) &tinfo.full_month[5],
  (unsigned char **) &tinfo.full_month[6],
  (unsigned char **) &tinfo.full_month[7],
  (unsigned char **) &tinfo.full_month[8],
  (unsigned char **) &tinfo.full_month[9],
  (unsigned char **) &tinfo.full_month[10],
  (unsigned char **) &tinfo.full_month[11],
  (unsigned char **) &tinfo.ampm[0],
  (unsigned char **) &tinfo.ampm[1],
  (unsigned char **) &tinfo.date_time,
  (unsigned char **) &tinfo.date,
  (unsigned char **) &tinfo.time,
  (unsigned char **) &tinfo.ut0,
  (unsigned char **) &tinfo.tz,
  NULL
};

int parse (void);

int
main (int argc, char *argv[])
{
  int i;
  unsigned char *outname = "LC_TIME";

  while ((i = getopt (argc, argv, "o:")) != EOF)
    {
      switch (i)
	{
	case 'o':
	  outname = optarg;
	  break;
	}
    }

  if (argc - optind > 1)
    {
      (void) fprintf (stderr, "Usage: %s [-o out_file_name] [file]\n", argv[0]);
      return 3;
    }
  else if ((argc - optind) == 1)
    {
      if (freopen (argv[optind], "r", stdin) == NULL)
	{
	  perror (argv[optind]);
	  return 2;
	}
    }

  if (!parse ())
    return 1;

  return !write_out (outname);
}

void
write_str (char *str, FILE * ofp)
{
  short int slen = strlen (str) + 1;

  (void) fwrite (&slen, sizeof (slen), 1, ofp);
  (void) fwrite (str, sizeof (char), slen, ofp);
}

int
write_out (outname)
     unsigned char *outname;
{
  unsigned char ***target = lines;
  FILE *ofp = fopen (outname, "w");

  if (ofp == NULL)
    return 0;

  while (*target != NULL)
    {
      write_str (**target, ofp);
      target++;
    }
#ifndef NOGUARD
  (void) fwrite (codename, sizeof (unsigned char), strlen (codename) + 1, ofp);
#endif
  (void) fclose (ofp);
  return 1;
}

unsigned char iline[1024];

int
parse ()
{
  int codename_seen = 0;
  int lineno = 0;
  unsigned char ***target = lines;
  unsigned char *cp;

  while (fgets (iline, sizeof (iline), stdin) != NULL)
    {
      lineno++;
      if (iline[0] == '#')
	continue;
      if ((cp = strchr (iline, '\n')) == NULL)
	{
	  (void) fprintf (stderr, "Line %d: buffer overflow\n", lineno);
	  return 0;
	}
      *cp = '\0';
      if (strlen (iline) == 0)
	continue;
      if (!codename_seen)
	{
	  if (grok_codename ())
	    {
	      codename_seen = 1;
	      continue;
	    }
	  else
	    return 0;
	}
      **target++ = strdup (iline);
      if (*target == NULL)
	return 1;
    }
  return 0;
}

int
grok_codename ()
{
  unsigned char *cp = iline;

  while (*cp && *cp == ' ' && *cp == '\t')
    cp++;
  if (*cp == '\0')
    return 0;
  if (strncmp (cp, "codeset ", 8) && strncmp (cp, "codeset\t", 8))
    return 0;
  cp += 8;
  while (*cp && *cp == ' ' && *cp == '\t')
    cp++;
  if (*cp == '\0')
    return 0;
  codename = strdup (cp);
  return 1;
}
