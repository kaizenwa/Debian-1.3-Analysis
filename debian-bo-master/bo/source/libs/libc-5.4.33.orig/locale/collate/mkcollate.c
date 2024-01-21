#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <locale.h>
#include <localeinfo.h>

#include "mkcollate.h"

extern int yyparse (void);

int check_order (mkorder_t *, int);
int write_out (unsigned char *);

void grok_order (void);

unsigned char cvalues[UCHAR_MAX + 1];
unsigned char coffsets[UCHAR_MAX + 1];

struct collate_info cinfo =
{
  0,
  NULL,
  cvalues,
  coffsets
};

mksub_t *substitutions = NULL;
mksub_t *last_sub = NULL;
int nsubs = 0;

mkorder_t *order = NULL;

unsigned char *codename;

int
main (int argc, char *argv[])
{
  int i;
  unsigned char *outname = "LC_COLLATE";

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

  if (yyparse ())
    return 1;

  if (nsubs)
    (void) fprintf (stderr, "Warning: substitutions ignored\n");

  for (i = 0; i <= UCHAR_MAX; i++)
    {
      cvalues[i] = UCHAR_MAX;
      coffsets[i] = '\0';
    }

  if (!check_order (order, 0))
    return 1;

  grok_order ();

  return !write_out (outname);
}

int
check_order (mkorder_t * node, int dep)
{
  int res = 1;

  while (node != NULL)
    {
      switch (node->node_type)
	{
	case range:
	  if (strlen (node->low) != 1 || strlen (node->top) != 1)
	    {
	      (void) fprintf (stderr, "Can't handle multibytes (%s;...;%s)\n",
			      node->low, node->top);
	      res = 0;
	    }
	  if (*(node->low) > *(node->top))
	    {
	      (void) fprintf (stderr, "Invalid range (%s;...;%s)\n",
			      node->low, node->top);
	      res = 0;
	    }
	  break;

	case primary_only:
	case secondary:
	  if (dep)
	    {
	      (void) fprintf (stderr, "Overnested order list\n");
	      res = 0;
	    }
	  else if (!check_order (node->extra, !dep))
	    res = 0;
	  break;
	}
      node = node->next;
    }
  return res;
}

void
grok_order ()
{
  mkorder_t *node = order;
  unsigned char p_order = 1;

  while (node != NULL)
    {
      switch (node->node_type)
	{
	case range:
	  {
	    unsigned int i = *node->low;

	    for (; i <= *node->top; i++)
	      {
		cvalues[i] = p_order++;
		coffsets[i] = 0;
	      }
	  }
	  break;

	case primary_only:
	  {
	    mkorder_t *node2 = node->extra;

	    while (node2 != NULL)
	      {
		switch (node2->node_type)
		  {
		  case range:
		    {
		      unsigned int i2 = *node2->low;

		      for (; i2 <= *node2->top; i2++)
			{
			  cvalues[i2] = p_order;
			  coffsets[i2] = 0;
			}
		    }
		    break;

		  default:
		    (void) fprintf (stderr, "Should not happens\n");
		    exit (4);
		  }
		node2 = node2->next;
	      }
	  }
	  p_order++;
	  break;

	case secondary:
	  {
	    unsigned char s_order = 1;
	    mkorder_t *node3 = node->extra;

	    while (node3 != NULL)
	      {
		switch (node3->node_type)
		  {
		  case range:
		    {
		      unsigned int i3 = *node3->low;

		      for (; i3 <= *node3->top; i3++)
			{
			  cvalues[i3] = p_order;
			  coffsets[i3] = s_order++;
			}
		    }
		    break;

		  default:
		    (void) fprintf (stderr, "Should not happens\n");
		    exit (4);
		  }
		node3 = node3->next;
	      }
	  }
	  p_order++;
	  break;
	}
      node = node->next;
    }
}

int
write_out (outname)
     unsigned char *outname;
{
  FILE *ofp = fopen (outname, "w");

  if (ofp == NULL)
    return 0;

  (void) fwrite (cvalues, sizeof (cvalues), 1, ofp);
  (void) fwrite (coffsets, sizeof (coffsets), 1, ofp);
  (void) fwrite (&cinfo.nsubsts, sizeof (cinfo.nsubsts), 1, ofp);
#ifndef NOGUARD
  (void) fwrite (codename, sizeof (unsigned char), strlen (codename) + 1, ofp);
#endif
  (void) fclose (ofp);
  return 1;
}
