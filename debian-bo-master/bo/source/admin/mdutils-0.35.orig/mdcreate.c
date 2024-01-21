
/*
   mdcreate.c : Multiple Devices tools for Linux
                Copyright (C) 1994-96 Marc ZYNGIER
	        <zyngier@ufr-info-p7.ibp.fr> or
	        <maz@gloups.fdn.fr>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   You should have received a copy of the GNU General Public License
   (for example /usr/src/linux/COPYING); if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <linux/md.h>

char **parser (char *, int, int *, unsigned long **);
int s2f (char *);
int open_or_die (char *file);
unsigned long calc_checksum (char *entry[], int pers);

int main (int argc, char *argv[])
{
  int flag, fault=0, factor=0, pers=0, i;
  unsigned long *dummy_long;
  FILE *mdtab;
  
  /*
   * Expected syntax is :
   * mdcreate [-cx] [-fx] personality /dev/mdx /dev/... /dev/...
   */
  
  while (1)
  {
    if ((flag=getopt (argc, argv, "c:f:"))==-1)
      break;

    switch (flag)
    {
      case 'c':
      factor=(unsigned char) s2f (optarg);
      break;
      
      case 'f':
      fault=(unsigned char) atoi (optarg);
      break;
    }
  }

  if (argc<(optind+3))
  {
    fprintf (stderr, "Not enough parameters\n");
    exit (EXIT_FAILURE);
  }

  /* Create /etc/mdtab if it doesn't exists
     Thanx to Nat <nat@nataa.frmug.fr.net> */
  
  if ((i=open ("/etc/mdtab", O_RDONLY | O_CREAT))==-1)
  {
    perror ("/etc/mdtab");
    exit (EXIT_FAILURE);
  }

  close (i);

  if (parser (argv[optind+1], 0, &pers, &dummy_long))
  {
    fprintf (stderr, "%s already exists in /etc/mdtab\n", argv[optind+1]);
    exit (EXIT_FAILURE);
  }

  if (!(mdtab=fopen ("/etc/mdtab", "r+")))
  {
    perror ("/etc/mdtab");
    exit (EXIT_FAILURE);
  }

  if (!strcmp (argv[optind], "linear"))
    pers=LINEAR;
  
  if (!strcmp (argv[optind], "stripped") ||
      !strcmp (argv[optind], "striped") ||
      !strcmp (argv[optind], "raid0"))
    pers=STRIPED | (factor & FACTOR_MASK);
  
  if (!strcmp (argv[optind], "raid1"))
    pers=(RAID1 |
	  (factor & FACTOR_MASK) |
	  ((fault << FAULT_SHIFT) & FAULT_MASK));
  
  if (!strcmp (argv[optind], "raid5"))
    pers=(RAID5 |
	  (factor & FACTOR_MASK) |
	  ((fault << FAULT_SHIFT) & FAULT_MASK));

  if (!pers)
  {
    fprintf (stderr, "Unknown personality %s\n", argv[optind]);
    exit (EXIT_FAILURE);
  }

  fseek (mdtab, 0L, SEEK_END);

  fprintf (mdtab, "# mdtab entry for %s\n%s\t%s,%dk,%d,%08lx\t",
	   argv[optind+1],
	   argv[optind+1],
	   argv[optind],
	   1 << (PAGE_SHIFT-10+factor),
	   fault,
	   calc_checksum (argv+optind+1, pers));
  
  for (i=optind+2; i<argc; i++)
    fprintf (mdtab, "%s ", argv[i]);
  
  fprintf (mdtab, "\n");
  fclose (mdtab);
  
  return (EXIT_SUCCESS);
}
