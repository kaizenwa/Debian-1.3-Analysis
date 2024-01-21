
/*
   mdparse.c : Multiple Devices tools for Linux
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
#include <limits.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <linux/md.h>

#define MDTAB_FILE "/etc/mdtab"

static FILE *tab=NULL;

void mdtab_open (void)
{
  if (!(tab=fopen (MDTAB_FILE, "r")))
  {
    perror (MDTAB_FILE);
    exit (EXIT_FAILURE);
  }
}

int s2f (char *s)
{
  int l, size, f, c, i;
  
  if (!s || !(l=strlen (s)))
    return 0;

  size=atoi(s);
  if (s[l-1]!='k')
    return (size);

  f=size >> (PAGE_SHIFT - 10);
  for (i=1, c=0; i<size; i*=2)
    if (f & i) c++;
  
  if ((size*1024) % PAGE_SIZE || c!=1)
  {
    fprintf (stderr, "Cannot handle %dk chunks. Defaulting to %dk\n",
	     size, 1 << (PAGE_SHIFT-10));
    return (0);
  }

  for (i=0; !(f & (1 << i)); i++);
  return (i);
}

char **parser (char *md_dev, int number,
	       int *personality, unsigned long **chksum)
{
  int c, pos, len, count=0, i=1, x;
  short factor=0, fault=0;
  char line[256], dummy[256], options[256], sfactor[256];
  static char *ent[10]; /* Max 9 dev/md */
  static unsigned long chk;

  if (!tab)
    mdtab_open ();

  rewind (tab);
  
  while (!feof (tab))
  {
    /* Parse les espaces de debut */
    fscanf (tab, " ");

    /* Recherche de commentaires */
    while ((c=fgetc (tab))=='#')
      fscanf (tab, "%*[^\n] ");	/* Ignore toute la fin de la ligne */

    ungetc (c, tab);

    if (feof (tab))
      return NULL;
    
    /* Parse la ligne complete
       FIXME : Pas de ligne de plus de 255 caracteres */
    fscanf (tab, "%255[^#\n]", line);

    /* Parse le md_dev */
    ent[0]=(char *) malloc (256);
    sscanf (line, "%255[^ \t]%255[ \t]", ent[0], dummy);
    
    count++;
    if ((md_dev && strcmp (md_dev, ent[0])) ||
	(!md_dev && count<number))
    {
      free (ent[0]);
      continue;
    }

    len=strlen (line);
    pos=strlen (ent[0])+strlen (dummy)+1;

    sscanf (line+pos-1, "%255[^ \t]%255[ \t]", options, dummy);
    pos+=strlen (options)+strlen (dummy);

    x=sscanf (options, "%255[^,],%255[^,],%hd,%lx",
	      dummy, sfactor, &fault, &chk);
    if (x<4)
      *chksum=NULL;
    else
      *chksum=&chk;
    
    factor=s2f (sfactor);

    *personality=0;
    
    if (!strcmp (dummy, "linear"))
      *personality=LINEAR;

    if (!strcmp (dummy, "stripped") || /* This spelling mistake will last... */
	!strcmp (dummy, "striped") ||
	!strcmp (dummy, "raid0"))
      *personality=STRIPED | (factor & FACTOR_MASK);

    if (!strcmp (dummy, "raid1"))
      *personality=(RAID1 |
		    (factor & FACTOR_MASK) |
		    ((fault << FAULT_SHIFT) & FAULT_MASK));
        
    if (!strcmp (dummy, "raid5"))
      *personality=(RAID5 |
		    (factor & FACTOR_MASK) |
		    ((fault << FAULT_SHIFT) & FAULT_MASK));
        
    if (!*personality)
    {
      fprintf (stderr, "Unknown option %s\n", options);
      exit (EXIT_FAILURE);
    }
    
    while (pos<(len-1))
    {
      ent[i]=(char *) malloc (256);
      sscanf (line+pos-1, "%255[^ \t]%255[ \t]", ent[i], dummy);
      pos+=strlen (ent[i])+strlen (dummy);
      i++;
    }
    ent[i]=NULL;

    return (ent);
  }
  return (NULL);
}

