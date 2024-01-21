
/*
   mdadd.c : Multiple Devices tools for Linux
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
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <linux/md.h>
#include <linux/kdev_t.h>

#define MDADD_VERSION "v0.3c"

char **parser (char *, int, int *, unsigned long **);
int s2f (char *);
int open_or_die (char *file);
unsigned long calc_checksum (char *entry[], int pers);

static int compare_checksum (char *entry[], int pers, unsigned long *chk)
{
  unsigned long c=calc_checksum (entry, pers);

  if (!chk)
  {
    fprintf (stderr, "Warning : no checksum field for %s\n", entry[0]);
    return (1);
  }

  if (c!=*chk)
  {
    fprintf (stderr, "Invalid checksum for %s\n", entry[0]);
    return (0);
  }

  return (1);
}

static void do_mdadd (int fd, char *dev)
{
  struct stat s;

  if (stat (dev, &s))
  {
    perror (dev);
    return;
  }

  if (MAJOR (s.st_rdev)==FLOPPY_MAJOR)
    fprintf (stderr, "%s : warning, md cannot use floppies\n", dev);
  
  if (ioctl (fd, REGISTER_DEV, s.st_rdev))
    perror (dev);
}

static void do_mdstart (int fd, char *dev, int pers)
{
  if (ioctl (fd, START_MD, pers))
    perror (dev);
}

static void do_mdstop (int fd, char *dev)
{
  if (ioctl (fd, STOP_MD, 0))
    perror (dev);
}

int main (int argc, char *argv[])
{
  int i, j, fd, all=0, run=0, flag, pers=-1, r5op=0;
  unsigned char factor=0, fault=0;
  char *namestart=argv[0];
  unsigned long *chksum;
  char **ent;
  enum {noop, add, start, stop, op, r5d} func=noop;

  for (i=0; i<strlen (argv[0]); i++)
    if (argv[0][i]=='/')
      namestart=argv[0]+i+1;

  if (!strcmp (namestart, "mdadd"))
    func=add;
  if (!strcmp (namestart, "mdrun"))
    func=start;
  if (!strcmp (namestart, "mdstop"))
    func=stop;
  
  if (func==noop)
  {
    fprintf (stderr, "Unknown command %s\n", argv[0]);
    return (EXIT_FAILURE);
  }

  while (1)
  {
    if ((flag=getopt (argc, argv, "arvisVp:c:f:"))==-1)
      break;

    switch (flag)
    {
      case 'a':
      all=1;
      break;
      
      case 'r':
      run=1;
      break;
      
      case 'p':
      if (pers!=-1)
      {
	fprintf (stderr, "Cannot handle 2 -p options in a row\n");
	exit (EXIT_FAILURE);
      }
      
      switch (*optarg)
      {
	case 'l':
	pers=LINEAR;
	break;

	case '0':
	pers=RAID0;
	break;

	case '1':
	pers=RAID1;
	break;

	case '5':
	pers=RAID5;
	break;
	
	default:
	fprintf (stderr, "Unknown personality %s\n", optarg);
	exit (EXIT_FAILURE);
      }
      
      break;
      
      case 'c':
      factor=(unsigned char) s2f (optarg);
      break;
      
      case 'f':
      fault=(unsigned char) atoi (optarg);
      break;
      
      case 's':
      r5op=1;
      break;
      
      case 'V':
      printf ("%s " MDADD_VERSION " compiled for md " MD_VERSION "\n",
	      argv[0]);
      break;
      
      case ':':
      fprintf (stderr, "Missing parameter\n");
      exit (EXIT_FAILURE);
      
      default:
      exit (EXIT_FAILURE);
    }
  }

  pers=pers | factor | (fault << FAULT_SHIFT);
         
  if (!((func==add && all) ||
	(func==add &&  optind<argc) ||
	(func==start && all) ||
	(func==start && optind<argc && pers!=-1) ||
	(func==stop && all) ||
	(func==stop && optind<argc)))
	
  {
    fprintf (stderr, "usage :\t%s [arivVpcf] [md-device] ...\n", argv[0]);
    exit (EXIT_FAILURE);
  }

  if (!all)
    fd=open_or_die (argv[optind]);
  else
    fd=-1;			/* Should never happend ! */

  if (func==add)
  {
    if (all)
    {
      i=1;
      while ((ent=parser (NULL, i++, &pers, &chksum)))
      {
	if (!compare_checksum (ent, pers, chksum))
	  continue;
	
	fd=open_or_die (ent[0]);

	j=1;
	while (ent[j])
	{
	  do_mdadd (fd, ent[j]);
	  free (ent[j]);
	  j++;
	}

	if (run)
	  do_mdstart (fd, ent[0], pers);
	free (ent[0]);
      }
    }
    else
    {
      if ((argc-optind)==1)
	if ((ent=parser (argv[optind], 0, &pers, &chksum)))
	{
	if (!compare_checksum (ent, pers, chksum))
	  exit (EXIT_FAILURE);
	  
	  free (ent[0]);
	  
	  j=1;
	  while (ent[j])
	  {
	    do_mdadd (fd, ent[j]);
	    free (ent[j]);
	    j++;
	  }
	}
	else
	{
	  fprintf (stderr, "Unknown device %s\n", argv[optind]);
	  exit (EXIT_FAILURE);
	}
      else
	for (i=optind+1; i<argc; i++)
	  do_mdadd (fd, argv[i]);
      
      if (run)
	do_mdstart (fd, argv[optind], pers);
    }
    return (0);
  }
  
  if (func==start)
  {
    if (all)
    {
      i=1;
      while ((ent=parser (NULL, i++, &pers, &chksum)))
      {
	if (!compare_checksum (ent, pers, chksum))
	  continue;

	fd=open_or_die (ent[0]);
	do_mdstart (fd, ent[0], pers);

	j=0;
	while (ent[j])
	  free (ent[j++]);
      }
    }
    else
      if ((argc-optind)==1)
      {
	if (pers== -1 && (ent=parser (argv[optind], 0, &pers, &chksum)))
	{
	  if (!compare_checksum (ent, pers, chksum))
	    exit (EXIT_FAILURE);
	  
	  fd=open_or_die (ent[0]);
	  do_mdstart (fd, ent[0], pers);
	  
	  j=0;
	  while (ent[j])
	    free (ent[j++]);
	}
	else
	{
	  if (pers!=-1)
	  {
	    fd=open_or_die (argv[optind]);
	    do_mdstart (fd, argv[optind], pers);
	  }
	  else
	  {
	    fprintf (stderr, "Unknown device %s\n", argv[optind]);
	    exit (EXIT_FAILURE);
	  }
	}
      }
    return (0);
  }

  if (func==stop)
  {
    if (all)
    {
      i=1;
      while ((ent=parser (NULL, i++, &pers, &chksum)))
      {
	fd=open_or_die (ent[0]);
	do_mdstop (fd, ent[0]);
	free (ent[0]);

	j=1;
	while (ent[j])
	  free (ent[j++]);
      }
    }
    else
      do_mdstop (fd, argv[optind]);
	
    return (0);
  }
  
  fprintf (stderr, "Unknown command %s\n", argv[0]);
  return (EXIT_FAILURE);
}

