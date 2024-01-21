/*
 * Copyright 1990 - 1994, John F. Haugh II
 * All rights reserved.
 *
 * This software is derived from the Shadow Password Suite, version 3.3.2.
 * No portion of this comment block or copyright statement may be altered
 * without the written permission of the copyright holder.
 *
 *    This library is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU Library General Public License as
 *    published by the Free Software Foundation; either version 2 of the
 *    License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Library General Public License for more details.
 *
 *    You should have received a copy of the GNU Library General Public
 *    License along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#if 0		/* To suppres gcc warning message */
#ifndef        lint
static char    copyright[] = "@(#)Copyright 1990 - 1994, John F. Haugh II";
static char    sccsid[] = "@(#)gshadow.c       1.1     12:22:51        13 Feb 1994 (GNU Shadow Library Suite)";
#endif
#endif

#include <sys/types.h>
#include <stdio.h>
#include <shadow.h>
#include <string.h>
#include <memory.h>

#undef NDBM
#undef USE_NIS

#ifdef NDBM
#include <ndbm.h>
#include <fcntl.h>
DBM    *sg_dbm;
int    sg_dbm_mode = -1;
static int     dbmopened;
static int     dbmerror;
#endif

#define        MAXMEM  1024

#ifndef        GSHADOW
#define        GSHADOW "/etc/gshadow"
#endif

#ifdef NDBM
static char    *sgrpfile = GSHADOW ;
#endif

static FILE    *shadow;
static char    sgrbuf[BUFSIZ*4];
static char    *members[MAXMEM+1];
static char    *admins[MAXMEM+1];
static struct  sgrp    sgroup;

#define        FIELDS  4

#if defined(__STDC__)
int putsgent( const struct sgrp *, FILE * );
struct sgrp *getsgnam( const char * );
struct sgrp *sgetsgent( const char *string );
#else
int putsgent( struct sgrp *, FILE * );
struct sgrp *getsgnam( char * );
struct sgrp *sgetsgent( char * );
#endif

struct sgrp *getsgent ( void );
struct sgrp *fgetsgent ( FILE * );
void endsgent( void );
void setsgent( void );
static char **list( char *, char ** );

#ifdef USE_NIS
static int     nis_used;
static int     nis_ignore;
static enum    { native, start, middle, native2 } nis_state;
static int     nis_bound;
static char    *nis_domain;
static char    *nis_key;
static int     nis_keylen;
static char    *nis_val;
static int     nis_vallen;
#define        IS_NISCHAR(c) ((c)=='+')

void __setsgNIS( int flag );
static int bind_nis( void );

/*
 * __setsgNIS - turn on or off NIS searches
 */

void
__setsgNIS( int flag )
{
       nis_ignore = ! flag;

       if (nis_ignore)
               nis_used = 0;
}

/*
 * bind_nis - bind to NIS server
 */

static int
bind_nis( void )
{
       if (yp_get_default_domain (&nis_domain))
               return -1;

       nis_bound = 1;
       return 0;
}
#endif

static char **
list( char *s, char **l )
{
       int     nmembers = 0;

       while (s && *s) {
               l[nmembers++] = s;
               if ((s = strchr (s, ',')))
                       *s++ = '\0';
       }
       l[nmembers] = (char *) 0;
       return l;
}

void
setsgent( void )
{
#ifdef NDBM
       int     mode;
#endif /* NDBM */

#ifdef USE_NIS
       nis_state = native;
#endif
       if (shadow)
               rewind (shadow);
       else
               shadow = fopen (GSHADOW, "r");

       /*
        * Attempt to open the DBM files if they have never been opened
        * and an error has never been returned.
        */

#ifdef NDBM
       if (! dbmerror && ! dbmopened) {
               char    dbmfiles[BUFSIZ];

               strcpy (dbmfiles, sgrpfile);
               strcat (dbmfiles, ".pag");

               if (sg_dbm_mode == -1)
                       mode = O_RDWR;
               else
                       mode = (sg_dbm_mode == O_RDWR) ? O_RDWR:O_RDONLY;

               if (access (dbmfiles, 0) ||
                       (! (sg_dbm = dbm_open (sgrpfile, mode, 0))))
                       dbmerror = 1;
               else
                       dbmopened = 1;
       }
#endif /* NDBM */
}

void
endsgent( void )
{
       if (shadow)
               (void) fclose (shadow);

       shadow = (FILE *) 0;
#ifdef NDBM
       if (dbmopened && sg_dbm) {
               dbm_close (sg_dbm);
               dbmopened = 0;
               sg_dbm = 0;
       }
#endif
}

#if defined(__STDC__)
struct sgrp *
sgetsgent( const char *string )
#else
struct sgrp *
sgetsgent( char *string )
#endif
{
       char    *fields[FIELDS];
       char    *cp;
       int     i;

       strncpy (sgrbuf, string, (int) sizeof sgrbuf - 1);
       sgrbuf[sizeof sgrbuf - 1] = '\0';

       if ((cp = strrchr (sgrbuf, '\n')))
               *cp = '\0';

       /*
        * There should be exactly 4 colon separated fields.  Find
        * all 4 of them and save the starting addresses in fields[].
        */

       for (cp = sgrbuf, i = 0;i < FIELDS && cp;i++) {
               fields[i] = cp;
               if ((cp = strchr (cp, ':')))
                       *cp++ = '\0';
       }

       /*
        * If there was an extra field somehow, or perhaps not enough,
        * the line is invalid.
        */

       if (cp || i != FIELDS)
#ifdef USE_NIS
               if (! IS_NISCHAR (fields[0][0]))
                       return 0;
               else
                       nis_used = 1;
#else
               return 0;
#endif

       sgroup.sg_name = fields[0];
       sgroup.sg_passwd = fields[1];
       sgroup.sg_adm = list (fields[2], admins);
       sgroup.sg_mem = list (fields[3], members);

       return &sgroup;
}

struct sgrp *
fgetsgent ( FILE *fp )
{
       char    buf[sizeof sgrbuf];
       char    *cp;

       if (! fp)
               return (0);

#ifdef USE_NIS
       while (fgets (buf, sizeof buf, fp) != (char *) 0)
#else
       if (fgets (buf, sizeof buf, fp) != (char *) 0)
#endif
       {
               if ((cp = strchr (buf, '\n')))
                       *cp = '\0';
#ifdef USE_NIS
               if (nis_ignore && IS_NISCHAR (buf[0]))
                       continue;
#endif
               return (sgetsgent (buf));
       }
       return 0;
}

/*
 * getsgent - get a single shadow group entry
 */
struct sgrp *
getsgent ( void )
{
#ifdef USE_NIS
       int     nis_1_group = 0;
       struct  sgrp    *val;
       char    buf[BUFSIZ];
#endif
       if (! shadow)
               setsgent ();

#ifdef USE_NIS
again:
       /*
        * See if we are reading from the local file.
        */

       if (nis_state == native || nis_state == native2) {

               /*
                * Get the next entry from the shadow group file.  Return
                * NULL right away if there is none.
                */

               if (! (val = fgetsgent (shadow)))
                       return 0;

               /*
                * If this entry began with a NIS escape character, we have
                * to see if this is just a single group, or if the entire
                * map is being asked for.
                */

               if (IS_NISCHAR (val->sg_name[0])) {
                       if (val->sg_name[1])
                               nis_1_group = 1;
                       else
                               nis_state = start;
               }

               /*
                * If this isn't a NIS group and this isn't an escape to go
                * use a NIS map, it must be a regular local group.
                */

               if (nis_1_group == 0 && nis_state != start)
                       return val;

               /*
                * If this is an escape to use an NIS map, switch over to
                * that bunch of code.
                */

               if (nis_state == start)
                       goto again;

               /*
                * NEEDSWORK.  Here we substitute pieces-parts of this entry.
                */

               return 0;
       } else {
               if (nis_bound == 0) {
                       if (bind_nis ()) {
                               nis_state = native2;
                               goto again;
                       }
               }
               if (nis_state == start) {
                       if (yp_first (nis_domain, "gshadow.byname", &nis_key,
                               &nis_keylen, &nis_val, &nis_vallen)) {
                               nis_state = native2;
                               goto again;
                       }
                       nis_state = middle;
               } else if (nis_state == middle) {
                       if (yp_next (nis_domain, "gshadow.byname", nis_key,
                               nis_keylen, &nis_key, &nis_keylen,
                               &nis_val, &nis_vallen)) {
                               nis_state = native2;
                               goto again;
                       }
               }
               return sgetsgent (nis_val);
       }
#else
       return (fgetsgent (shadow));
#endif
}

/*
 * getsgnam - get a shadow group entry by name
 */
#if defined(__STDC__)
struct sgrp *
getsgnam( const char *name )
#else
struct sgrp *
getsgnam( char *name )
#endif
{
       struct  sgrp    *sgrp;
#ifdef NDBM
       datum   key;
       datum   content;
#endif
#ifdef USE_NIS
       char    buf[BUFSIZ];
       static  char    save_name[16];
       int     nis_disabled = 0;
#endif

       setsgent ();

#ifdef NDBM

       /*
        * If the DBM file are now open, create a key for this group and
        * try to fetch the entry from the database.  A matching record
        * will be unpacked into a static structure and returned to
        * the user.
        */

       if (dbmopened) {
               key.dsize = strlen (name);
               key.dptr = (void *) name;

               content = dbm_fetch (sg_dbm, key);
               if (content.dptr != 0) {
                       memcpy (sgrbuf, content.dptr, content.dsize);
                       sgroup.sg_mem = members;
                       sgroup.sg_adm = admins;
                       sgr_unpack (sgrbuf, content.dsize, &sgroup);
                       return &sgroup;
               }
       }
#endif
#ifdef USE_NIS
       if (nis_used) {
again:

               /*
                * Search the gshadow.byname map for this group.
                */

               if (! nis_bound)
                       bind_nis ();

               if (nis_bound) {
                       char    *cp;

                       if (yp_match (nis_domain, "gshadow.byname", name,
                                       strlen (name), &nis_val, &nis_vallen) == 0) {
                               if (cp = strchr (nis_val, '\n'))
                                       *cp = '\0';

                               nis_state = middle;
                               if (sgrp = sgetsgent (nis_val)) {
                                       strcpy (save_name, sgrp->sg_name);
                                       nis_key = save_name;
                                       nis_keylen = strlen (save_name);
                               }
                               return sgrp;
                       }
               }
               nis_state = native2;
       }
#endif
#ifdef USE_NIS
       if (nis_used) {
               nis_ignore++;
               nis_disabled++;
       }
#endif
       while ((sgrp = getsgent ()) != (struct sgrp *) 0) {
               if (strcmp (name, sgrp->sg_name) == 0)
                       break;
       }
#ifdef USE_NIS
       nis_ignore--;
#endif
       if (sgrp)
               return sgrp;
       return (0);
}

#if defined(__STDC__)
int
putsgent( const struct sgrp *sgrp, FILE *fp )
#else
int
putsgent( struct sgrp *sgrp, FILE *fp )
#endif
{
       char    buf[sizeof sgrbuf];
       char    *cp = buf;
       int     i;

       if (! fp || ! sgrp)
               return -1;

       /*
        * Copy the group name and passwd.
        */

       strcpy (cp, sgrp->sg_name);
       cp += strlen (cp);
       *cp++ = ':';

       strcpy (cp, sgrp->sg_passwd);
       cp += strlen (cp);
       *cp++ = ':';

       /*
        * Copy the administrators, separating each from the other
        * with a ",".
        */

       for (i = 0;sgrp->sg_adm[i];i++) {
               if (i > 0)
                       *cp++ = ',';

               strcpy (cp, sgrp->sg_adm[i]);
               cp += strlen (cp);
       }
       *cp++ = ':';

       /*
        * Now do likewise with the group members.
        */

       for (i = 0;sgrp->sg_mem[i];i++) {
               if (i > 0)
                       *cp++ = ',';

               strcpy (cp, sgrp->sg_mem[i]);
               cp += strlen (cp);
       }
       *cp++ = '\n';
       *cp = '\0';

       return fputs (buf, fp);
}
