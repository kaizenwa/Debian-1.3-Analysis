/*
 * dpkg - main program for package management
 * dbmodify.c - routines for managing dpkg database updates
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
#include <assert.h>

#include "config.h"
#include "dpkg.h"
#include "dpkg-db.h"

char *statusfile=0, *availablefile=0;

static enum modstatdb_rw cstatus=-1, cflags=0;
static char *importanttmpfile=0;
static FILE *importanttmp;
static int nextupdate;
static int updateslength;
static char *updatefnbuf, *updatefnrest;
static const char *admindir;
static struct varbuf uvb;

static int ulist_select(const struct dirent *de) {
  const char *p;
  int l;
  for (p= de->d_name, l=0; *p; p++, l++)
    if (!isdigit(*p)) return 0;
  if (l > IMPORTANTMAXLEN)
    ohshit("updates directory contains file `%.250s' whose name is too long "
           "(length=%d, max=%d)", de->d_name, l, IMPORTANTMAXLEN);
  if (updateslength == -1) updateslength= l;
  else if (l != updateslength)
    ohshit("updates directory contains files with different length names "
           "(both %d and %d)", l, updateslength);
  return 1;
}

static void cleanupdates(void) {
  struct dirent **cdlist;
  int cdn, i;

  parsedb(statusfile, pdb_weakclassification, 0,0,0);

  *updatefnrest= 0;
  updateslength= -1;
  cdn= scandir(updatefnbuf, &cdlist, &ulist_select, alphasort);
  if (cdn == -1) ohshite("cannot scan updates directory `%.255s'",updatefnbuf);

  if (cdn) {
    
    for (i=0; i<cdn; i++) {
      strcpy(updatefnrest, cdlist[i]->d_name);
      parsedb(updatefnbuf, pdb_weakclassification, 0,0,0);
      if (cstatus < msdbrw_write) free(cdlist[i]);
    }

    if (cstatus >= msdbrw_write) {
      writedb(statusfile,0,1);
    
      for (i=0; i<cdn; i++) {
        strcpy(updatefnrest, cdlist[i]->d_name);
        if (unlink(updatefnbuf))
          ohshite("failed to remove incorporated update file %.255s",updatefnbuf);
        free(cdlist[i]);
      }
    }
    
  }
  free(cdlist);

  nextupdate= 0;
}

static void createimptmp(void) {
  int i;
  
  onerr_abort++;
  
  importanttmp= fopen(importanttmpfile,"w");
  if (!importanttmp) ohshite("unable to create %.250s",importanttmpfile);
  for (i=0; i<512; i++) fputs("#padding\n",importanttmp);
  if (ferror(importanttmp))
    ohshite("unable to fill %.250s with padding",importanttmpfile);
  if (fflush(importanttmp))
    ohshite("unable flush %.250s after padding",importanttmpfile);
  if (fseek(importanttmp,0,SEEK_SET))
    ohshite("unable seek to start of %.250s after padding",importanttmpfile);

  onerr_abort--;
}

enum modstatdb_rw modstatdb_init(const char *adir, enum modstatdb_rw readwritereq) {
  static const struct fni { const char *suffix; char **store; } fnis[]= {
    {   STATUSFILE,                 &statusfile         },
    {   AVAILFILE,                  &availablefile      },
    {   UPDATESDIR IMPORTANTTMP,    &importanttmpfile   },
    {   0                                               }
  }, *fnip;

  admindir= adir;
  
  for (fnip=fnis; fnip->suffix; fnip++) {
    free(*fnip->store);
    *fnip->store= m_malloc(strlen(adir)+strlen(fnip->suffix)+2);
    sprintf(*fnip->store, "%s/%s", adir, fnip->suffix);
  }

  cflags= readwritereq & msdbrw_flagsmask;
  readwritereq &= ~msdbrw_flagsmask;

  switch (readwritereq) {
  case msdbrw_needsuperuser:
  case msdbrw_needsuperuserlockonly:
    if (getuid() || geteuid())
      ohshit("requested operation requires superuser privilege");
    /* fall through */
  case msdbrw_write: case msdbrw_writeifposs:
    if (access(adir,W_OK)) {
      if (errno != EACCES)
        ohshite("unable to access dpkg status area");
      else if (readwritereq == msdbrw_write)
        ohshit("operation requires read/write access to dpkg status area");
      cstatus= msdbrw_readonly;
    } else {
      lockdatabase(adir);
      cstatus= (readwritereq == msdbrw_needsuperuserlockonly ?
                msdbrw_needsuperuserlockonly :
                msdbrw_write);
    }
    break;
  case msdbrw_readonly:
    cstatus= msdbrw_readonly; break;
  default:
    internerr("unknown readwritereq");
  }

  updatefnbuf= m_malloc(strlen(adir)+sizeof(UPDATESDIR)+IMPORTANTMAXLEN+5);
  strcpy(updatefnbuf,adir);
  strcat(updatefnbuf,"/" UPDATESDIR);
  updatefnrest= updatefnbuf+strlen(updatefnbuf);

  if (cstatus != msdbrw_needsuperuserlockonly) {
    cleanupdates();
    parsedb(availablefile,
            pdb_recordavailable|pdb_rejectstatus,
            0,0,0);
  }

  if (cstatus >= msdbrw_write) {
    createimptmp();
    uvb.used= 0;
    uvb.size= 10240;
    uvb.buf= m_malloc(uvb.size);
  }

  return cstatus;
}

static void checkpoint(void) {
  int i;

  assert(cstatus >= msdbrw_write);
  writedb(statusfile,0,1);
  
  for (i=0; i<nextupdate; i++) {
    sprintf(updatefnrest, IMPORTANTFMT, i);
    assert(strlen(updatefnrest)<=IMPORTANTMAXLEN); /* or we've made a real mess */
    if (unlink(updatefnbuf))
      ohshite("failed to remove my own update file %.255s",updatefnbuf);
  }
  nextupdate= 0;
}

void modstatdb_shutdown(void) {
  switch (cstatus) {
  case msdbrw_write:
    checkpoint();
    writedb(availablefile,1,0);
    /* tidy up a bit, but don't worry too much about failure */
    fclose(importanttmp);
    strcpy(updatefnrest, IMPORTANTTMP); unlink(updatefnbuf);
    varbuffree(&uvb);
    /* fall through */
  case msdbrw_needsuperuserlockonly:
    unlockdatabase(admindir);
  default:
    break;
  }

  free(updatefnbuf);
}

void modstatdb_note(struct pkginfo *pkg) {
  assert(cstatus >= msdbrw_write);

  onerr_abort++;

  varbufreset(&uvb);
  varbufrecord(&uvb, pkg, &pkg->installed);
  if (fwrite(uvb.buf, 1, uvb.used, importanttmp) != uvb.used)
    ohshite("unable to write updated status of `%.250s'", pkg->name);
  if (fflush(importanttmp))
    ohshite("unable to flush updated status of `%.250s'", pkg->name);
  if (ftruncate(fileno(importanttmp), uvb.used))
    ohshite("unable to truncate for updated status of `%.250s'", pkg->name);
  if (fsync(fileno(importanttmp)))
    ohshite("unable to fsync updated status of `%.250s'", pkg->name);
  if (fclose(importanttmp))
    ohshite("unable to close updated status of `%.250s'", pkg->name);
  sprintf(updatefnrest, IMPORTANTFMT, nextupdate);
  if (rename(importanttmpfile, updatefnbuf))
    ohshite("unable to install updated status of `%.250s'", pkg->name);
  assert(strlen(updatefnrest)<=IMPORTANTMAXLEN); /* or we've made a real mess */

  nextupdate++;  

  if (nextupdate > MAXUPDATES) checkpoint();

  createimptmp();

  onerr_abort--;
}
