/* Copyright (C) 1994 - 1996 
 *          Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)
 *
 *   diskid.c  Functions to calculate disk ids.
 *             This is compatible to xmcd and in fact the code was
 *             taken from xmcd according to the GNU license.
 *
 *   Copyright of the original code: (C) 1994  Ti Kan
 *                                   E-mail: ti@amb.org
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include "../config.h"

#include <McTools/McApp.h>
#include <McTools/McResource.h>
#include <McTools/McGadget.h>
#include <McTools/McOrder.h>
#include <McTools/McDigits.h>
#include <McTools/McInfoRequest.h>
#include "struct.h"
#include "cd.h"
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <strings.h>
#include <errno.h>
#include <stdlib.h>

#include "discid.h"

/*
 * prg_sum
 *	Convert an integer to its text string representation, and
 *	compute its checksum.  Used by get_discid to derive the
 *	disc ID.
 *
 * Args:
 *	n - The integer value.
 *
 * Return:
 *	The integer checksum.
 */
static int prg_sum(unsigned long n) {
  char	buf[12], *p;
  int	ret = 0;

  /* For backward compatibility this algorithm must not change */
  sprintf(buf, "%lu", n);
  for (p = buf; *p != '\0'; p++)
    ret += (*p - '0');

  return(ret);
}


/*
 * get_discid
 *	Compute a magic disc ID based on the number of tracks,
 *	the length of each track, and a checksum of the string
 *	that represents the offset of each track.
 *
 * Return:
 *	The integer disc ID.
 */
unsigned long get_discid(void) {
  int	i, t = 0, n = 0;
  /* For backward compatibility this algorithm must not change */
  for (i = 0; i < thiscd.ntracks; i++) {
    n += prg_sum(thiscd.trk[i].start / 75);
    t += thiscd.trk[i].length;
  }
  return((n % 0xff) << 24 | t << 8 | thiscd.ntracks);
}

/*
 * find_db_file(char *path, int discid)
 *	Search for a db file
 *
 * Return:
 *	The absolute path as a malloc'ed string,
 *      or NULL on failure.
 */
static unsigned char *find_db_file(unsigned char *path, int discid) {
  unsigned char *epath;
  unsigned char file[10];
  DIR *dir;
  int plen;
  struct dirent *dre;
  struct stat stb;

  if (!path) path=".";

  sprintf(file,"%08x",discid);

  epath=(unsigned char *)calloc((plen=strlen(path))+NAME_MAX+16, 1);
  strcpy(epath,path);
  epath[plen++]='/';

  if (!(dir=opendir(path))) {
    free(epath);
    return NULL;
  }

  while ((dre=readdir(dir))) {
    if (strcmp(dre->d_name,"..")) {
      if (strcmp(dre->d_name,".")) {
	strcpy(epath+plen,dre->d_name);
	strcat(epath+plen,"/");
      }
      strcat(epath+plen,file);
      if (!stat(epath, &stb)) {
	closedir(dir);
	return epath;
      }
    }
  }
  closedir(dir);
  free(epath);
  return NULL;
}

/*
 * parse_db_line(unsigned char *buf)
 *	Parse a line and act accordingly.
 *
 */
static void parse_db_line(unsigned char *buf) {

  if (*buf!='#') {
    if (buf[strlen(buf)-1]=='\n') buf[strlen(buf)-1]=0;

    if (!strncmp(buf,"DTITLE=",7)) {
      strncpy(thiscd.cdname,buf+7,83);
      return;
    }

    if (!strncmp(buf,"TTITLE",6)) {
      int tr,aa=0;
      sscanf(buf+6,"%d=%n",&tr,&aa);
      if ((tr>=0) && (tr<thiscd.ntracks)) {
	strncpy(thiscd.trk[tr].songname,buf+aa+6,83);
      }
      return;
    }
    if (!strncmp(buf,"PLAYORDER=",10)) {
      /* explode it... */
      McOrderItem *it;
      int i,n,hasitems=0;
      char *l=(char *)calloc(thiscd.ntracks * sizeof(char), 1);
      free_songs();
      it=McCreateOrderItem(NULL);
      ORDER(Gad[ORD_SONGS])->first=it;
      it->id=ID_START;
      while ((*buf) && ((*buf<'0') || (*buf>'9'))) buf++;
      while(*buf) {
	n=0;
	while((*buf>='0') && (*buf<='9')) n=n*10+((*buf++)-'0');
	if (n>0 && n<=thiscd.ntracks) {
	  it=McCreateOrderItem(it);
	  it->id=n;
	  hasitems=1;
	  l[n-1]=1;
	}
	while ((*buf) && ((*buf<'0') || (*buf>'9'))) buf++;
      }
      if (hasitems) {
	it=McCreateOrderItem(it);
	it->id=ID_STOP;
      }
      for (i=0;i<thiscd.ntracks;i++) {
	if (!l[i]) {
	  it=McCreateOrderItem(it);
	  it->id=i+1;
	}
      }
      if (!hasitems) {
	it=McCreateOrderItem(it);
	it->id=ID_STOP;
      }
      McGadgetRedraw(Gad[ORD_SONGS]);
      free(l);
      return;
    }
  }
}

static unsigned char *read_db_file(unsigned char *file) {
  unsigned char buf[512];
  int i;
  FILE *f;

  *thiscd.cdname=0;
  for (i = 0; i < thiscd.ntracks; i++) {
    *thiscd.trk[i].songname=0;
  }

  if (file) {
    if ((f=fopen(file,"r"))) {
      while (fgets(buf,512,f)) {
	parse_db_line(buf);
      }
      fclose(f);
    }
    return file;
  }
  return 0;
}

unsigned char *read_db(void) {
  unsigned char *path, *file, *savepath, *tmp;
  unsigned long id = get_discid();
  int noresource=1;

  if ((savepath=path=McGetResource(app, "cddb"))) {
    noresource=0;
    path = McMakePathAbsolute(path);
    if ((file=find_db_file(path,id))) {
      if ((file=read_db_file(file))) {
	free(path);
	return file;
      }
    }
    free(path);
  }

  if ((path=McGetResource(app, "systemCddb"))) {
    noresource=0;
    path = McMakePathAbsolute(path);
    if ((file=find_db_file(path,id))) {
      read_db_file(file);
    } else {
      free(path);
      return 0;
    }

    if ((!savepath) || (strncmp(path, file, strlen(path)))) {
      free(path);
      free(file);
      return 0;
    }

    tmp = McMakePathAbsolute(savepath);
    tmp = realloc(tmp, strlen(tmp)+strlen(file+strlen(path)) + 2);
    strcat(tmp, file+strlen(path));
    free(path);
    free(file);
#ifdef DEBUG
    printf("Savefile: '%s'\n", tmp);
#endif
    return tmp;
  }

  if (noresource)
    McError(app,
	    _("Error: Neither *cddb nor *systemCddb resource is set.\n"
	      "  The cd database won't work and you will get this nasty\n"
	      "  message until you set up a proper resource..."));

  return 0;
}


/*
 * mkdirhier(char *path)
 *
 * create a complete directory hirarchy to hold file specified by path
 *
 */
static void mkdirhier(char *path) {
  char *p, *p2;
  struct stat st;
  int umsk;

  /* First check if the directory already exists. */

  if ((p=strrchr(path, '/'))) {
    *p=0;
    if (!stat(path, &st)) {
      if ((st.st_mode&S_IFMT)==S_IFDIR) {
	*p='/';
	return;
      }
    }
    *p='/';
  }

  /* If not, scan each path component and try to create it if it is
   * missing.
   * Be sure to use the users umask for directory creation.
   */

  umsk = umask(077); umask(umsk); /* There should be a way to read it
				   * without changing it. */

  if (*path=='/') p2=path+1; else p2=path;

  while ((p=strchr(p2, '/'))) {
    *p=0;
    if (stat(path, &st)) {
      if (mkdir(path, 0755 & (~umsk))) {
	McError(app, _("Can't create directory '%s'\nReason: %s\n"),
		path,strerror(errno));
	*p='/';
	return;
      }
    } else {
      if ((st.st_mode&S_IFMT)!=S_IFDIR) {
	*p='/';
	McError(app, 
		_("Can't create directory '%s'\nReason: File exists\n"), path);
	return;
      }
    }
    *p='/';
    p2=p+1;
  }
}

  
/*
 * write_db(unsigned char *file)
 *
 * Write a new db file
 *
 */

void write_db(unsigned char *file) {
  int i;
  McOrderItem *it;
  unsigned char *c, *path;
  FILE *f;

  if (!file) {
    if ((path=McGetResource(app, "cddb"))) {
      path = McMakePathAbsolute(path);
      file = (unsigned char *)alloca((i=strlen(path))+10);
      strcpy(file, path);
      if (i && file[i-1]!='/') file[i++]='/';
      sprintf(&file[i],"%08lx", get_discid());
      free(path);
    } else {
      McError(app, _("I don't know where to safe your stuff, since the\n"
		     "xplaycd*cddb resource is not set. I'm confused.\n"
		     "I won't save anything.\n"));
      return;
    }
  }

  mkdirhier(file);
  if ((f=fopen(file,"w"))) {
    fputs("# xplaycd & xmcd 1.0 CD database file\n", f);
    fputs("# Copyright: xmcd & file layout: (C) 1994 Ti Kan\n", f);
    fputs("#            xplaycd:            (C) 1994 Olav Woelfelschneider\n",
	  f);
    fputs("#\n",f);
    fputs("# Track frame offsets:\n",f);
    for (i = 0; i < thiscd.ntracks; i++) {
      fprintf(f, "#\t%d\n", thiscd.trk[i].start);
    }
    fputs("#\n",f);
    fprintf(f, "# Disc length: %d seconds\n", thiscd.length);
    fputs("#\n",f);

    fprintf(f, "DISCID=%08lx\n", get_discid());
    if (!(c=thiscd.cdname)) c="";
    fprintf(f, "DTITLE=%s\n", c);
    for (i = 0; i < thiscd.ntracks; i++) {
      if (!(c=thiscd.trk[i].songname)) c="";
      fprintf(f, "TTITLE%d=%s\n", i, c);
    }
    /* Currently unused, but xmcd may need it */
    fputs("EXTD=\n", f);
    for (i = 0; i < thiscd.ntracks; i++) fprintf(f, "EXTT%d=\n", i);
    fputs("PLAYORDER=", f);
    it=ORDER(Gad[ORD_SONGS])->first;
    while(it && (it->id!=DIG_RIGHT_ARROW)) it=it->next;
    if (it) it=it->next;
    while(it && (it->id!=DIG_LEFT_ARROW)) {
      fprintf(f, "%d", it->id);
      if ((it=it->next) && (it->id<=thiscd.ntracks)) fputc(',',f);
    }
    fputs("\n", f);

    fclose(f);
  } else {
    McError(app, _("Failed to write to file '%s'\nReason: %s\n"),
	    file, strerror(errno));
  }
}

