/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef MIRROR_H
#define MIRROR_H

#include <sys/types.h>
#include "xmalloc.h"

#ifdef TYPE
#error
#endif

class FileInfo
{
public:
   char	    *name;
   mode_t   mode;
   time_t   date;
   long	    size;

   enum	 type
   {
      DIRECTORY,
      SYMLINK,
      NORMAL
   };

   type	 filetype;
   char	 *symlink;

   int	 defined;
   enum defined_bits
   {
      NAME=001,MODE=002,DATE=004,TYPE=010,SYMLINK_DEF=020,
      DATE_UNPREC=040,SIZE=0100
   };

   ~FileInfo()
   {
      xfree(name);
      xfree(symlink);
   }

   FileInfo()
   {
      name=NULL;
      defined=0;
      symlink=NULL;
   }
   FileInfo(const FileInfo &fi)
   {
      name=xstrdup(fi.name);
      symlink=xstrdup(fi.symlink);
      defined=fi.defined;
      filetype=fi.filetype;
      mode=fi.mode;
      date=fi.date;
      size=fi.size;
   }

   void SetName(const char *n) { xfree(name); name=xstrdup(n); defined|=NAME; }
   void SetMode(mode_t m) { mode=m; defined|=MODE; }
   void SetDate(time_t t) { date=t; defined|=DATE; defined&=~DATE_UNPREC; }
   void SetDateUnprec(time_t t) { if(defined&DATE) return;
      date=t; defined|=DATE_UNPREC; }
   void SetType(type t) { filetype=t; defined|=TYPE; }
   void SetSymlink(const char *s) { xfree(symlink); symlink=xstrdup(s);
      filetype=SYMLINK; defined|=TYPE|SYMLINK_DEF; }
   void	SetSize(long s) { size=s; defined|=SIZE; }

   void	 Merge(const FileInfo&);

   bool	 SameAs(const FileInfo *);
};

class FileSet
{
   FileInfo **files;
   int	 fnum;

   int	 ind;

   void Sub(int i)
   {
      if(i>=fnum)
	 abort();
      delete files[i];
      memmove(files+i,files+i+1,(--fnum-i)*sizeof(*files));
      if(ind>i)
	 ind--;
   }

public:
   FileSet()
   {
      files=0;
      fnum=0;
      ind=0;
   }
   FileSet(const FileSet *s);
   ~FileSet();

   int	 get_fnum() { return fnum; }

   void	 Add(FileInfo *);
   void	 Merge(const FileSet *);
   void	 SubtractSame(const FileSet *);
   void	 SubtractAny(const FileSet *);
   void	 Merge(char **);   // file list

   void	 rewind() { ind=0; }
   FileInfo *curr() { return (ind<fnum ? files[ind] : 0); }
   FileInfo *next() { return (ind<fnum&&++ind<fnum ? files[ind] : 0); }

   void	 LocalRemove(const char *dir);
   void	 LocalUtime(const char *dir);
   void	 LocalChmod(const char *dir);

   void Count(int *d,int *f,int *s,int *o);

   void  SetSize(char *name,long size);
   void  SetDate(char *name,time_t date);
};

FileSet *parse_listing(char **lines);
FileSet *local_files(char *dir);

char *dir_file(const char *dir,const char *file);

#endif MIRROR_H
