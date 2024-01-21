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

#ifndef RGLOB_H
#define RGLOB_H

#include "SMTask.h"
#include "ftpclass.h"

class RemoteGlob : public SMTask
{
   char	 **list;
   int	 list_size;
   int	 list_alloc;

   Ftp::open_mode mode;

   static const bufsize=0x1000;
   char	 buf[bufsize];
   int	 inbuf;

   Ftp	 *f;

   int	 flags;
   enum { ALL_DONE=1,RESTRICT_SLASHES=2 };
   int	 extra_slashes;

public:
   int	 Do();

   char	 *pattern;

   RemoteGlob(Ftp *session,char *n_pattern,Ftp::open_mode n_mode);
   virtual ~RemoteGlob();

   void	 SetSlashFilter(int num)
   {
      extra_slashes=num;
      flags|=RESTRICT_SLASHES;
   }

   bool	 Done() { return flags&ALL_DONE; }

   char **GetResult() { return((flags&ALL_DONE)?list:0); }
};

#endif
