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

#ifndef MIRRORJOB_H
#define MIRRORJOB_H

#include "mirror.h"
#include "rglob.h"
#include "Job.h"
#include "XferJob.h"

/*
 * Sequentially do the following steps:
 * 1. retrieve long listing of current remote directory
 * 2. make FileSet of it
 * 2.1. get short list and merge thes lists.
 * 2.2. get date for those files which don't have prec date
 * 2.3. get size ^^^
 * 3. make FileSet of current local dir
 * 4. `subtract' them, thus getting list of newer/different remote files
 *    and list of local files to be deleted.
 * 5. download these files
 * 6. make local directories/links, delete files.
 * 7. for each directory, start recursive mirror job
 * 8. wait for them to finish (or wait for each after starting)
 * 9. Done.
 */

class MirrorJob : public FtpJob
{
   enum state_t
   {
      CHANGING_REMOTE_DIR,
      GETTING_LONG_LIST,
      GETTING_SHORT_LIST,
      GETTING_INFO,
      WAITING_FOR_SUBGET,
      WAITING_FOR_SUBMIRROR,
      DONE
   };
   state_t state;

   FileSet *to_get;
   FileSet *same;
   FileSet *to_rm;

   FileInfo *file;
   void	 HandleFile(int);

   Ftp::fileinfo  *get_info;
   int	 cnt;

   RemoteGlob *glob;

   char	 *local_dir;
   char	 *remote_dir;

   int	 tot_files,new_files,mod_files,del_files;
   int	 dirs,del_dirs;
   int	 tot_symlinks,new_symlinks,mod_symlinks,del_symlinks;

public:
   MirrorJob(Ftp *f,const char *new_local_dir,const char *new_remote_dir);
   ~MirrorJob();

   int	 Do();
   int	 Done() { return state==DONE; }
   void	 ShowRunStatus(StatusLine *);
   void	 PrintStatus(int v);
   void	 SayFinal() { PrintStatus(-1); }
};

#endif  MIRRORJOB_H
