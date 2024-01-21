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

#ifndef SITEJOB_H
#define SITEJOB_H

#include "Job.h"
#include "Filter.h"
#include "xmalloc.h"

class SiteJob : public FtpJob
{
   FDStream *output;
   char *site_cmd;
   char	 buf[0x400];
   int	 in_buf;
public:
   SiteJob(Ftp *f,char *cmd,FDStream *o) : FtpJob(f)
   {
      site_cmd=cmd;
      output=o;
      session->Open(site_cmd,Ftp::SITE_CMD);
      in_buf=0;
   }
   ~SiteJob()
   {
      if(output)
	 delete output;
      if(site_cmd)
	 free(site_cmd);
   }
   int	 Do();
   int	 Done();
};

#endif  SITEJOB_H
