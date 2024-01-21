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

#ifndef FILTER_H
#define FILTER_H

#include "ProcWait.h"

class FDStream
{
public:
   int fd;
   char *name;
   char *error_text;

   bool	 error() { return error_text!=0; }

   virtual int getfd() { return fd; }

   FDStream();
   FDStream(int new_fd,const char *new_name);
   virtual ~FDStream();

   void MakeErrorText();

   virtual long getsize_and_seek_end();
   virtual void setmtime(time_t) {}
   virtual void remove_if_empty() {}
   virtual bool Done() { return 1; }
   virtual bool usesfd(int fd) { return this->fd==fd; }
};

class OutputFilter : public FDStream
{
   ProcWait *w;
   FDStream *second;
   int second_fd;
   char *oldcwd;

   void Init();

public:
   OutputFilter(const char *filter,int second_fd=-1);
   OutputFilter(const char *filter,FDStream *second);
   ~OutputFilter();

   long getsize_and_seek_end() { return 0; }

   int getfd();
   bool Done();

   bool usesfd(int fd) { return FDStream::usesfd(fd) || fd<=2; }
};

class FileStream : public FDStream
{
   int mode;
public:
   char *full_name;
   FileStream(const char *fname,int open_mode);
   ~FileStream();

   void setmtime(time_t t);
   void remove_if_empty();
   int getfd();
};

#endif /* FILTER_H */
