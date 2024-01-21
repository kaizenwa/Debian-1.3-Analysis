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

#ifndef CHARREADER_H
#define CHARREADER_H

#include "SMTask.h"

class CharReader : public SMTask
{
   int	 fd;
   int	 ch;

   int	 Do();

public:
   static const NOCHAR=-2;
   static const EOFCHAR=-1;

   int	 GetChar() { return ch; };

   CharReader(int new_fd)
   {
      fd=new_fd;
      ch=NOCHAR;
   }
   ~CharReader() {}
};

#endif  CHARREADER_H
