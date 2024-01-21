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

#ifndef SIGNALHOOK_H
#define SIGNALHOOK_H

#include <signal.h>

class SignalHook
{
   static int counts[256];
   static void cnt_handler(int sig);
public:
   static void DoCount(int sig) { signal(sig,cnt_handler); }
   static int GetCount(int sig) { return counts[sig]; }
   static void ResetCount(int sig) { counts[sig]=0; }
   static void AddHook(int sig,void (*)(int));
};

#endif  SIGNALHOOK_H
