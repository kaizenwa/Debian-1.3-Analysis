/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: 
   Path: 
   Description: 
   Created: Sun Aug  7 16:56:10 MET 1994
   Author: Lionel Fournigault
   Modified: Sun Aug  7 16:56:11 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   Requires: 

   Defines: 

   Suggested bindings: 

   Procedure: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */

{
  if (! function("rcs_check_in"))
    load_file("rcs.sc");
}

void cv_check_in ()
{
  rcs_check_in();
}

void cv_check_out_locked ()
{
  rcs_check_out_locked();
}

void cv_check_out_unlocked ()
{
  rcs_check_out_unlocked();
}

void cv_check_in_and_out_locked ()
{
  rcs_check_in_and_out_locked();
}

void cv_check_in_and_out_unlocked ()
{
  rcs_check_in_and_out_unlocked();
}

void cv_diff ()
{
  rcs_diff();
}


void cv_log ()
{
  rcs_log();
}

void cv_repository ()
{
  rcs_repository();
}

void cv_lock_revision()
{
    rcs_lock_revision();
}

void cv_unlock_revision()
{
    rcs_unlock_revision();
}

void cv_initialize()
{
    rcs_initialize();
}
