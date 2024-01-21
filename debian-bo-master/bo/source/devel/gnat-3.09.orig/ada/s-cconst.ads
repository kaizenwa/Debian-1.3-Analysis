-----------------------------------------------------------------------------
--                                                                         --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                         --
--                         C _ C o n s t a n t s                           --
--                                                                         --
--                                 S p e c                                 --
--                                                                         --
--                            $Revision: 1.2 $                            --
--                                                                         --
--           Copyright (C) 1991,1992,1993 Florida State University          --
--                                                                         --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

package System.C_Constants is

pthread_t_size : constant Integer := 4;
pthread_attr_t_size : constant Integer := 28;
pthread_mutexattr_t_size : constant Integer := 12;
pthread_mutex_t_size : constant Integer := 32;
pthread_condattr_t_size : constant Integer := 4;
pthread_cond_t_size : constant Integer := 20;
pthread_key_t_size : constant Integer := 4;
pthread_jmp_buf_size : constant Integer := 16;
pthread_sigjmp_buf_size : constant Integer := 16;
Add_Prio : constant Integer := 2;

end System.C_Constants;
