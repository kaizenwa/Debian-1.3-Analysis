------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--              I N T E R F A C E S . C . P O S I X _ E R R O R             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.2 $                             --
--                                                                          --
--   Copyright (C) 1991,1992,1993,1994,1995,1996 Florida State University   --
--                                                                          --
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

--  This is the Linux version of this package.

--  This package contains those parts of the package POSIX defined in P1003.5
--  (Ada binding to POSIX.1) needed to interface to Pthreads.

with Interfaces.C.System_Constants;
--  Used for: various constants

package Interfaces.C.POSIX_Error is

   package ICSC renames Interfaces.C.System_Constants;

   type Return_Code is new int;

   Success : constant Return_Code := 0;
   Failure : constant Return_Code := -1;

   type Error_Code is new int;

   subtype EC is Error_Code;
   --  Synonym used only in this package

   function Get_Error_Code return Error_Code;
   pragma Import (C, Get_Error_Code, "__get_errno");
   --  An interface to the error number of the current thread.  This is updated
   --  by Pthreads at each context switch.

   --  Error number definitions.  These definitions are derived from
   --  /usr/include/errno.h and /usr/include/sys/errno.h. These are SunOS
   --  errors; they have not yet been checked fsor POSIX complience.

   Operation_Not_Permitted            : constant EC := ICSC.EPERM;
   No_Such_File_Or_Directory          : constant EC := ICSC.ENOENT;
   No_Such_Process                    : constant EC := ICSC.ESRCH;
   Interrupted_Operation              : constant EC := ICSC.EINTR;
   Input_Output_Error                 : constant EC := ICSC.EIO;
   No_Such_Device_Or_Address          : constant EC := ICSC.ENXIO;
   Argument_List_Too_Long             : constant EC := ICSC.E2BIG;
   Exec_Format_Error                  : constant EC := ICSC.ENOEXEC;
   Bad_File_Descriptor                : constant EC := ICSC.EBADF;
   No_Child_Process                   : constant EC := ICSC.ECHILD;
   Resource_Temporarily_Unavailable   : constant EC := ICSC.EAGAIN;
   Not_Enough_Space                   : constant EC := ICSC.ENOMEM;
   Permission_Denied                  : constant EC := ICSC.EACCES;
   Resource_Busy                      : constant EC := ICSC.EFAULT;
   File_Exists                        : constant EC := ICSC.ENOTBLK;
   Improper_Link                      : constant EC := ICSC.EBUSY;
   No_Such_Operation_On_Device        : constant EC := ICSC.EEXIST;
   Not_A_Directory                    : constant EC := ICSC.EXDEV;
   Is_A_Directory                     : constant EC := ICSC.ENODEV;
   Invalid_Argument                   : constant EC := ICSC.ENOTDIR;
   Too_Many_Open_Files_In_System      : constant EC := ICSC.EISDIR;
   Too_Many_Open_Files                : constant EC := ICSC.EINVAL;
   Priority_Ceiling_Violation         : constant EC := ICSC.EINVAL;
   Inappropriate_IO_Control_Operation : constant EC := ICSC.ENFILE;
   File_Too_Large                     : constant EC := ICSC.EMFILE;
   No_Space_Left_On_Device            : constant EC := ICSC.ENOTTY;
   Invalid_Seek                       : constant EC := ICSC.ETXTBSY;
   Read_Only_File_System              : constant EC := ICSC.EFBIG;
   Too_Many_Links                     : constant EC := ICSC.ENOSPC;
   Broken_Pipe                        : constant EC := ICSC.ESPIPE;
   Operation_Not_Implemented          : constant EC := ICSC.ENOSYS;
   Operation_Not_Supported            : constant EC := ICSC.ENOTSUP;
   Timer_Expired                      : constant EC := ICSC.ETIMEDOUT;

end Interfaces.C.POSIX_Error;
