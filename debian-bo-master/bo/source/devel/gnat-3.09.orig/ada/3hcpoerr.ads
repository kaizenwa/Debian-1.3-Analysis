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

--  This is a HP_UX 9.05 version of this package.

--  This package contains those parts of the package POSIX defined in P1003.5
--  (Ada binding to POSIX.1) needed to interface to Pthreads.

with Interfaces.C.System_Constants;
--  Used for: various constants

package Interfaces.C.POSIX_Error is

   package ICSC renames Interfaces.C.System_Constants;

   type Return_Code is new int;

   Failure : constant Return_Code := -1;

   Success : constant Return_Code := 0;

   type Error_Code is new int;

   subtype EC is Error_Code;
   --  Synonym used only in this package

   function Get_Error_Code return Error_Code;
   pragma Import (C, Get_Error_Code, "__get_errno");
   --  An interface to the error number of the current thread.  This is updated
   --  by Pthreads at each context switch.

   --  Error number definitions.  These definitions are derived from
   --  POSIX .1 Standard.

   Arg_List_Too_Long                  : constant EC := ICSC.E2BIG;
   Permission_Denied                  : constant EC := ICSC.EACCES;
   Resource_Temporarily_Unavailable   : constant EC := ICSC.EAGAIN;
   Bad_File_Descriptor                : constant EC := ICSC.EBADF;
   Resource_Busy                      : constant EC := ICSC.EBUSY;
   No_Child_Processes                 : constant EC := ICSC.ECHILD;
   Resource_Deadlock_Avoided          : constant EC := ICSC.EDEADLK;
   Domain_Error                       : constant EC := ICSC.EDOM;
   File_Exists                        : constant EC := ICSC.EEXIST;
   Bad_Address                        : constant EC := ICSC.EFAULT;
   File_Too_Large                     : constant EC := ICSC.EFBIG;
   Interrupted_Function_Call          : constant EC := ICSC.EINTR;
   Invalid_Argument                   : constant EC := ICSC.EINVAL;
   Priority_Ceiling_Violation         : constant EC := ICSC.EINVAL;
   Input_Output_Error                 : constant EC := ICSC.EIO;
   Is_A_Directory                     : constant EC := ICSC.EISDIR;
   Too_Many_Open_Files                : constant EC := ICSC.EMFILE;
   Too_Many_Links                     : constant EC := ICSC.EMLINK;
   Filename_Too_Long                  : constant EC := ICSC.ENAMETOOLONG;
   Too_Many_Open_Files_In_System      : constant EC := ICSC.ENFILE;
   No_Such_Device                     : constant EC := ICSC.ENODEV;
   No_Such_File_Or_Directory          : constant EC := ICSC.ENOENT;
   Exec_Format_Error                  : constant EC := ICSC.ENOEXEC;
   No_Locks_Available                 : constant EC := ICSC.ENOLCK;
   Not_Enough_Space                   : constant EC := ICSC.ENOMEM;
   No_Space_Left_On_Device            : constant EC := ICSC.ENOSPC;
   Function_Not_Implemented           : constant EC := ICSC.ENOSYS;
   Not_A_Directory                    : constant EC := ICSC.ENOTDIR;
   Directory_Not_Empty                : constant EC := ICSC.ENOTEMPTY;
   Inappropriate_IO_Control_Operation : constant EC := ICSC.ENOTTY;
   No_Such_Device_Or_Address          : constant EC := ICSC.ENXIO;
   Operation_Not_Permitted            : constant EC := ICSC.EPERM;
   Broken_Pipe                        : constant EC := ICSC.EPIPE;
   Result_Too_Large                   : constant EC := ICSC.ERANGE;
   Read_Only_File_Sysyem              : constant EC := ICSC.EROFS;
   Invalid_Seek                       : constant EC := ICSC.ESPIPE;
   No_Such_Process                    : constant EC := ICSC.ESRCH;
   Improper_Link                      : constant EC := ICSC.EXDEV;

   --  Error number definitions.  These definitions are need for
   --  POSIX .4 Operations.

   Timer_Expired                      : constant EC := ICSC.ETIME;
   Connection_Timed_Out               : constant EC := ICSC.ETIMEDOUT;

end Interfaces.C.POSIX_Error;
