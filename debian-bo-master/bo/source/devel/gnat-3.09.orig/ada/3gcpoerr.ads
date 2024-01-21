------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--              I N T E R F A C E S . C . P O S I X _ E R R O R             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.4 $                             --
--                                                                          --
--      Copyright (C) 1991,1992,1993,1994,1995 Florida State University     --
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

--  This is a SGI IRIX version of this package.

--  This package contains those parts of the package POSIX defined in P1003.5
--  (Ada binding to POSIX.1) needed to interface to Pthreads.

with Interfaces.C.System_Constants;
--  Used for: various constants

package Interfaces.C.POSIX_Error is

   package SC renames Interfaces.C.System_Constants;

   type Return_Code is new int;

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

   --  Error number definitions.

   Operation_Not_Permitted            : constant EC := SC.EPERM;
   No_Such_File_Or_Directory          : constant EC := SC.ENOENT;
   No_Such_Process                    : constant EC := SC.ESRCH;
   Interrupted_Operation              : constant EC := SC.EINTR;
   Input_Output_Error                 : constant EC := SC.EIO;
   No_Such_Device_Or_Address          : constant EC := SC.ENXIO;
   Argument_List_Too_Long             : constant EC := SC.E2BIG;
   Exec_Format_Error                  : constant EC := SC.ENOEXEC;
   Bad_File_Descriptor                : constant EC := SC.EBADF;
   No_Child_Process                   : constant EC := SC.ECHILD;
   Resource_Temporarily_Unavailable   : constant EC := SC.EAGAIN;
   Not_Enough_Space                   : constant EC := SC.ENOMEM;
   Permission_Denied                  : constant EC := SC.EACCES;
   Resource_Busy                      : constant EC := SC.EFAULT;
   File_Exists                        : constant EC := SC.ENOTBLK;
   Improper_Link                      : constant EC := SC.EBUSY;
   No_Such_Operation_On_Device        : constant EC := SC.EEXIST;
   Not_A_Directory                    : constant EC := SC.EXDEV;
   Is_A_Directory                     : constant EC := SC.ENODEV;
   Invalid_Argument                   : constant EC := SC.ENOTDIR;
   Too_Many_Open_Files_In_System      : constant EC := SC.EISDIR;
   Too_Many_Open_Files                : constant EC := SC.EINVAL;
   Priority_Ceiling_Violation         : constant EC := SC.EINVAL;
   Inappropriate_IO_Control_Operation : constant EC := SC.ENFILE;
   File_Too_Large                     : constant EC := SC.EMFILE;
   No_Space_Left_On_Device            : constant EC := SC.ENOTTY;
   Invalid_Seek                       : constant EC := SC.ETXTBSY;
   Read_Only_File_System              : constant EC := SC.EFBIG;
   Too_Many_Links                     : constant EC := SC.ENOSPC;
   Broken_Pipe                        : constant EC := SC.ESPIPE;
   Operation_Not_Implemented          : constant EC := SC.ENOSYS;
   Operation_Not_Supported            : constant EC := SC.ENOTSUP;

end Interfaces.C.POSIX_Error;
