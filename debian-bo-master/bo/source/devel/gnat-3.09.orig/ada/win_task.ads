------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                              W I N _ T A S K                             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--          Copyright (c) 1993,1994,1995 NYU, All Rights Reserved           --
--                                                                          --
--  GNARL is free software; you can redistribute it and/or modify it  under --
--  terms  of  the  GNU  Library General Public License as published by the --
--  Free Software Foundation; either version 2,  or (at  your  option)  any --
--  later  version.   GNARL is distributed in the hope that it will be use- --
--  ful, but but WITHOUT ANY WARRANTY; without even the implied warranty of --
--  MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
--  eral Library Public License for more details.  You should have received --
--  a  copy of the GNU Library General Public License along with GNARL; see --
--  file COPYING. If not, write to the Free Software Foundation,  675  Mass --
--  Ave, Cambridge, MA 02139, USA.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package (and children) provide interface definitions to the standard
--  Windows NT Library. They are merely a translation of the various <win*.h>
--  files.

--  It is intended that higher level interfaces (with better names, and
--  stronger typing!) be built on top of this one for Ada (i.e. clean)
--  programming.

with System;
with Interfaces.C;
with Interfaces.C.Strings;

package Win_Task is
pragma Preelaborate (Win_Task);
pragma Linker_Options ("-lkernel32");

--   use Interfaces.C;
--   use Interfaces.C.Strings;

   -------------------
   -- General Types --
   -------------------

   type    DWORD    is new Interfaces.C.unsigned_long;
   type    WORD     is new Interfaces.C.unsigned_short;

   subtype PSZ   is Interfaces.C.Strings.chars_ptr;
   subtype PCHAR is Interfaces.C.Strings.chars_ptr;
   subtype PVOID is System.Address;
   Null_Void   : constant PVOID := System.Null_Address;

   type PLONG  is access all Interfaces.C.long;
   type PDWORD is access all DWORD;

   type BOOL is new boolean;
   for BOOL'Size use Interfaces.C.unsigned_long'Size;

   -------------------------
   -- Handles for objects --
   -------------------------

   type HANDLE is new Interfaces.C.long;
   type PHANDLE is access all HANDLE;

   ---------------------
   -- Time Management --
   ---------------------

   procedure Sleep (dwMilliseconds : DWORD);
   pragma Import (Stdcall, Sleep, External_Name => "Sleep");

   type SYSTEMTIME is
      record
         wYear          : WORD;
         wMonth         : WORD;
         wDayOfWeek     : WORD;
         wDay           : WORD;
         wHour          : WORD;
         wMinute        : WORD;
         wSecond        : WORD;
         wMilliseconds  : WORD;
      end record;
   type PSYSTEMTIME is access all SYSTEMTIME;

   procedure GetSystemTime (pSystemTime : Win_Task.PSYSTEMTIME);
   pragma Import (Stdcall, GetSystemTime, "GetSystemTime");

   function SetSystemTime (pSystemTime : Win_Task.PSYSTEMTIME) return BOOL;
   pragma Import (Stdcall, SetSystemTime, "SetSystemTime");

   ----------------------------
   -- Miscelleneous Features --
   ----------------------------

   --  Features which do not fit any child

   function Beep (dwFreq : DWORD; dwDuration : DWORD) return BOOL;
   pragma Import (Stdcall, Beep, External_Name => "Beep");

   procedure Must_Not_Fail (Return_Code : Win_Task.DWORD);
   --  Many Windows NT functions return DWORD and are not supposed to fail.
   --  In C style, these would be called as procedures, disregarding the
   --  returned value. This procedure can be used to achieve the same effect
   --  with a call of the form: Must_Not_Fail (Some_Function (...));

   procedure Must_Not_Fail_Bool (Return_Code : BOOL);

end Win_Task;
