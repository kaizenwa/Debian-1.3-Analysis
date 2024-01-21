------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . P R O G R A M  _  I N F O                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
--                                                                          --
--               Copyright (C) 1996 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------
--  This package   contains the parameters  used by   the run-time system at
--  program startup.  These parameters are  isolated in this package body to
--  facilitate replacement by the end user.
--
--  To repalce the default values, copy this source file into your build
--  directory, edit the file to reflect your desired behavior, and recompile
--  with the command:
--
--     % gcc -c -O2 -gnatg s-proinf.adb
--
--  then relink your application as usual.
--

with Interfaces.C.Strings;
package body System.Program_Info is

   Kbytes : constant := 1024;

   Default_Initial_Sproc_Count  : constant := 0;
   Default_Max_Sproc_Count      : constant := 128;
   Default_Sproc_Stack_Size     : constant := 16#4000#;
   Default_Stack_Guard_Pages    : constant := 1;
   Default_Default_Time_Slice   : constant := 0.0;
   Default_Default_Task_Stack   : constant := 12 * Kbytes;
   Default_Pthread_Sched_Signal : constant := 33;
   Default_Pthread_Arena_Size   : constant := 16#40000#;
   Default_Os_Default_Priority  : constant := 0;

   use Interfaces.C.Strings;

   function Getenv (Name : String) return String;

   function Getenv (Name : String) return String is

      function C_Getenv (P1 : chars_ptr) return chars_ptr;
      pragma Import (C, C_Getenv, "getenv", "getenv");

      Result : chars_ptr;
      C_P1 : chars_ptr := New_String (Name);

   begin
      Result := C_Getenv (C_P1);
      Free (C_P1);
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Getenv;

   function Initial_Sproc_Count return Integer is

      function sysmp (P1 : Integer) return Integer;
      pragma Import (C, sysmp, "sysmp", "sysmp");

      MP_NPROCS      : constant := 1; --   # processor in complex

      PTHREAD_SPROC_COUNT_STR : constant String
        := Getenv ("PTHREAD_SPROC_COUNT");
   begin
      if PTHREAD_SPROC_COUNT_STR = "" then
         return Default_Initial_Sproc_Count;
      elsif PTHREAD_SPROC_COUNT_STR'Length >= 4 and then
         PTHREAD_SPROC_COUNT_STR (1 .. 4) = "AUTO" then
         return sysmp (MP_NPROCS);
      else
         return Integer'Value (PTHREAD_SPROC_COUNT_STR);
      end if;
   exception
      when others => return 0;
   end Initial_Sproc_Count;


   function Max_Sproc_Count     return Integer is
      PTHREAD_MAX_SPROC_COUNT_STR : constant String
        := Getenv ("PTHREAD_MAX_SPROC_COUNT");
   begin
      if PTHREAD_MAX_SPROC_COUNT_STR = "" then
         return Default_Max_Sproc_Count;
      else
         return Integer'Value (PTHREAD_MAX_SPROC_COUNT_STR);
      end if;
   exception
      when others =>
         return Integer'Value (PTHREAD_MAX_SPROC_COUNT_STR);
   end Max_Sproc_Count;


   function Sproc_Stack_Size return Integer is
   begin
      return Default_Sproc_Stack_Size;
   end Sproc_Stack_Size;

   function Default_Time_Slice  return Duration is
      PTHREAD_TIME_SLICE_USEC_STR : constant String
        := Getenv ("PTHREAD_TIME_SLICE_USEC");
      PTHREAD_TIME_SLICE_SEC_STR : constant String
        := Getenv ("PTHREAD_TIME_SLICE_SEC");
      Time_Slice : Duration := 0.0;
   begin
      if PTHREAD_TIME_SLICE_USEC_STR /= "" or
        PTHREAD_TIME_SLICE_SEC_STR /= "" then

         if PTHREAD_TIME_SLICE_SEC_STR /= "" then
            Time_Slice := Time_Slice +
              Duration (Integer'Value (PTHREAD_TIME_SLICE_SEC_STR));
         end if;

         if PTHREAD_TIME_SLICE_USEC_STR /= "" then
            Time_Slice := Time_Slice +
              Duration (Integer'Value (PTHREAD_TIME_SLICE_SEC_STR)) / 1000.0;
         end if;

         return Time_Slice;
      else
         return Default_Default_Time_Slice;
      end if;
   exception
      when others =>
         return Default_Default_Time_Slice;
   end Default_Time_Slice;

   function Default_Task_Stack  return Integer is
   begin
      return Default_Default_Task_Stack;
   end Default_Task_Stack;

   function Stack_Guard_Pages   return Integer is
      PTHREAD_STACK_GUARD_PAGES_STR : constant String
        := Getenv ("PTHREAD_STACK_GUARD_PAGES");
   begin
      if PTHREAD_STACK_GUARD_PAGES_STR /= "" then
         return Integer'Value (PTHREAD_STACK_GUARD_PAGES_STR);
      else
         return Default_Stack_Guard_Pages;
      end if;
   exception
      when others =>
         return Default_Stack_Guard_Pages;
   end Stack_Guard_Pages;

   function Pthread_Sched_Signal return Integer is
   begin
      return Default_Pthread_Sched_Signal;
   end Pthread_Sched_Signal;

   function Pthread_Arena_Size  return Integer is
      PTHREAD_ARENA_SIZE_STR : constant String
        := Getenv ("PTHREAD_ARENA_SIZE");
   begin
      if PTHREAD_ARENA_SIZE_STR = "" then
         return Default_Pthread_Arena_Size;
      else
         return Integer'Value (PTHREAD_ARENA_SIZE_STR);
      end if;
   exception
      when others =>
         return Default_Pthread_Arena_Size;
   end Pthread_Arena_Size;

   function Os_Default_Priority return Integer is
   begin
      return Default_Os_Default_Priority;
   end Os_Default_Priority;


end System.Program_Info;
