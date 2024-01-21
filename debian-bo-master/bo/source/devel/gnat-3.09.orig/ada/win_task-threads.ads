------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     W I N _ T A S K . T H R E A D S                      --
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

with Interfaces.C;

package Win_Task.Threads is
pragma Preelaborate (Threads);

--   use Interfaces.C;

   -------------------------------------------------------------
   -- Thread Creation, Activation, Suspension And Termination --
   -------------------------------------------------------------

   type PTHREAD_START_ROUTINE is access function
      (pThreadParameter : PVOID) return DWORD;
   pragma Convention (Stdcall, PTHREAD_START_ROUTINE);

   type SECURITY_ATTRIBUTES is
      record
         nLength              : DWORD;
         pSecurityDescriptor  : PVOID;
         bInheritHandle       : BOOL;
      end record;
   type PSECURITY_ATTRIBUTES is access all SECURITY_ATTRIBUTES;

   function CreateThread (
      pThreadAttributes    : PSECURITY_ATTRIBUTES;
      dwStackSize          : DWORD;
      pStartAddress        : PTHREAD_START_ROUTINE;
      pParameter           : PVOID;
      dwCreationFlags      : DWORD;
      pThreadId            : PDWORD) return HANDLE;
   pragma Import (Stdcall, CreateThread, "CreateThread");

   function BeginThreadEx (
      pThreadAttributes    : PSECURITY_ATTRIBUTES;
      dwStackSize          : DWORD;
      pStartAddress        : PTHREAD_START_ROUTINE;
      pParameter           : PVOID;
      dwCreationFlags      : DWORD;
      pThreadId            : PDWORD) return HANDLE;
   pragma Import (C, BeginThreadEx, "_beginthreadex");


   Debug_Process              : constant := 16#00000001#;
   Debug_Only_This_Process    : constant := 16#00000002#;
   Create_Suspended           : constant := 16#00000004#;
   Detached_Process           : constant := 16#00000008#;
   Create_New_Console         : constant := 16#00000010#;

   Create_New_Process_Group   : constant := 16#00000200#;

   Create_No_window           : constant := 16#08000000#;

   Profile_User               : constant := 16#10000000#;
   Profile_Kernel             : constant := 16#20000000#;
   Profile_Server             : constant := 16#40000000#;

   function GetExitCodeThread (
      hThread     : HANDLE;
      pExitCode   : PDWORD) return BOOL;
   pragma Import (Stdcall, GetExitCodeThread, "GetExitCodeThread");

   function ResumeThread (hThread : HANDLE) return DWORD;
   pragma Import (Stdcall, ResumeThread, "ResumeThread");

   function SuspendThread (hThread : HANDLE) return DWORD;
   pragma Import (Stdcall, SuspendThread, "SuspendThread");

   procedure ExitThread (dwExitCode : DWORD);
   pragma Import (Stdcall, ExitThread, "ExitThread");

   procedure EndThreadEx (dwExitCode : DWORD);
   pragma Import (C, EndThreadEx, "_endthreadex");

   function TerminateThread (
      hThread     : HANDLE;
      dwExitCode  : DWORD) return BOOL;
   pragma Import (Stdcall, TerminateThread, "TerminateThread");

   function GetCurrentThread return HANDLE;
   pragma Import (Stdcall, GetCurrentThread, "GetCurrentThread");

   function GetCurrentThreadId return DWORD;
   pragma Import (Stdcall, GetCurrentThreadId, "GetCurrentThreadId");

   function TlsAlloc return DWORD;
   pragma Import (Stdcall, TlsAlloc, "TlsAlloc");

   function TlsGetValue (dwTlsIndex : DWORD) return PVOID;
   pragma Import (Stdcall, TlsGetValue, "TlsGetValue");

   function TlsSetValue (dwTlsIndex : DWORD; pTlsValue : PVOID) return BOOL;
   pragma Import (Stdcall, TlsSetValue, "TlsSetValue");

   function TlsFree (dwTlsIndex : DWORD) return BOOL;
   pragma Import (Stdcall, TlsFree, "TlsFree");

   TLS_Nothing    : constant := DWORD'Last;

   procedure ExitProcess (uExitCode : Interfaces.C.unsigned);
   pragma Import (Stdcall, ExitProcess, "ExitProcess");

   function WaitForSingleObject
      (hHandle          : HANDLE;
       dwMilliseconds   : DWORD) return DWORD;
   pragma Import (Stdcall, WaitForSingleObject, "WaitForSingleObject");

   function WaitForSingleObjectEx
      (hHandle          : HANDLE;
       dwMilliseconds   : DWORD;
       fAlertable       : BOOL) return DWORD;
   pragma Import (Stdcall, WaitForSingleObjectEx, "WaitForSingleObjectEx");

   Wait_Infinite : constant := DWORD'Last;
   WAIT_TIMEOUT  : constant := DWORD'Last;

   ---------------------------------------------------
   -- Accessing properties of Threads and Processes --
   ---------------------------------------------------

   -----------------
   --  Priorities --
   -----------------

   function SetThreadPriority
      (hThread     : HANDLE;
       nPriority   : Interfaces.C.int) return BOOL;
   pragma Import (Stdcall, SetThreadPriority, "SetThreadPriority");

   function GetThreadPriority (hThread  : HANDLE) return Interfaces.C.int;
   pragma Import (Stdcall, GetThreadPriority, "GetThreadPriority");
 
   Normal_Priority_Class   : constant := 16#00000020#;
   Idle_Priority_Class     : constant := 16#00000040#;
   High_Priority_Class     : constant := 16#00000080#;
   Realtime_Priority_Class : constant := 16#00000100#;

   Thread_Priority_Idle            : constant := -15;
   Thread_Priority_Lowest          : constant := -2;
   Thread_Priority_Below_Normal    : constant := -1;
   Thread_Priority_Normal          : constant := 0;
   Thread_Priority_Above_Normal    : constant := 1;
   Thread_Priority_Highest         : constant := 2;
   Thread_Priority_Time_Critical   : constant := 15;
   Thread_Priority_Error_Return    : constant := Interfaces.C.long'Last;

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

end Win_Task.Threads;
