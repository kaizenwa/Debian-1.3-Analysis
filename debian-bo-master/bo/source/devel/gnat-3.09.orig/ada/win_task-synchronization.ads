------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             W I N _ T A S K . S Y N C H R O N I Z A T I O N              --
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
with System;
with Win_Task.Threads;

package Win_Task.Synchronization is
pragma Preelaborate (Synchronization);

--   use Interfaces.C;
--   use System;
--   use Win_Task.Threads;

   --  Critical sections

   type LIST_ENTRY is private;
   type PLIST_ENTRY is access all LIST_ENTRY;

   type CRITICAL_SECTION is private;
   type PCRITICAL_SECTION is access all CRITICAL_SECTION;

   procedure InitializeCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import
      (Stdcall, InitializeCriticalSection, "InitializeCriticalSection");

   procedure EnterCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import (Stdcall, EnterCriticalSection, "EnterCriticalSection");

   procedure LeaveCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import (Stdcall, LeaveCriticalSection, "LeaveCriticalSection");

   procedure DeleteCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import (Stdcall, DeleteCriticalSection, "DeleteCriticalSection");

   --  Semaphores, Events and Mutexs

   function CloseHandle (
      hObject           : HANDLE) return BOOL;
   pragma Import (Stdcall, CloseHandle, "CloseHandle");

   function CreateSemaphore (
      pSemaphoreAttributes    : Win_Task.Threads.PSECURITY_ATTRIBUTES;
      lInitialCount           : Interfaces.C.long;
      lMaximumCount           : Interfaces.C.long;
      pName                   : PSZ) return HANDLE;
   pragma Import (Stdcall, CreateSemaphore, "CreateSemaphoreA");

   function OpenSemaphore (
      dwDesiredAccess   : DWORD;
      bInheritHandle    : BOOL;
      pName             : PSZ) return HANDLE;
   pragma Import (Stdcall, OpenSemaphore, "OpenSemaphoreA");

   function ReleaseSemaphore (
      hSemaphore        : HANDLE;
      lReleaseCount     : Interfaces.C.long;
      pPreviousCount    : PLONG) return BOOL;
   pragma Import (Stdcall, ReleaseSemaphore, "ReleaseSemaphore");

   function CreateEvent (
      pEventAttributes  : Win_Task.Threads.PSECURITY_ATTRIBUTES;
      bManualReset      : BOOL;
      bInitialState     : BOOL;
      pName             : PSZ) return HANDLE;
   pragma Import (Stdcall, CreateEvent, "CreateEventA");

   function OpenEvent (
      dwDesiredAccess   : DWORD;
      bInheritHandle    : BOOL;
      pName             : PSZ) return HANDLE;
   pragma Import (Stdcall, OpenEvent, "OpenEventA");

   function SetEvent (
      hEvent            : HANDLE) return BOOL;
   pragma Import (Stdcall, SetEvent, "SetEvent");

   function ResetEvent (
      hEvent            : HANDLE) return BOOL;
   pragma Import (Stdcall, ResetEvent, "ResetEvent");

   function PulseEvent (
      hEvent            : HANDLE) return BOOL;
   pragma Import (Stdcall, PulseEvent, "PulseEvent");

   function CreateMutex (
      pMutexAttributes  : Win_Task.Threads.PSECURITY_ATTRIBUTES;
      bInitialOwner     : BOOL;
      pName             : PSZ) return HANDLE;
   pragma Import (Stdcall, CreateMutex, "CreateMutexA");

   function OpenMutex (
      dwDesiredAccess   : DWORD;
      bInheritHandle    : BOOL;
      pName             : PSZ) return HANDLE;
   pragma Import (Stdcall, OpenMutex, "OpenMutexA");

   function ReleaseMutex (
      hMutex   : HANDLE) return BOOL;
   pragma Import (Stdcall, ReleaseMutex, "ReleaseMutex");


private

   type LIST_ENTRY is
      record
         forwardLink    : PLIST_ENTRY;
         backwardLink   : PLIST_ENTRY;
      end record;

   type BACK_TRACE is array (1 .. 5) of PVOID;

   type CRITICAL_SECTION_DEBUG is
      record
         wType                   : WORD;
         wCreatorBackTraceIndex  : WORD;
         CriticalSection         : PCRITICAL_SECTION;
         ProcessLocksList        : LIST_ENTRY;
         dwEntryCount            : DWORD;
         dwContentionCount       : DWORD;
         dwDepth                 : DWORD;
         OwnerBackTrace          : BACK_TRACE;
      end record;
   type PCRITICAL_SECTION_DEBUG is access all CRITICAL_SECTION_DEBUG;

   type CRITICAL_SECTION is
      record
         DebugInfo      : PCRITICAL_SECTION_DEBUG;
         --
         --  The following three fields control entering and
         --  exiting the critical section for the resource
         --
         LockCount      : Long_Integer;
         RecursionCount : Long_Integer;
         OwningThread   : HANDLE;
         LockSemaphore  : HANDLE;
         Reserved       : DWORD;
      end record;

end Win_Task.Synchronization;
