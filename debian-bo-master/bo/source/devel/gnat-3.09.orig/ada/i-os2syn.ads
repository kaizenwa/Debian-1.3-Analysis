------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    I N T E R F A C E S . O S 2 L I B . S Y N C H R O N I Z A T I O N     --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.10 $                            --
--                                                                          --
--        Copyright (C) 1993,1994,1995 Free Software Foundation, Inc.       --
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

with Interfaces.C;
with Interfaces.OS2Lib.Threads;
with System;

package Interfaces.OS2Lib.Synchronization is
pragma Preelaborate (Synchronization);

   package IC  renames Interfaces.C;
   package IOT renames Interfaces.OS2Lib.Threads;
   package S   renames System;

   --  Critical sections

   function DosEnterCritSec return APIRET;
   pragma Import (C, DosEnterCritSec, "DosEnterCritSec");

   function DosExitCritSec return APIRET;
   pragma Import (C, DosExitCritSec, "DosExitCritSec");

   --  Semaphore Attributes

   DC_SEM_SHARED : constant := 16#01#;
   --  DosCreateMutex, DosCreateEvent, and DosCreateMuxWait use it to indicate
   --  whether the semaphore is shared or private when the PSZ is null

   DCMW_WAIT_ANY : constant := 16#02#;
   --  DosCreateMuxWait option for wait on any event/mutex to occur

   DCMW_WAIT_ALL : constant := 16#04#;
   --  DosCreateMuxWait option for wait on all events/mutexes to occur

   SEM_INDEFINITE_WAIT  : constant ULONG := -1;
   SEM_IMMEDIATE_RETURN : constant ULONG :=  0;

   type HSEM is new System.Address;
   type PHSEM is access all HSEM;

   type SEMRECORD is
      record
         hsemCur : HSEM;
         ulUser  : ULONG;
      end record;
   type PSEMRECORD is access all SEMRECORD;


   type HEV is new IC.unsigned_long;
   type PHEV is access all HEV;

   type HMTX  is new IC.unsigned_long;
   type PHMTX is access all HMTX;

   type HMUX  is new IC.unsigned_long;
   type PHMUX is access all HMUX;

   function DosCreateEventSem
     (pszName   : PSZ;
      f_phev    : PHEV;
      flAttr    : ULONG;
      fState    : BOOL32)
      return      APIRET;
   pragma Import (C, DosCreateEventSem, "DosCreateEventSem");

   function DosOpenEventSem
     (pszName   : PSZ;
      F_phev    : PHEV)
      return      APIRET;
   pragma Import (C, DosOpenEventSem, "DosOpenEventSem");

   function DosCloseEventSem
     (F_hev     : HEV)
      return      APIRET;
   pragma Import (C, DosCloseEventSem, "DosCloseEventSem");

   function DosResetEventSem
     (F_hev     : HEV;
      pulPostCt : PULONG)
      return      APIRET;
   pragma Import (C, DosResetEventSem, "DosResetEventSem");

   function DosPostEventSem
     (F_hev     : HEV)
      return      APIRET;
   pragma Import (C, DosPostEventSem, "DosPostEventSem");

   function DosWaitEventSem
     (F_hev     : HEV;
      ulTimeout : ULONG)
      return      APIRET;
   pragma Import (C, DosWaitEventSem, "DosWaitEventSem");

   function DosQueryEventSem
     (F_hev     : HEV;
      pulPostCt : PULONG)
      return      APIRET;
   pragma Import (C, DosQueryEventSem, "DosQueryEventSem");

   function DosCreateMutexSem
     (pszName   : PSZ;
      F_phmtx   : PHMTX;
      flAttr    : ULONG;
      fState    : BOOL32)
      return      APIRET;
   pragma Import (C, DosCreateMutexSem, "DosCreateMutexSem");

   function DosOpenMutexSem
     (pszName   : PSZ;
      F_phmtx   : PHMTX)
      return      APIRET;
   pragma Import (C, DosOpenMutexSem, "DosOpenMutexSem");

   function DosCloseMutexSem
     (F_hmtx    : HMTX)
      return      APIRET;
   pragma Import (C, DosCloseMutexSem, "DosCloseMutexSem");

   function DosRequestMutexSem
     (F_hmtx    : HMTX;
      ulTimeout : ULONG)
      return      APIRET;
   pragma Import (C, DosRequestMutexSem, "DosRequestMutexSem");

   function DosReleaseMutexSem
     (F_hmtx    : HMTX)
      return      APIRET;
   pragma Import (C, DosReleaseMutexSem, "DosReleaseMutexSem");

   function DosQueryMutexSem
     (F_hmtx    : HMTX;
      F_ppid    : IOT.PPID;
      F_ptid    : IOT.PTID;
      pulCount  : PULONG)
      return      APIRET;
   pragma Import (C, DosQueryMutexSem, "DosQueryMutexSem");

   function DosCreateMuxWaitSem
     (pszName   : PSZ;
      F_phmux   : PHMUX;
      cSemRec   : ULONG;
      pSemRec   : PSEMRECORD;
      flAttr    : ULONG)
      return      APIRET;
   pragma Import (C, DosCreateMuxWaitSem, "DosCreateMuxWaitSem");

   function DosOpenMuxWaitSem
     (pszName   : PSZ;
      F_phmux   : PHMUX)
      return      APIRET;
   pragma Import (C, DosOpenMuxWaitSem, "DosOpenMuxWaitSem");

   function DosCloseMuxWaitSem
     (F_hmux    : HMUX)
      return      APIRET;
   pragma Import (C, DosCloseMuxWaitSem, "DosCloseMuxWaitSem");

   function DosWaitMuxWaitSem
     (F_hmux    : HMUX;
      ulTimeout : ULONG;
      pulUser   : PULONG)
      return      APIRET;
   pragma Import (C, DosWaitMuxWaitSem, "DosWaitMuxWaitSem");

   function DosAddMuxWaitSem
     (F_hmux    : HMUX;
      pSemRec   : PSEMRECORD)
      return      APIRET;
   pragma Import (C, DosAddMuxWaitSem, "DosAddMuxWaitSem");

   function DosDeleteMuxWaitSem
     (F_hmux    : HMUX;
      F_hsem    : HSEM)
      return      APIRET;
   pragma Import (C, DosDeleteMuxWaitSem, "DosDeleteMuxWaitSem");

   function DosQueryMuxWaitSem
     (F_hmux    : HMUX;
     pcSemRec   : PULONG;
     pSemRec    : PSEMRECORD;
     pflAttr    : PULONG)
     return       APIRET;
   pragma Import (C, DosQueryMuxWaitSem, "DosQueryMuxWaitSem");

end Interfaces.OS2Lib.Synchronization;
