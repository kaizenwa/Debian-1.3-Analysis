INTERFACE ilu BRAND "version 2";

(*
   Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  
   
   Unlimited use, reproduction, and distribution of this software is
   permitted.  Any copy of this software must include both the above
   copyright notice of Xerox Corporation and this paragraph.  Any
   distribution of this software must comply with all applicable United
   States export control laws.  This software is made available AS IS,
   and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
   INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
   PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
   THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
   CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
   XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

   $Id: ilu.isl,v 1.7 1995/12/05 02:00:25 janssen Exp $
*)

TYPE CString = SEQUENCE OF SHORT CHARACTER;

TYPE ProtocolErrorDetail =
  ENUMERATION
    NoSuchClassAtServer,	(* server doesn't handle specified class *)
    BrandMismatch,		(* versions out of sync *)
    NoSuchMethodOnClass,	(* invalid method, or method not implemented *)
    InvalidArguments,		(* bad arguments passed *)
    UnknownObjectInstance,	(* specified instance not on server *)
    UnreachableModule,		(* no path to handler *)
    RequestRejectedByModule,	(* request not looked at, for some reason *)
    TimeoutOnRequest,		(* no response from server within timeout *)
    UnknownError		(* catchall error *)
  END;

EXCEPTION ProtocolError : ProtocolErrorDetail;

