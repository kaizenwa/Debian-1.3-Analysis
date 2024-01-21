(* Batcher.isl *)
(* $Id: Batcher.isl,v 1.1 1995/08/18 00:05:55 spreitze Exp $ *)
(* Last edited by Mike Spreitzer August 17, 1995 8:17 am PDT *)

INTERFACE Batcher;

TYPE Time = LONG CARDINAL;
TYPE TimeRec = RECORD s: Time, r: Time END;
TYPE TimeSeq = SEQUENCE OF TimeRec;

TYPE T = OBJECT METHODS
	ASYNCHRONOUS Send(s: Time),
	Sync(s: Time): TimeSeq
	END;
