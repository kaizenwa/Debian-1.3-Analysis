Unit Fifos;


(*************************************************************************)
(*                                                                       *)
(*  FIFOs - a simple FIFO buffer object.                                 *)
(*                                                                       *)
(*  Copyright (C) 1996  Peter Gerwinski  <peter.gerwinski@uni-essen.de>  *)
(*                                                                       *)
(*  FIFOs is a free library; you can redistribute and/or modify it under *)
(*  the terms of the GNU Library General Public License, version 2, as   *)
(*  published by the Free Software Foundation.                           *)
(*                                                                       *)
(*  You should have received a copy of the GNU Library General Public    *)
(*  License along with FIFOs (see the file COPYING.LIB); if not, write   *)
(*  to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,      *)
(*  MA 02139, USA.                                                       *)
(*                                                                       *)
(*  FIFOs is distributed in the hope that it will be useful, but WITHOUT *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY   *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General    *)
(*  Public License for more details.                                     *)
(*                                                                       *)
(*************************************************************************)


(*$N+*)  (* allow nested comments *)
(*$B-*)  (* short circuit Boolean operations *)


Interface


Type
  CharArray = array [ 0..MaxInt ] of Char;
  CharArrayPtr = ^CharArray;


Type
  FifoPtr = ^FifoObj;


FifoObj = object
  Length, BufIn, BufOut: Integer;
  Buffer: CharArrayPtr;
  Constructor Init ( aLength: Integer );
  Destructor Fini; virtual;
  Procedure Put ( C: Char );
  Function Get: Char;
  Function Peek ( offset: Integer ): Char;
  Function Count: Integer;
  Procedure Reset;
end (* FifoObj *);


Implementation


(* It is kind of overkill to use objects for the simple task of RelocHTML, *)
(* but we need interesting demo programs for GNU Pascal. ;-)  On the other *)
(* side, FifoObj is a quite simple implementation of a FIFO buffer with    *)
(* fixed maximum length, no checks, etc., so this Unit might motivate      *)
(* programmers to write more elaborate libraries for GNU Pascal ...        *)


Constructor FifoObj.Init ( aLength: Integer );

begin (* FifoObj.Init *)
  Length:= aLength;
  BufIn:= 0;
  BufOut:= 0;
  Buffer:= GetMem ( Length );
end (* FifoObj.Init *);


Destructor FifoObj.Fini;

begin (* FifoObj.Fini *)
  FreeMem ( Buffer, Length );
end (* FifoObj.Fini *);


Procedure FifoObj.Put ( C: Char );

begin (* FifoObj.Put *)
  Buffer^ [ BufIn ]:= C;
  BufIn:= ( BufIn + 1 ) mod Length;
end (* FifoObj.Put *);


Function FifoObj.Get: Char;

begin (* FifoObj.Get *)
  if BufOut = BufIn then
    Get:= chr ( 0 )
  else
    begin
      Get:= Buffer^ [ BufOut ];
      BufOut:= ( BufOut + 1 ) mod Length;
    end (* else *);
end (* FifoObj.Get *);


Function FifoObj.Peek ( Offset: Integer ): Char;

Var
  i, b: Integer;

begin (* FifoObj.Peek *)
  b:= BufOut;
  i:= 0;
  while ( i < Offset ) and ( b <> BufIn ) do
    begin
      inc ( i );
      b:= ( b + 1 ) mod Length;
    end (* while *);
  if b = BufIn then
    Peek:= chr ( 0 )
  else
    Peek:= Buffer^ [ b ];
end (* FifoObj.Peek *);


Function FifoObj.Count: Integer;

Var
  Result: Integer;

begin (* FifoObj.Count *)
  Result:= BufIn - BufOut;
  while Result < 0 do
    inc ( Result, Length );
  Count:= Result
end (* FifoObj.Count *);


Procedure FifoObj.Reset;

begin (* FifoObj.Reset *)
  BufIn:= 0;
  BufOut:= 0;
end (* FifoObj.Reset *);


end.
