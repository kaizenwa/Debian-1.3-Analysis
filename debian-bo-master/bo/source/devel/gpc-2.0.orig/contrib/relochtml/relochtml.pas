Program RelocHTML;


(*************************************************************************)
(*                                                                       *)
(*  RelocHTML - a HTML relocator.                                        *)
(*                                                                       *)
(*  Copyright (C) 1996  Peter Gerwinski  <peter.gerwinski@uni-essen.de>  *)
(*                                                                       *)
(*  RelocHTML is free software; you can redistribute and/or modify it    *)
(*  under the terms of the GNU General Public License, version 2, as     *)
(*  published by the Free Software Foundation.                           *)
(*                                                                       *)
(*  You should have received a copy of the GNU General Public License    *)
(*  along with RelocHTML (see the file COPYING); if not, write to the    *)
(*  Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,   *)
(*  USA.                                                                 *)
(*                                                                       *)
(*  RelocHTML is distributed in the hope that it will be useful, but     *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU    *)
(*  General Public License for more details.                             *)
(*                                                                       *)
(*************************************************************************)


(* To compile, type (at least;)                   *)
(*     gpc --automake relochtml.pas -o relochtml  *)


(*$N+*)  (* allow nested comments *)
(*$B-*)  (* short circuit Boolean operations *)


uses
  Fifos;


Type
  ioFifoPtr = ^ioFifoObj;


ioFifoObj = object ( FifoObj )
  Procedure Flush;
  Procedure Trim ( l: Integer );
  Procedure Advance;
end (* ioFifoObj *);


Type
  WrkString = String ( 80 );


Var
  C: Char;
  Fifo: ioFifoObj;
  NewDirectory: WrkString;


Function LoCase ( C: Char ): Char;

begin (* LoCase *)
  case C of
    chr ( 10 ):
      LoCase:= ' ';
    'A'..'Z':
      LoCase:= chr ( ord ( C ) - ord ( 'A' ) + ord ( 'a' ) );
    otherwise:
      LoCase:= C;
  end (* case *);
end (* LoCase *);


Function ReadChar: Char;

Var
  isEoln: __static__ Boolean value false;
  C: Char;

begin (* ReadChar *)
  read ( C );
  ReadChar:= C;
  if isEoln then
    begin
      ReadChar:= chr ( 10 );
      isEoln:= false;
    end (* if *);
  if eof ( Input ) or eoln ( Input ) then
    isEoln:= true;
end (* ReadChar *);


Procedure WriteChar ( C: Char );

begin (* WriteChar *)
  if C = chr ( 10 ) then
    writeln
  else
    write ( C );
end (* WriteChar *);


(* Use the GPC run-time-system's ParamCount and ParamStr function *)
(* to implement Borland-compatible versions of these functions.   *)

Function rtsParamCount: Integer;
AsmName '_p_paramcount';

Function rtsParamStr ( i: Integer; Var S: String ): Boolean;
AsmName '_p_paramstr';


Function ParamCount: Integer;  (* ;-) *)

begin (* ParamCount *)
  ParamCount:= rtsParamCount - 1;
end (* ParamCount *);


Function ParamStr ( i: Integer ): WrkString;

Var
  S: WrkString;

begin (* ParamStr *)
  if rtsParamStr ( i, S ) then
    ParamStr:= Trim ( S )
  else
    ParamStr:= '';
end (* ParamStr *);


Procedure ioFifoObj.Flush;

Var
  i: Integer;

begin (* ioFifoObj.Flush *)
  for i:= 1 to Count do
    WriteChar ( Get );
end (* ioFifoObj.Flush *);


Procedure ioFifoObj.Trim ( l: Integer );

begin (* ioFifoObj.Trim *)
  while ( Count < l ) 
   and not eof ( Input ) do
    Put ( ReadChar );
  while ( Count > l ) do
    WriteChar ( Get );
end (* ioFifoObj.Trim *);


Procedure ioFifoObj.Advance;

begin (* ioFifoObj.Advance *)
  Put ( ReadChar );
  WriteChar ( Get );
end (* ioFifoObj.Advance *);


Procedure HandleCommand;


Var
  C: Char;


Function Command ( which: WrkString ): Boolean;

Var
  l, i: Integer;

begin (* Command *)
  l:= length ( which );
  i:= 0;
  while ( i < l )
   and ( LoCase ( Fifo.Peek ( i ) ) = which [ i + 1 ] ) do
    inc ( i );
  Command:= i >= l;
end (* Command *);


Procedure SkipUntil ( what: WrkString );

begin (* SkipUntil *)
  Fifo.Trim ( length ( what ) );
  while not eof ( Input ) 
   and not Command ( what ) do
    Fifo.Advance;
  Fifo.Flush;
end (* SkipUntil *);


Procedure HandleReference ( what: WrkString );

Var
  i: Integer;

begin (* HandleReference *)
  Fifo.Trim ( length ( what ) );
  while not eof ( Input )
   and not Command ( what )
   and ( Fifo.Peek ( 0 ) <> '>' ) do
    Fifo.Advance;
  if Command ( what ) then
    begin
      for i:= 1 to Fifo.Count - 1 do
        WriteChar ( Fifo.Get );
      write ( NewDirectory );
      Fifo.Reset;
    end (* if *);
end (* HandleReference *);


begin (* HandleCommand *)
  repeat
    if Command ( '<!--' ) then
      SkipUntil ( '-->' )
    else if Command ( '<a ' ) then
      HandleReference ( 'href="/' )
    else if Command ( '<img ' ) then
      HandleReference ( 'src="/' )
    else
      begin
        C:= ReadChar;
        Fifo.Put ( C );
      end (* else *);
  until ( Fifo.Count > 5 ) or ( C = '>' ) or eof ( Input );
end (* HandleCommand *);


begin
  if ParamCount <> 1 then
    begin
      writeln ( 'RelocHTML  Copyright (C) 1996  Peter Gerwinski  <peter.gerwinski@uni-essen.de>' );
      writeln ( 'FreeWare according to the GNU GPL, version 2 (file COPYING).  NO WARRANTY!' );
      writeln ( 'Usage:  relochtml /new/directory <sourcefile.html >destfile.html' );
      writeln ( 'See README for details.' );
      Halt ( 0 );
    end (* if *);
  NewDirectory:= ParamStr ( 1 );
  if NewDirectory [ 1 ] = '"' then
    NewDirectory:= NewDirectory [ 2 .. length ( NewDirectory ) - 1 ];
  if NewDirectory [ length ( NewDirectory ) ] <> '/' then
    NewDirectory:= NewDirectory + '/';
  Fifo.Init ( 16 );
  while not eof ( Input ) do
    begin
      C:= ReadChar;
      Fifo.Put ( C );
      if C = '<' then
        HandleCommand;
      Fifo.Flush;
    end (* while *);
  Fifo.Fini;
end.
