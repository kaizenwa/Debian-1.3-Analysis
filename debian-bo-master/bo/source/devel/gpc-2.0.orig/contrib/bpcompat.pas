Unit BPCompat;


(*************************************************************************)
(*                                                                       *)
(*  BPCompat - a GNU Pascal Unit to achive as much compatibility to      *)
(*             Borland Pascal as possible (a goal not yet reached).      *)
(*                                                                       *)
(*  Copyright (C) 1996  Peter Gerwinski  <peter.gerwinski@uni-essen.de>  *)
(*                                                                       *)
(*  BPCompat is a free library; you can redistribute and/or modify       *)
(*  it under the terms of the GNU Library General Public License,        *)
(*  version 2, as published by the Free Software Foundation.             *)
(*                                                                       *)
(*  You should have received a copy of the GNU Library General Public    *)
(*  License along with BPCompat (see the file COPYING.LIB); if not,      *)
(*  write to the Free Software Foundation, Inc., 675 Mass Ave,           *)
(*  Cambridge, MA 02139, USA.                                            *)
(*                                                                       *)
(*  BPCompat is distributed in the hope that it will be useful, but      *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU    *)
(*  Library General Public License for more details.                     *)
(*                                                                       *)
(*************************************************************************)


Interface


(* Warnings, e.g. about remaining incompatibilities, *)
(* are given as comments starting with "!!!".        *)


(*** pi ***)

Const
  pi = 3.14159265358979323846;


(*** Data types ***)

Type
  ShortInt = __byte__ Integer;
  LongInt  = Integer;
  Comp     = __longlong__ Integer;

  Byte = __unsigned__ ShortInt;
  Word = __unsigned__ Integer;

  (* !!! "Integer" is *not* made "__short__ Integer" because *)
  (* the run time library would not work with this.          *)

  Integer16 = __short__ Integer;

  Single = __short__ Real;
  Double = Real;             (* !!! There is no 6-Byte Real *)
  Extended = __long__ Real;

  (* !!! At the moment, the run time library does not work *)
  (* with modified type (e.g. __short__ ones).  Especially *)
  (* you cannot "writeln" such variables.  To work around, *)
  (* cast them to Real or Integer.                         *)

  Pointer = ^Void;
  (* !!! Pointers have 4 Bytes on Intel machines, but these Bytes  *)
  (* are *not* splitted into a 2-Byte Segment and a 2-Byte offset. *)
  (* Instead, all Pointers have the same "Segment" and contain a   *)
  (* 4-Byte Offset only.                                           *)

  WrkString = String [ 255 ];
  (* !!! Strings are stored differently *)


Const
  MaxLongInt = MaxInt;


(*** Files ***)

(* Borland style file name assignment *)
Procedure Assign ( Var T: Text; Name: WrkString );


(*** Memory ***)

Procedure FillChar ( Var Dest: Void; Count: Integer; C: Char );
(* !!! FillChar does not accept Byte values as the last parameter. *)

Procedure Move ( Var Source, Dest: Void; Count: Integer );


(*** Command line parameters, system environment ***)

Function ParamCount: Integer;
Function ParamStr ( i: Integer ): WrkString;
Function GetEnv ( Entry: WrkString ): WrkString;


(*** Random numbers ***)

(* Return a random Real number 0 <= x < 1 *)
Function Random: Real;

(* Return a random Integer number 0 <= x < limit *)
Function RandInt ( limit: Integer ): Integer;

(* !!! A Unit cannot (yet;) provide both     *)
(* "Function Random ( i: Integer ): Integer" *)
(* and "Function Random: Real", so use two   *)
(* functions instead.                        *)

(* Initialize random number generator *)
Procedure InitRandom ( Seed: Integer ); asmname 'srand';

(* !!! There is no RandSeed variable, so use this as a workaround. *)


(*** Miscellaneous functions *)

Function UpCase ( Ch: Char ): Char;


Implementation


(* There are no more "!!!" comments below. *)


Type
  CharPtr = ^Char;


Procedure Assign ( Var T: Text; Name: WrkString );

Var
  B: BindingType;

begin (* Assign *)
  unbind ( T );
  B:= binding ( T );
  B.Name:= Name + chr ( 0 );
  bind ( T, B );
  B:= binding ( T );
end (* Assign *);


Procedure FillChar ( Var Dest: Void; Count: Integer; C: Char );

Type
  BytePtr = ^Byte;

Var
  p, q: BytePtr;

begin (* FillChar *)
  (*$W-*)  (* Warning "dereferencing `void *' pointer" is a minor bug in GPC *)
  p:= @Dest;
  (*$W+*)
  q:= BytePtr ( LongInt ( p ) + Count );
  while LongInt ( p ) < LongInt ( q ) do
    begin
      p^:= ord ( C );
      inc ( LongInt ( p ) );
    end (* while *);
end (* FillChar *);


Procedure Move ( Var Source, Dest: Void; Count: Integer );

Type
  BytePtr = ^Byte;

Var
  p, q, r: BytePtr;

begin (* Move *)
  (*$W-*)  (* Warning "dereferencing `void *' pointer" is a minor bug in GPC *)
  p:= @Source;
  q:= @Dest;
  (*$W+*)
  if LongInt ( q ) > LongInt ( p ) then
    begin
      (* Avoid overwriting of overlapping memory areas *)
      r:= p;
      p:= BytePtr ( LongInt ( p ) + Count );
      q:= BytePtr ( LongInt ( q ) + Count );
      while LongInt ( p ) > LongInt ( r ) do
        begin
          dec ( LongInt ( p ) );
          dec ( LongInt ( q ) );
          q^:= p^;
        end (* while *);
    end (* if *)
  else
    begin
      r:= BytePtr ( LongInt ( p ) + Count );
      while LongInt ( p ) < LongInt ( r ) do
        begin
          q^:= p^;
          inc ( LongInt ( p ) );
          inc ( LongInt ( q ) );
        end (* while *);
    end (* else *);
end (* Move *);


(* Use the GPC run-time-system's ParamCount and ParamStr function *)

Function rtsParamCount: Integer;
AsmName '_p_paramcount';

Function rtsParamStr ( i: Integer; Var S: WrkString ): Boolean;
AsmName '_p_paramstr';


Function ParamCount: Integer;  (* This is stupid. *)

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


Function CGetEnv ( Entry: __CString__ ): CharPtr;
AsmName 'getenv';


Function GetEnv ( Entry: WrkString ): WrkString;

Var
  C: CharPtr;
  Contents: WrkString;

begin (* GetEnv *)
  C:= CGetEnv ( Entry );
  Contents:= '';
  if C <> Nil then
    while ( C^ <> chr ( 0 ) )
     and ( length ( Contents ) < Contents.Capacity ) do
      begin
        Contents:= Contents + C^;
        inc ( LongInt ( C ) );
      end (* while *);
  GetEnv:= Contents;
end (* GetEnv *);


Function CRandom: Integer;
AsmName 'rand';


Function RandInt ( limit: Integer ): Integer;

begin (* RandInt *)
  RandInt:= CRandom mod limit;
end (* RandInt *);


Function Random: Real;

begin (* Random *)
  Random:= CRandom / $7FFFFFFF;
end (* Random *);


Function UpCase ( Ch: Char ): Char;

begin (* UpCase *)
  if ( Ch >= 'a' ) and ( Ch <= 'z' ) then
    dec ( Ch, ord ( 'a' ) - ord ( 'A' ) );
  UpCase:= Ch;
end (* UpCase *);


end.
