/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                              A _ A T R E E                               */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*                            $Revision: 1.46 $                             */
/*                                                                          */
/*          Copyright (C) 1992-1997, Free Software Foundation, Inc.         */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* This is the C header corresponding to the Ada package specification for
   Atree. It also contains the implementations of inlined functions from the
   package body for Tree.  It was generated manually from atree.ads and
   atree.adb and must be kept synchronized with changes in these files.

   Note that only routines for reading the tree are included, since the tree
   transformer is not supposed to modify the tree in any way. */

/* Structure used for extra flags in third component overlaying Nkind */
struct Flag_Byte
{
  Boolean      flag65	    :  1;
  Boolean      flag66	    :  1;
  Boolean      flag67	    :  1;
  Boolean      flag68	    :  1;
  Boolean      flag69	    :  1;
  Boolean      flag70	    :  1;
  Boolean      flag71	    :  1;
  Boolean      flag72	    :  1;
};

/* Structure used for extra flags in third component overlaying Field12 */
struct Flag_Word
{
  Boolean      flag73	    :  1;
  Boolean      flag74	    :  1;
  Boolean      flag75	    :  1;
  Boolean      flag76	    :  1;
  Boolean      flag77	    :  1;
  Boolean      flag78	    :  1;
  Boolean      flag79	    :  1;
  Boolean      flag80	    :  1;
  Boolean      flag81	    :  1;
  Boolean      flag82	    :  1;
  Boolean      flag83	    :  1;
  Boolean      flag84	    :  1;
  Boolean      flag85	    :  1;
  Boolean      flag86	    :  1;
  Boolean      flag87	    :  1;
  Boolean      flag88	    :  1;
  Boolean      flag89	    :  1;
  Boolean      flag90	    :  1;
  Boolean      flag91	    :  1;
  Boolean      flag92	    :  1;
  Boolean      flag93	    :  1;
  Boolean      flag94	    :  1;
  Boolean      flag95	    :  1;
  Boolean      flag96	    :  1;
  Short        convention   :  8;
};

/* Structure used for extra flags in fourth component overlaying Field12 */
struct Flag_Word2
{
  Boolean      flag97	    :  1;
  Boolean      flag98	    :  1;
  Boolean      flag99	    :  1;
  Boolean      flag100	    :  1;
  Boolean      flag101	    :  1;
  Boolean      flag102	    :  1;
  Boolean      flag103	    :  1;
  Boolean      flag104	    :  1;
  Boolean      flag105	    :  1;
  Boolean      flag106	    :  1;
  Boolean      flag107	    :  1;
  Boolean      flag108	    :  1;
  Boolean      flag109	    :  1;
  Boolean      flag110	    :  1;
  Boolean      flag111	    :  1;
  Boolean      flag112	    :  1;
  Boolean      flag113	    :  1;
  Boolean      flag114	    :  1;
  Boolean      flag115	    :  1;
  Boolean      flag116	    :  1;
  Boolean      flag117	    :  1;
  Boolean      flag118	    :  1;
  Boolean      flag119	    :  1;
  Boolean      flag120	    :  1;
  Boolean      flag121	    :  1;
  Boolean      flag122	    :  1;
  Boolean      flag123	    :  1;
  Boolean      flag124	    :  1;
  Boolean      flag125	    :  1;
  Boolean      flag126	    :  1;
  Boolean      flag127	    :  1;
  Boolean      flag128	    :  1;
};

struct Non_Extended
{
  Source_Ptr   sloc;
  Int	       link;
  Int	       field1;
  Int	       field2;
  Int	       field3;
  Int	       field4;
  Int	       field5;
};

/* The Following structure corresponds to variant with is_extension = True.  */
struct Extended
{
  Int	       field6;
  Int	       field7;
  Int	       field8;
  Int	       field9;
  Int	       field10;
  Int	       field11;
  union
    {
      Int      field12;
      struct Flag_Word fw;
      struct Flag_Word2 fw2;
    } U;
};

/* A tree node itself.  */

struct Node
{
  Boolean      is_extension      :  1;
  Boolean      pflag1            :  1;
  Boolean      pflag2            :  1;
  Boolean      in_list           :  1;
  Boolean      rewrite_sub       :  1;
  Boolean      rewrite_ins       :  1;
  Boolean      analyzed          :  1;
  Boolean      comes_from_source :  1;

  Boolean      error_posted  :  1;
  Boolean      flag4  :  1;
  Boolean      flag5  :  1;
  Boolean      flag6  :  1;
  Boolean      flag7  :  1;
  Boolean      flag8  :  1;
  Boolean      flag9  :  1;
  Boolean      flag10 :  1;

  Boolean      flag11 :  1;
  Boolean      flag12 :  1;
  Boolean      flag13 :  1;
  Boolean      flag14 :  1;
  Boolean      flag15 :  1;
  Boolean      flag16 :  1;
  Boolean      flag17 :  1;
  Boolean      flag18 :  1;

  union
    {
      unsigned char kind;
      struct Flag_Byte fb;
    } U;

  union variant
    {
      struct Non_Extended NX;
      struct Extended EX;
    } V;
};

/* The actual tree is an array of nodes. The pointer to this array is passed
   as a parameter to the tree transformer procedure and stored in the global
   variable Nodes_Ptr after adjusting it by subtracting Node_First_Entry, so
   that Node_Id values can be used as subscripts.  */
extern struct Node *Nodes_Ptr;


#define Parent atree__parent
extern Node_Id Parent PROTO((Node_Id));

/* Overloaded Functions:

   These functions are overloaded in the original Ada source, but there is
   only one corresponding C function, which works as described below.	*/

/* Type used for union of Node_Id, List_Id, Elist_Id. */
typedef Int Tree_Id;

/* These two functions can only be used for Node_Id and List_Id values and
   they work in the C version because Empty = No_List = 0.  */

INLINE Boolean
No (N)
     Tree_Id N;
{
  return N == Empty;
}

INLINE Boolean
Present (N)
     Tree_Id N;
{
  return N != Empty;
}

/* Test the range of N to distinguish between the cases of Node_Id, List_Id
   and Elist_Id arguments.  */
extern Node_Id Parent		PROTO((Tree_Id));

/* Node Access Functions:  */

#define Nkind(N)        ((Node_Kind)(Nodes_Ptr [N].U.kind))
#define Ekind(N)        ((Entity_Kind)(Nodes_Ptr [N + 1].U.kind))
#define Sloc(N)         (Nodes_Ptr [N].V.NX.sloc)
#define Paren_Count (N) (Nodes_Ptr [N].Pflag1 + 2 * Nodes_Ptr [N].Pflag2)

#define Field1(N)     (Nodes_Ptr [N].V.NX.field1)
#define Field2(N)     (Nodes_Ptr [N].V.NX.field2)
#define Field3(N)     (Nodes_Ptr [N].V.NX.field3)
#define Field4(N)     (Nodes_Ptr [N].V.NX.field4)
#define Field5(N)     (Nodes_Ptr [N].V.NX.field5)
#define Field6(N)     (Nodes_Ptr [N+1].V.EX.field6)
#define Field7(N)     (Nodes_Ptr [N+1].V.EX.field7)
#define Field8(N)     (Nodes_Ptr [N+1].V.EX.field8)
#define Field9(N)     (Nodes_Ptr [N+1].V.EX.field9)
#define Field10(N)    (Nodes_Ptr [N+1].V.EX.field10)
#define Field11(N)    (Nodes_Ptr [N+1].V.EX.field11)
#define Field12(N)    (Nodes_Ptr [N+1].V.EX.U.field12)
#define Field13(N)    (Nodes_Ptr [N+2].V.EX.field6)
#define Field14(N)    (Nodes_Ptr [N+2].V.EX.field7)
#define Field15(N)    (Nodes_Ptr [N+2].V.EX.field8)
#define Field16(N)    (Nodes_Ptr [N+2].V.EX.field9)
#define Field17(N)    (Nodes_Ptr [N+2].V.EX.field10)
#define Field18(N)    (Nodes_Ptr [N+2].V.EX.field11)
#define Field19(N)    (Nodes_Ptr [N+3].V.EX.field6)
#define Field20(N)    (Nodes_Ptr [N+3].V.EX.field7)
#define Field21(N)    (Nodes_Ptr [N+3].V.EX.field8)
#define Field22(N)    (Nodes_Ptr [N+3].V.EX.field9)
#define Field23(N)    (Nodes_Ptr [N+3].V.EX.field10)

#define Node1(N)      (Nodes_Ptr [N].V.NX.field1)
#define Node2(N)      (Nodes_Ptr [N].V.NX.field2)
#define Node3(N)      (Nodes_Ptr [N].V.NX.field3)
#define Node4(N)      (Nodes_Ptr [N].V.NX.field4)
#define Node5(N)      (Nodes_Ptr [N].V.NX.field5)
#define Node6(N)      (Nodes_Ptr [N+1].V.EX.field6)
#define Node7(N)      (Nodes_Ptr [N+1].V.EX.field7)
#define Node8(N)      (Nodes_Ptr [N+1].V.EX.field8)
#define Node9(N)      (Nodes_Ptr [N+1].V.EX.field9)
#define Node10(N)     (Nodes_Ptr [N+1].V.EX.field10)
#define Node11(N)     (Nodes_Ptr [N+1].V.EX.field11)
#define Node12(N)     (Nodes_Ptr [N+1].V.EX.U.field12)
#define Node13(N)     (Nodes_Ptr [N+2].V.EX.field6)
#define Node14(N)     (Nodes_Ptr [N+2].V.EX.field7)
#define Node15(N)     (Nodes_Ptr [N+2].V.EX.field8)
#define Node16(N)     (Nodes_Ptr [N+2].V.EX.field9)
#define Node17(N)     (Nodes_Ptr [N+2].V.EX.field10)
#define Node18(N)     (Nodes_Ptr [N+2].V.EX.field11)
#define Node19(N)     (Nodes_Ptr [N+3].V.EX.field6)
#define Node20(N)     (Nodes_Ptr [N+3].V.EX.field7)
#define Node21(N)     (Nodes_Ptr [N+3].V.EX.field8)
#define Node22(N)     (Nodes_Ptr [N+3].V.EX.field9)
#define Node23(N)     (Nodes_Ptr [N+3].V.EX.field10)

#define List1(N)      (Nodes_Ptr [N].V.NX.field1)
#define List2(N)      (Nodes_Ptr [N].V.NX.field2)
#define List3(N)      (Nodes_Ptr [N].V.NX.field3)
#define List4(N)      (Nodes_Ptr [N].V.NX.field4)
#define List5(N)      (Nodes_Ptr [N].V.NX.field5)

#define Elist3(N)     (Nodes_Ptr [N].V.NX.field3)
#define Elist6(N)     (Nodes_Ptr [N+1].V.EX.field6)
#define Elist7(N)     (Nodes_Ptr [N+1].V.EX.field7)
#define Elist13(N)    (Nodes_Ptr [N+2].V.EX.field6)
#define Elist14(N)    (Nodes_Ptr [N+2].V.EX.field7)

#define Name1(N)      (Nodes_Ptr [N].V.NX.field1)
#define Name2(N)      (Nodes_Ptr [N].V.NX.field2)

#define Str3(N)       (Nodes_Ptr [N].V.NX.field3)

#define Char_Code2(N) (Nodes_Ptr [N].V.NX.field2 - Char_Code_Bias)

#define Uint3(N)      (Nodes_Ptr [N].V.NX.field3)
#define Uint4(N)      (Nodes_Ptr [N].V.NX.field4)
#define Uint8(N)      (Nodes_Ptr [N+1].V.EX.field8)
#define Uint9(N)      (Nodes_Ptr [N+1].V.EX.field9)
#define Uint11(N)     (Nodes_Ptr [N+1].V.EX.field11)
#define Uint12(N)     (Nodes_Ptr [N+1].V.EX.U.field12)
#define Uint13(N)     (Nodes_Ptr [N+2].V.EX.field6)
#define Uint15(N)     (Nodes_Ptr [N+2].V.EX.field8)
#define Uint16(N)     (Nodes_Ptr [N+2].V.EX.field9)
#define Uint22(N)     (Nodes_Ptr [N+3].V.EX.field9)

#define Ureal3(N)     (Nodes_Ptr [N].V.NX.field3)
#define Ureal6(N)     (Nodes_Ptr [N+1].V.EX.field6)
#define Ureal7(N)     (Nodes_Ptr [N+1].V.EX.field7)

#define Analyzed(N)          (Nodes_Ptr [N].analyzed)
#define Comes_From_Source(N) (Nodes_Ptr [N].comes_from_source)
#define Error_Posted(N)      (Nodes_Ptr [N].error_posted)

#define Flag4(N)      (Nodes_Ptr [N].flag4)
#define Flag5(N)      (Nodes_Ptr [N].flag5)
#define Flag6(N)      (Nodes_Ptr [N].flag6)
#define Flag7(N)      (Nodes_Ptr [N].flag7)
#define Flag8(N)      (Nodes_Ptr [N].flag8)
#define Flag9(N)      (Nodes_Ptr [N].flag9)
#define Flag10(N)     (Nodes_Ptr [N].flag10)
#define Flag11(N)     (Nodes_Ptr [N].flag11)
#define Flag12(N)     (Nodes_Ptr [N].flag12)
#define Flag13(N)     (Nodes_Ptr [N].flag13)
#define Flag14(N)     (Nodes_Ptr [N].flag14)
#define Flag15(N)     (Nodes_Ptr [N].flag15)
#define Flag16(N)     (Nodes_Ptr [N].flag16)
#define Flag17(N)     (Nodes_Ptr [N].flag17)
#define Flag18(N)     (Nodes_Ptr [N].flag18)

#define Flag19(N)     (Nodes_Ptr [N+1].in_list)
#define Flag20(N)     (Nodes_Ptr [N+1].rewrite_sub)
#define Flag21(N)     (Nodes_Ptr [N+1].rewrite_ins)
#define Flag22(N)     (Nodes_Ptr [N+1].analyzed)
#define Flag23(N)     (Nodes_Ptr [N+1].comes_from_source)
#define Flag24(N)     (Nodes_Ptr [N+1].error_posted)
#define Flag25(N)     (Nodes_Ptr [N+1].flag4)
#define Flag26(N)     (Nodes_Ptr [N+1].flag5)
#define Flag27(N)     (Nodes_Ptr [N+1].flag6)
#define Flag28(N)     (Nodes_Ptr [N+1].flag7)
#define Flag29(N)     (Nodes_Ptr [N+1].flag8)
#define Flag30(N)     (Nodes_Ptr [N+1].flag9)
#define Flag31(N)     (Nodes_Ptr [N+1].flag10)
#define Flag32(N)     (Nodes_Ptr [N+1].flag11)
#define Flag33(N)     (Nodes_Ptr [N+1].flag12)
#define Flag34(N)     (Nodes_Ptr [N+1].flag13)
#define Flag35(N)     (Nodes_Ptr [N+1].flag14)
#define Flag36(N)     (Nodes_Ptr [N+1].flag15)
#define Flag37(N)     (Nodes_Ptr [N+1].flag16)
#define Flag38(N)     (Nodes_Ptr [N+1].flag17)
#define Flag39(N)     (Nodes_Ptr [N+1].flag18)

#define Flag40(N)     (Nodes_Ptr [N+2].in_list)
#define Flag41(N)     (Nodes_Ptr [N+2].rewrite_sub)
#define Flag42(N)     (Nodes_Ptr [N+2].rewrite_ins)
#define Flag43(N)     (Nodes_Ptr [N+2].analyzed)
#define Flag44(N)     (Nodes_Ptr [N+2].comes_from_source)
#define Flag45(N)     (Nodes_Ptr [N+2].error_posted)
#define Flag46(N)     (Nodes_Ptr [N+2].flag4)
#define Flag47(N)     (Nodes_Ptr [N+2].flag5)
#define Flag48(N)     (Nodes_Ptr [N+2].flag6)
#define Flag49(N)     (Nodes_Ptr [N+2].flag7)
#define Flag50(N)     (Nodes_Ptr [N+2].flag8)
#define Flag51(N)     (Nodes_Ptr [N+2].flag9)
#define Flag52(N)     (Nodes_Ptr [N+2].flag10)
#define Flag53(N)     (Nodes_Ptr [N+2].flag11)
#define Flag54(N)     (Nodes_Ptr [N+2].flag12)
#define Flag55(N)     (Nodes_Ptr [N+2].flag13)
#define Flag56(N)     (Nodes_Ptr [N+2].flag14)
#define Flag57(N)     (Nodes_Ptr [N+2].flag15)
#define Flag58(N)     (Nodes_Ptr [N+2].flag16)
#define Flag59(N)     (Nodes_Ptr [N+2].flag17)
#define Flag60(N)     (Nodes_Ptr [N+2].flag18)
#define Flag61(N)     (Nodes_Ptr [N+1].pflag1)
#define Flag62(N)     (Nodes_Ptr [N+1].pflag2)
#define Flag63(N)     (Nodes_Ptr [N+2].pflag1)
#define Flag64(N)     (Nodes_Ptr [N+2].pflag2)

#define Flag65(N)     (Nodes_Ptr [N+2].U.fb.flag65)
#define Flag66(N)     (Nodes_Ptr [N+2].U.fb.flag66)
#define Flag67(N)     (Nodes_Ptr [N+2].U.fb.flag67)
#define Flag68(N)     (Nodes_Ptr [N+2].U.fb.flag68)
#define Flag69(N)     (Nodes_Ptr [N+2].U.fb.flag69)
#define Flag70(N)     (Nodes_Ptr [N+2].U.fb.flag70)
#define Flag71(N)     (Nodes_Ptr [N+2].U.fb.flag71)
#define Flag72(N)     (Nodes_Ptr [N+2].U.fb.flag72)

#define Flag73(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag73)
#define Flag74(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag74)
#define Flag75(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag75)
#define Flag76(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag76)
#define Flag77(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag77)
#define Flag78(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag78)
#define Flag79(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag79)
#define Flag80(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag80)
#define Flag81(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag81)
#define Flag82(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag82)
#define Flag83(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag83)
#define Flag84(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag84)
#define Flag85(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag85)
#define Flag86(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag86)
#define Flag87(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag87)
#define Flag88(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag88)
#define Flag89(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag89)
#define Flag90(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag90)
#define Flag91(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag91)
#define Flag92(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag92)
#define Flag93(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag93)
#define Flag94(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag94)
#define Flag95(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag95)
#define Flag96(N)     (Nodes_Ptr [N+2].V.EX.U.fw.flag96)

#define Convention(N)     (Nodes_Ptr [N+2].V.EX.U.fw.convention)

#define Flag97(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag97)
#define Flag98(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag98)
#define Flag99(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag99)
#define Flag100(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag100)
#define Flag101(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag101)
#define Flag102(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag102)
#define Flag103(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag103)
#define Flag104(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag104)
#define Flag105(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag105)
#define Flag106(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag106)
#define Flag107(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag107)
#define Flag108(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag108)
#define Flag109(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag109)
#define Flag110(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag110)
#define Flag111(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag111)
#define Flag112(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag112)
#define Flag113(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag113)
#define Flag114(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag114)
#define Flag115(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag115)
#define Flag116(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag116)
#define Flag117(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag117)
#define Flag118(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag118)
#define Flag119(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag119)
#define Flag120(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag120)
#define Flag121(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag121)
#define Flag122(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag122)
#define Flag123(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag123)
#define Flag124(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag124)
#define Flag125(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag125)
#define Flag126(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag126)
#define Flag127(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag127)
#define Flag128(N)     (Nodes_Ptr [N+3].V.EX.U.fw2.flag128)

#define Flag129(N)     (Nodes_Ptr [N+3].in_list)
#define Flag130(N)     (Nodes_Ptr [N+3].rewrite_sub)
#define Flag131(N)     (Nodes_Ptr [N+3].rewrite_ins)
#define Flag132(N)     (Nodes_Ptr [N+3].analyzed)
#define Flag133(N)     (Nodes_Ptr [N+3].comes_from_source)
#define Flag134(N)     (Nodes_Ptr [N+3].error_posted)
#define Flag135(N)     (Nodes_Ptr [N+3].flag4)
#define Flag136(N)     (Nodes_Ptr [N+3].flag5)
#define Flag137(N)     (Nodes_Ptr [N+3].flag6)
#define Flag138(N)     (Nodes_Ptr [N+3].flag7)
#define Flag139(N)     (Nodes_Ptr [N+3].flag8)
#define Flag140(N)     (Nodes_Ptr [N+3].flag9)
#define Flag141(N)     (Nodes_Ptr [N+3].flag10)
#define Flag142(N)     (Nodes_Ptr [N+3].flag11)
#define Flag143(N)     (Nodes_Ptr [N+3].flag12)
#define Flag144(N)     (Nodes_Ptr [N+3].flag13)
#define Flag145(N)     (Nodes_Ptr [N+3].flag14)
#define Flag146(N)     (Nodes_Ptr [N+3].flag15)
#define Flag147(N)     (Nodes_Ptr [N+3].flag16)
#define Flag148(N)     (Nodes_Ptr [N+3].flag17)
#define Flag149(N)     (Nodes_Ptr [N+3].flag18)
#define Flag150(N)     (Nodes_Ptr [N+3].pflag1)
#define Flag151(N)     (Nodes_Ptr [N+3].pflag2)

/* End of a-atree.h (C version of Atree package specification) */
