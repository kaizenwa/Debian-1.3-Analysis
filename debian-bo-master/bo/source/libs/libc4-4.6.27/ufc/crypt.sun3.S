|
| UFC-crypt: ultra fast crypt(3) implementation
| Copyright (C) 1991, 1992, Michael Glad, email: glad@daimi.aau.dk
|
| This library is free software, you can redistribute it and/or
| modify it under the terms of the GNU Library General Public
| License as published by the Free Software Foundation, either
| version 2 of the License, or (at your option) any later version.
|
| This library is distributed in the hope that it will be useful,
| but WITHOUT ANY WARRANTY, without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
| Library General Public License for more details.
|
| You should have received a copy of the GNU Library General Public
| License along with this library, if not, write to the Free
| Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|
| @(#)crypt.sun3.S	2.8 01/11/92
|
| Assembly code for SUN3 68000 based workstations.
|

#define L1	d0
#define L2	d1
#define R1	d2
#define R2	d3
#define ITR	d4
#define SCR1	d5
#define SCR2	d6

#define I       d7

#define SB0	a0
#define SB1	a1
#define SB2	a2
#define SB3	a3
#define KPTR	a4
	
#define F(I,O1,O2,SBX,SBY)						\
	movl 	KPTR@+,SCR1 ;		eorl 	I,SCR1 ;		\
	movl	SBX@(0,SCR1:w),SCR2 ; 	eorl	SCR2,O1 ;		\
	movl	SBX@(4,SCR1:w),SCR2 ;	eorl	SCR2,O2 ;		\
	swap	SCR1 ;							\
	movl	SBY@(0,SCR1:w),SCR2 ;	eorl	SCR2,O1 ;		\
	movl	SBY@(4,SCR1:w),SCR2 ;	eorl	SCR2,O2 ;

#define G(I1,I2,O1,O2)			\
	F(I1,O1,O2,SB1,SB0) F(I2,O1,O2,SB3,SB2)

#define H G(R1,R2,L1,L2) ; G(L1,L2,R1,R2)

	.text
	.proc
	.globl _ufc_doit
_ufc_doit:
|
| Preamble
|
	moveml	d2-d7/a2-a4,sp@-

	moveml	sp@(40),L1/L2/R1/R2/ITR
|
| Setup address registers with sb pointers
|
	movl	#_ufc_sb0,SB0 ; movl #_ufc_sb1,SB1 
	movl    #_ufc_sb2,SB2 ; movl #_ufc_sb3,SB3
| 
| And loop...
|
Lagain:	
	movl  #_ufc_keytab,KPTR
	moveq #7,I
Lagain1:
	H
	dbra   I,Lagain1	
| Permute
	exg L1,R1
	exg L2,R2
|
	subql	#1,ITR
	jne	Lagain
|
| Output conversion
|
	moveml L1/L2/R1/R2,sp@-
	jbsr _ufc_dofinalperm ; addl #16,sp
|
| Postamble
|
	moveml 	sp@+,d2-d7/a2-a4
	rts
