/* Sparc simulator.
   Copyright 1995 Tristan Gingold
		  Written Juny 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/

#ifndef _ASM
#include <sys/types.h>

union Fregs
{
  float s[32];
  double d[16];
  long double x[8];
  uint i[32];
};

union Instr
{
  uint word;
  struct
    {
      uint op : 2;
      int disp30 : 30;
    } format_call;
  struct
    {
      uint op : 2;
      uint rd : 5;
      uint op2 : 3;
      int imm22 : 22;
    } format_sethi;
  struct
    {
      uint op : 2;
      uint a : 1;
      uint cond : 4;
      uint op2 : 3;
      int disp22 : 22;
    } format_branch;
  struct
    {
      uint op : 2;
      uint rd : 5;
      uint op3 : 6;
      uint rs1 : 5;
      uint i : 1;
      uint asi : 8;
      uint rs2 : 5;
    } format_rs2;
  struct
    {
      uint op : 2;
      uint rd : 5;
      uint op3 : 6;
      uint rs1 : 5;
      uint i : 1;
      int imm13 : 13;
    } format_imm;
  struct
    {
      uint op : 2;
      uint rd : 5;
      uint op3 : 6;
      uint rs1 : 5;
      uint opf : 9;
      uint rs2 : 5;
    } format_fp;
};

/* Well known field of an instruction word. */
#define RD instr.format_imm.rd
#define RS2 instr.format_fp.rs2
#define RS1 instr.format_fp.rs1
#define R_RD regs[RD]
#define R_RS1 regs[instr.format_imm.rs1]
#define N_RD regs_name[RD]

struct psr
{
  uint impl : 4;
  uint ver : 4;
  uint n : 1;
  uint z : 1;
  uint v : 1;
  uint c : 1;
  uint reserved : 6;
  uint ec : 1;
  uint ef : 1;
  uint pil : 4;
  uint s : 1;
  uint ps : 1;
  uint et : 1;
  uint cwp : 5;
};

struct fpsr
{
  uint rd : 2;
  uint rp : 2;
  uint nvm : 1;
  uint ofm : 1;
  uint ufm : 1;
  uint dzm : 1;
  uint nxm : 1;
  uint ns : 1;
  uint reserved1 : 2;
  uint version : 3;
  uint fft : 2;
  uint qne : 1;
  uint reserved2 : 1;
  uint fcc : 2;
  uint nva : 1;
  uint ofa : 1;
  uint ufa : 1;
  uint dza : 1;
  uint nxa : 1;
  uint nvc : 1;
  uint ofc : 1;
  uint ufc : 1;
  uint dzc : 1;
  uint nxc : 1;
};

struct SPARC_regs
{
  uint pc;
  uint npc;
  struct psr psr;
  uint regs[32];
  uint y;
  struct fpsr fpsr;
  union Fregs fregs;
};

extern uint pc;
extern uint npc;
extern struct psr psr;
extern uint regs[32];
extern uint y;
extern struct fpsr fpsr;
extern union Fregs fregs;

extern void sim (int ss);
extern void gdb_server_sim (void);

#endif

#define G0 0
#define G1 1
#define G2 3
#define G3 3
#define G4 4
#define G5 5
#define G6 6
#define G7 7
#define O0 8
#define O1 9
#define O2 10
#define O3 11
#define O4 12
#define O5 13
#define O6 14
#define O7 15
#define SP 14
#define L0 16
#define L1 17
#define L2 18
#define L3 19
#define L4 20
#define L5 21
#define L6 22
#define L7 23
#define I0 24
#define I1 25
#define I2 26
#define I3 27
#define I4 28
#define I5 29
#define I6 30
#define I7 31
#define FP 30
