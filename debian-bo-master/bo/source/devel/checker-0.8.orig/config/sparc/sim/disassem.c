/* Sparc disassembler.  Very primitive.
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

#include "instr.h"
#ifdef SSTEP
#define chkr_printf printf
#include <stdio.h>
#else
#include "checker.h"
#endif

#ifdef SSTEP
/* Print the Floating Point Status Register.  */
void print_fpsr (void);
#endif

/* Registers name.  */
static char *regs_name[] = 
{ 
  "%g0", "%g1", "%g2", "%g3", "%g4", "%g5", "%g6", "%g7",
  "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%sp", "%o7",
  "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",
  "%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%fp", "%i7"
};

static char *fp_regs_name[] =
{
  "%f0",  "%f1",  "%f2",  "%f3",  "%f4",  "%f5",  "%f6",  "%f7", 
  "%f8",  "%f9",  "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",   
  "%f16", "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23", 
  "%f24", "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31"
};

/* Branches name.  */
static char *br_names[] =
{
  "n", "e",  "le", "l",  "leu", "cs", "neg", "vs",
  "a", "ne", "g",  "ge", "gu",  "cc", "pos", "vc"
};

static char *fbr_names[] =
{
  "n", "ne", "lg", "ul", "l",   "ug", "g",   "u",
  "a", "e",  "ue", "ge", "uge", "le", "ule", "o"
};

/* Opcodes name.  */
static char *op_2_names[] =
{
/* 00-07 */ "add",    "and",    "or",       "xor",      "sub",    "andn",   "orn",   "xnor",
/* 08-0f */ "addc",   "mulx",   "umul",     "smul",     "subc",   "udivx",  "udiv",  "sdiv",
/* 10-17 */ "addcc",  "andcc",  "orcc",     "xorcc",    "subcc",  "andncc", "orncc", "xnorcc",
/* 18-1f */ "addccc", "rdpsr",  "umulcc",   "smulcc",   "subxcc", "5",      "udivcc","sdivcc",
/* 20-27 */ "taddcc", "tsubcc", "taddcctv", "tsubcctv", "mulscc", "sll",    "srl",   "sra",
/* 28-2f */ "rdasr",  "1",      "rdpr",     "flushw",   "movcc",  "sdivx",  "popc",  "movr",
/* 30-37 */ "wry",    "wrpsr",  "wrwim",    "wrtbr",    "FpOp",   "FpOp",   "impdep1","impdep2",
/* 38-3f */ "jmpl",   "return", "ticc",     "flush",    "save",   "restore","done",   "7"
};

static char *op_3_names[] =
{
/* 00-07 */ "ld",  "ldub",  "lduh",  "ldd",  "st",  "stb",     "sth",   "std",
/* 08-0f */ "0",   "ldsb",  "ldsh",  "3",    "4",   "ldstub",  "6",     "swap",
/* 10-17 */ "lda", "lduba", "lduha", "ldda", "sta", "stba",    "stha",  "stda",
/* 18-1f */ "0",   "ldsba", "ldsha", "3",    "4",   "ldstuba", "6",     "swapa",
/* 20-27 */ "ldf", "ldfsr", "2",     "lddf", "stf", "stfsr",   "stdfq", "stdf",
/* 28-2f */ "0",   "1",     "2",     "3",    "4",   "5",       "6",     "7",
/* 30-37 */ "ldc", "ldcsr", "lddc",  "3",   "stc",  "stcsr",   "stdcq", "stdc", 
/* 38-3f */ "0",   "1",     "2",     "3",    "4",   "5",       "6",     "7"
};

static void
print_fp_instr (union Instr instr)
{
  char name[6];
  char suffix[4] = "isdx";
  int type1;
  int type2;
  
  strcpy (name, "?????");
  type1 = type2 = 0;
  
  if (instr.format_fp.op == 0x2 && instr.format_fp.op3 == 0x35)
    switch (instr.format_fp.opf)
      {
    case 0x051:
    case 0x052:
    case 0x053:
         strcpy (name, "fcmp?");
         type1 = type2 = instr.format_fp.opf & 3;
         name[4] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x055:
    case 0x056:
    case 0x057:
         strcpy (name, "fcmpe?");
         type1 = type2 = instr.format_fp.opf & 3;
         name[5] = suffix[instr.format_fp.opf & 3];
         break;
      }
  else if (instr.format_fp.op == 0x2 && instr.format_fp.op3 == 0x34)
    switch (instr.format_fp.opf)
      {
    case 0x001:
         strcpy (name, "fmovs");
         type1 = type2 = 1;
         break;
    case 0x005:
         strcpy (name, "fnegs");
         type1 = type2 = 1;
         break;
    case 0x009:
         strcpy (name, "fabs");
         type1 = type2 = 1;
         break;
    case 0x029:
    case 0x02a:
    case 0x02b:
         strcpy (name, "fsqrt?");
         type1 = type2 = instr.format_fp.opf & 3;
         name[5] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x041:
    case 0x042:
    case 0x043:
         strcpy (name, "fadd?");
         type1 = type2 = instr.format_fp.opf & 3;
         name[4] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x049:
    case 0x04a:
    case 0x04b:
         strcpy (name, "fmul?");
         type1 = type2 = instr.format_fp.opf & 3;
         name[4] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x04d:
    case 0x04e:
    case 0x04f:
         strcpy (name, "fdiv?");
         type1 = type2 = instr.format_fp.opf & 3;
         name[4] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x085:
    case 0x086:
    case 0x087:
         strcpy (name, "fsub?");
         type1 = type2 = instr.format_fp.opf & 3;
         name[4] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x0c4:
    case 0x0c6:
    case 0x0c7:
         strcpy (name, "f?tos");
         type1 = instr.format_fp.opf & 3;
         type2 = 1;
         name[1] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x0c8:
    case 0x0c9:
    case 0x0cb:
         strcpy (name, "f?tod");
         type1 = instr.format_fp.opf & 3;
         type2 = 2;
         name[1] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x0cc:
    case 0x0cd:
    case 0x0ce:
         strcpy (name, "f?tox");
         type1 = instr.format_fp.opf & 3;
         type2 = 3;
         name[1] = suffix[instr.format_fp.opf & 3];
         break;
    case 0x0d1:
    case 0x0d2:
    case 0x0d3:
         strcpy (name, "f?toi");
         type1 = instr.format_fp.opf & 3;
         type2 = 0;
         name[1] = suffix[instr.format_fp.opf & 3];
         break;
      }
  chkr_printf ("%%f%02d:", RS1);
  switch (type1)
    {
  case 0:
      chkr_printf ("%08d", fregs.i[RS1]);
      break;
  case 1:
      chkr_printf ("%f", fregs.s[RS1]);
      break;
  case 2:
      chkr_printf ("%f", fregs.d[RS1 >> 1]);
      break;
  case 3:
      chkr_printf ("%f", (double)fregs.x[RS1 >> 2]);
      break;
    }
  chkr_printf (" %%f%02d:", RS2);
  switch (type2)
    {
  case 0:
      chkr_printf ("%08d", fregs.i[RS2]);
      break;
  case 1:
      chkr_printf ("%f", fregs.s[RS2]);
      break;
  case 2:
      chkr_printf ("%f", fregs.d[RS2 >> 1]);
      break;
  case 3:
      chkr_printf ("%f", (double)fregs.x[RS2 >> 2]);
      break;
    }
  chkr_printf (" %s %%f%d, %%f%d\n",
  		name, RS1, RS2);
}

static void
print_2_3 (union Instr instr, char *op_names[], char *reg_name[])
{
  if (instr.format_imm.i)
    chkr_printf ("%s:0x%08x                %s %s, 0x%04x, %s\n",
                 regs_name[RS1],
                 regs[RS1],
                 op_names[instr.format_imm.op3],
		 regs_name[RS1],
        	 instr.format_imm.imm13, reg_name[RD]);
  else
    chkr_printf ("%s:0x%08x %s:0x%08x %s %s, %s, %s\n",
                 regs_name[RS1],
                 regs[RS1],
                 regs_name[RS2],
                 regs[RS2],
                 op_names[instr.format_rs2.op3],
        	 regs_name[RS1],
        	 regs_name[RS2], reg_name[RD]);
}

void
disassemble (union Instr instr)
{
  chkr_printf ("pc=%08x (0x%08x)", pc, instr.word);
  switch (instr.format_call.op)
    {
  case 0x01:	/* Call */
      chkr_printf ("%%sp=0x%08x                call 0x%08x\n",
      			regs[SP],
      			pc + (instr.format_call.disp30 << 2));
      return;
  case 0x00:
      switch (instr.format_sethi.op2)
        {
      case 0x04:	/* sethi */
      	  if (instr.format_sethi.rd == G0)
      	    chkr_printf ("                              nop\n");
      	  else
            chkr_printf ("                              sethi 0x%08x, %s\n", instr.format_sethi.imm22 << 10, N_RD);
          return;
      case 0x02:	/* Bicc */
          chkr_printf (" %c%c%c%c                         b%s%s 0x%08x\n",
                        psr.n ? 'n' : '-',
                        psr.z ? 'z' : '-',
                        psr.v ? 'v' : '-',
                        psr.c ? 'c' : '-',
                        br_names[instr.format_branch.cond],
          		instr.format_branch.a ? ",a" : "",
          		pc + (instr.format_branch.disp22 << 2));
          return;
      case 0x06:	/* FBfcc */
          chkr_printf (" %c                            fb%s%s 0x%08x\n",
                        "=<>u" [fpsr.fcc],
                        fbr_names[instr.format_branch.cond],
          		instr.format_branch.a ? ",a" : "",
          		pc + (instr.format_branch.disp22 << 2));
          /* print_fpsr (); */
          return;
      case 0x00:	/* unimp */
          chkr_printf ("                              unimp 0x%08x\n", instr.word);
          return;
      default:
          chkr_printf (" ????? \n");
          return;
        }
  case 0x02:
      if (instr.format_fp.op3 == 0x35 || instr.format_fp.op3 == 0x34)
        print_fp_instr (instr);
      else
        print_2_3 (instr, op_2_names, regs_name);
      return;
  case 0x03:
      switch (instr.format_imm.op3)
        {
      case 0x23:	/* lddf */
      case 0x20:	/* ldf */
      case 0x27:	/* stdf */
      case 0x24:	/* stf */
          print_2_3 (instr, op_3_names, fp_regs_name);
          return;
      default:
          print_2_3 (instr, op_3_names, regs_name);
          return;
        }
    }
}

#ifdef SSTEP
void
print_fpsr (void)
{
  chkr_printf ("| RD | RP | NOUDN | NS | - | VER | FTT | QNE | - | FCC | NOUDN | NOUDN |\n");
  chkr_printf ("| %2d | %2d | %c%c%c%c%c | %c  | - | %3d | %3d | %c   | - | %3d | %c%c%c%c%c | %c%c%c%c%c |\n",
               fpsr.rd,
               fpsr.rp,
               fpsr.nvm ? 'n' : ' ',
               fpsr.ofm ? 'o' : ' ',
               fpsr.ufm ? 'u' : ' ',
               fpsr.dzm ? 'd' : ' ',
               fpsr.nxm ? 'n' : ' ',
               fpsr.ns ? 'n' : ' ',
               fpsr.version,
               fpsr.fft,
               fpsr.qne ? '1' : ' ',
               fpsr.fcc,
               fpsr.nva ? 'n' : ' ',
               fpsr.ofa ? 'o' : ' ',
               fpsr.ufa ? 'u' : ' ',
               fpsr.dza ? 'd' : ' ',
               fpsr.nxa ? 'n' : ' ',
               fpsr.nvc ? 'n' : ' ',
               fpsr.ofc ? 'o' : ' ',
               fpsr.ufc ? 'u' : ' ',
               fpsr.dzc ? 'd' : ' ',
               fpsr.nxc ? 'n' : ' ');
}                              
#endif
