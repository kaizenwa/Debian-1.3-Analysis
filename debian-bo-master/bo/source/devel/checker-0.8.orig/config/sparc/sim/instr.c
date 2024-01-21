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

#include "instr.h"
#include "math-sparc.h"
#ifdef SIMCHECKER
#include "checker.h"
#define sim_memcpy memcpy
#define sim_printf chkr_printf
#define abort chkr_abort
#define DISASSEMBLER
#else
#include <stdio.h>
#define DISASSEMBLER
#endif
#include "chkr-string.h"
#include <math.h>
#include <ieeefp.h>

void disassemble(union Instr instr);
void sim_printf(const char *message, ...);
void do_trap(int num);
#ifdef SIMCHECKER
void do_trap_121(void);
extern int chkr_in_checker;
extern int chkr_sig_catched;
void check_syscall (uint *regs);
void send_signal (void);
#endif
extern char *sysnames[];
static uint udiv (unsigned long long dividend, uint divi, uint *quotient);
static void umul (uint a, uint b, uint *high, uint *low);
static void smul (int a, int b, uint *high, uint *low);

#ifdef DISASSEMBLER
#ifdef SIMCHECKER
extern int flag_verbose;
#else
int flag_verbose = 1;
#endif
int disassemble_level = 0;
uint disassemble_flag = 0;
#endif

/* To stop the simulator, set sim_stop.  This is used by simserver.  */
int sim_stop;

/* Signal number. */
int sim_trap;

/* If true, PC and NPC are not updated after a trap.  This is for setcontext.
 *  It is automatically clear.  */
int do_not_update_pc_after_trap;

/* Sparc registers. */
/* Program Counter and next Program Counter. */
uint pc;
uint npc;

/* Processor Status Register. */
struct psr psr;

/* `Standard' registers of the Integer Unit. */
uint regs[32];
uint y;

/* Registers of the Floating Point Unit. */
union Fregs fregs;

/* Floating Point Status Register. */
struct fpsr fpsr;

/* Usual way to update PC. */
#define UPDATE_PC pc = npc; npc += 4

/* Extract bit 31 of x.  Used to set flags. */
#define C31(x) ((x) & 0x80000000)

#define ILLEGAL_INSTR sim_stop = 1; sim_trap = SIGILL

/* The simulator.  Registers must have been set.
 * If FOR_EVER is true, the simulator will stop when sim_stop is true.
 *  The variable sim_stop must be set by a signal handler.
 * If FOR_EVER is false, it will single step.
 */
void
sim (int for_ever)
{
 union Instr instr;
 int flag;
 int operand2;
 uint out;

 sim_stop = !for_ever;
 
 while (1)
   {
     /* Check signals. */
     if (chkr_sig_catched)
       send_signal ();
       
     /* exit ? */
     if (sim_stop)
       break;
     
     /* Read the next instruction. */  
     instr.word = *((uint*)pc);
     
#ifdef DISASSEMBLER
     if (flag_verbose > 1 || (disassemble_flag && disassemble_level > 0))
       {
         chkr_printf ("%d ", disassemble_level); 
         disassemble(instr);
       }
#endif

     /* Decode and execute. */
     switch (instr.format_call.op)
       {
     case 0x01:		/* Call */
	 regs[O7] = pc;
	 pc = npc;
	 npc = regs[O7] + (instr.format_call.disp30 << 2);
	 break;
     case 0x00:		/* Bicc, FBfcc, CBccc, SETHI */
         switch (instr.format_sethi.op2)
           {
         case 0x4:	/* SETHI */
             R_RD = instr.format_sethi.imm22 << 10;
             regs[G0] = 0;
             UPDATE_PC;
             break;
         case 0x02:	/* Bicc */
             switch (instr.format_branch.cond & 0x7)
               {
             case 0x00:	/* bn */
                 flag = 0;
                 break;
             case 0x01:	/* be */
                 flag = psr.z;
                 break;
             case 0x02: /* ble */
                 flag = psr.z || (psr.n ^ psr.v);
                 break;
             case 0x03: /* bl */
                 flag = psr.n ^ psr.v;
                 break;
             case 0x04: /* bleu */
                 flag = psr.c || psr.z;
                 break;
             case 0x05: /* bcs */
                 flag = psr.c;
                 break;
             case 0x06: /* bneg */
                 flag = psr.n;
                 break;
             case 0x07: /* bvs */
                 flag = psr.v;
                 break;
             default:
               abort();
               }
             if (instr.format_branch.cond & 0x8)
               flag = !flag;
             if (flag)
               {
                 out = npc;
                 npc = pc + (instr.format_branch.disp22 << 2);
                 pc = out;
               }
             else
               {
                 if (instr.format_branch.a)
                   {
                     pc = npc + 4;
                     npc = npc + 8;
                   }
                 else
                   {
                     pc = npc;
                     npc += 4;
                   }
               }
             /* ba,a => annul */
             if (instr.format_branch.cond == 0x8 && instr.format_branch.a)
               {
                 pc = npc;
                 npc += 4;
               }
             break;
         case 0x06:	/* fBfcc */
             switch (instr.format_branch.cond & 0x7)
               {
             case 0x00:	/* fbn */
                 flag = 0;
                 break;
             case 0x01:	/* fbne */
                 flag = fpsr.fcc != 0;
                 break;
             case 0x02: /* fblg */
                 flag = fpsr.fcc == 1 || fpsr.fcc == 2;
                 break;
             case 0x03: /* fbul */
                 flag = fpsr.fcc == 1 || fpsr.fcc == 3;
                 break;
             case 0x04: /* fbl */
                 flag = fpsr.fcc == 1;
                 break;
             case 0x05: /* fbug */
                 flag = fpsr.fcc == 2 || fpsr.fcc == 3;
                 break;
             case 0x06: /* fbg */
                 flag = fpsr.fcc == 2;
                 break;
             case 0x07: /* fbu */
                 flag = fpsr.fcc == 3;
                 break;
             default:
               abort();
               }
             if (instr.format_branch.cond & 0x8)
               flag = !flag;
             if (flag)
               {
                 out = npc;
                 npc = pc + (instr.format_branch.disp22 << 2);
                 pc = out;
               }
             else
               {
                 if (instr.format_branch.a)
                   {
                     pc = npc + 4;
                     npc = npc + 8;
                   }
                 else
                   {
                     pc = npc;
                     npc += 4;
                   }
               }
             /* ba,a => annul */
             if (instr.format_branch.cond == 0x8 && instr.format_branch.a)
               {
                 pc = npc;
                 npc += 4;
               }
             break;
         case 0x0:	/* unimp */
         default:
           ILLEGAL_INSTR;
           break;
           }
         break;
     case 0x02:
         if (instr.format_imm.i)
           operand2 = instr.format_imm.imm13;
         else
           operand2 = regs[instr.format_rs2.rs2];
         switch (instr.format_imm.op3)
           {
         case 0x00:	/* add */
             R_RD = R_RS1 + operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x01:	/* and */
             R_RD = R_RS1 & operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x02:	/* or */
             R_RD = R_RS1 | operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x03:	/* xor */
             R_RD = R_RS1 ^ operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x04:	/* sub */
             R_RD = R_RS1 - operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x05:	/* andn */
             R_RD = R_RS1 & ~operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x06:	/* orn */
             R_RD = R_RS1 | ~operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x07:	/* xnor */
             R_RD = R_RS1 ^ ~operand2;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x08:	/* addx */
             R_RD = R_RS1 + operand2 + psr.c;
             regs[0] = 0;
             UPDATE_PC;
             break;
	 case 0x0a:     /* umul */
	     umul (R_RS1, operand2, &y, &R_RD);
	     UPDATE_PC;
	     break;
	 case 0x0b:     /* smul */
	     smul (R_RS1, operand2, &y, &R_RD);
	     UPDATE_PC;
	     break;
         case 0x0c:	/* subx */
             R_RD = R_RS1 - operand2 - psr.c;
             regs[0] = 0;
             UPDATE_PC;
             break;
	 case 0x0e:     /* udiv */
	     {
	       unsigned long long dividend;
	       dividend = y;
	       dividend = dividend << 32 | R_RS1;
	       udiv (dividend, operand2, &R_RD);
	     }
	     UPDATE_PC;
	     break;
	 case 0x0f:     /* sdiv */
	     {
	       unsigned long long dividend;
	       int ov;
	       int sign = 0;
	       dividend = y;
	       dividend = dividend << 32 | R_RS1;
	       if (C31 (y))
		 {
		   sign = ~sign;
		   dividend = -dividend;
		 }
	       if (C31 (operand2))
		 {
		   sign = ~sign;
		   operand2 = -operand2;
		 }
	       ov = udiv (dividend, operand2, &R_RD);
	       if (ov)
	         R_RD = 0x7fffffff;
	       if (sign)
		 R_RD = -R_RD;
	     }
	     UPDATE_PC;
	     break;
         case 0x10:	/* addcc */
             out = R_RS1 + operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = (C31(R_RS1) && C31(operand2) && !C31(out))
                  || (!C31(R_RS1) && !C31(operand2) && C31(out));
             psr.c = (C31(R_RS1)  && C31(operand2))
                  || (!C31(out) && (C31(R_RS1) || C31(operand2)));
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x11:	/* andcc */
             out = R_RS1 & operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = 0;
             psr.c = 0;
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x12:	/* orcc */
             out = R_RS1 | operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = 0;
             psr.c = 0;
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x13:	/* xorcc */
             out = R_RS1 ^ operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = 0;
             psr.c = 0;
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x14:	/* subcc */
             out = R_RS1 - operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = (C31(R_RS1) && !C31(operand2) && !C31(out))
                  || (!C31(R_RS1) && C31(operand2) && C31(out));
             psr.c = (!C31(R_RS1) && C31(operand2))
                  || (C31(out) && (!C31(R_RS1) || C31(operand2)));
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x15:	/* andncc */
             out = R_RS1 & ~operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = 0;
             psr.c = 0;
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x16:	/* orncc */
             out = R_RS1 | ~operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = 0;
             psr.c = 0;
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x17:	/* xnorcc */
             out = R_RS1 ^ ~operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = 0;
             psr.c = 0;
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x18:	/* addxcc */
             out = R_RS1 + operand2 + psr.c;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = (C31(R_RS1) && C31(operand2) && !C31(out))
                  || (!C31(R_RS1) && !C31(operand2) && C31(out));
             psr.c = (C31(R_RS1)  && C31(operand2))
                  || (!C31(out) && (C31(R_RS1) || C31(operand2)));
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
	 case 0x1a:     /* umulcc */
	     umul (R_RS1, operand2, &y, &R_RD);
	     psr.n = C31(R_RD) ? 1 : 0;
	     psr.z = R_RD ? 0 : 1;
	     psr.v = 0;
	     psr.c = 0;
	     UPDATE_PC;
	     break;
	 case 0x1b:     /* smulcc */
	     smul (R_RS1, operand2, &y, &R_RD);
	     psr.n = C31(R_RD) ? 1 : 0;
	     psr.z = R_RD ? 0 : 1;
	     psr.v = 0;
	     psr.c = 0;
	     UPDATE_PC;
	     break;
         case 0x1c:	/* subxcc */
             out = R_RS1 - operand2 - psr.c;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v = (C31(R_RS1) && C31(operand2) && !C31(out))
                  || (!C31(R_RS1) && !C31(operand2) && C31(out));
             psr.c = (C31(R_RS1)  && C31(operand2))
                  || (!C31(out) && (C31(R_RS1) || C31(operand2)));
             R_RD = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
	 case 0x1e:     /* udivcc */
	     {
	       unsigned long long dividend;
	       dividend = y;
	       dividend = dividend << 32 | R_RS1;
	       psr.v = udiv (dividend, operand2, &R_RD);
	       psr.c = 0;
	       psr.z = R_RD == 0 ? 1 : 0;
	       psr.n = C31(R_RD) ? 1 : 0;
	     }
	     UPDATE_PC;
	     break;
	 case 0x1f:     /* sdivcc */
	     {
	       unsigned long long dividend;
	       int sign = 0;
	       dividend = y;
	       dividend = dividend << 32 | R_RS1;
	       if (C31 (y))
		 {
		   sign = ~sign;
		   dividend = -dividend;
		 }
	       if (C31 (operand2))
		 {
		   sign = ~sign;
		   operand2 = -operand2;
		 }
	       psr.v = udiv (dividend, operand2, &R_RD);
	       if (psr.v)
	         {
	           R_RD = 0x7fffffff;
	           psr.z = 0;
	           psr.n = sign ? 1 : 0;
	         }
	       else
	         {
	           psr.z = R_RD == 0 ? 1 : 0;
	           psr.n = C31(R_RD) ^ sign ? 1 : 0;
	         }
	       psr.c = 0;
	       if (sign)
		 R_RD = -R_RD;
	     }
	     UPDATE_PC;
	     break;
         case 0x20:	/* taddcc */
             out = R_RS1 + operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v =    (C31(R_RS1) && C31(operand2) && !C31(out))
                     || (!C31(R_RS1) && !C31(operand2) && C31(out))
                     || ((R_RS1 & 0x3) || (operand2 & 0x03));
             psr.c = (C31(R_RS1) && C31(operand2))
                     || (!C31(R_RD) && (C31(R_RS1) || C31(operand2)));
             R_RD = out;
             UPDATE_PC;
             break;
         case 0x21:	/* tsubcc */
             out = R_RS1 - operand2;
             psr.n = C31(out) ? 1 : 0;
             psr.z = out ? 0 : 1;
             psr.v =    (C31(R_RS1) && !C31(operand2) && !C31(out))
                     || (!C31(R_RS1) && C31(operand2) && C31(out))
                     || ((R_RS1 & 0x3) || (operand2 & 0x03));
             psr.c = (!C31(R_RS1) && C31(operand2))
                     || (C31(R_RD) && (!C31(R_RS1) || C31(operand2)));
             R_RD = out;
             UPDATE_PC;
             break;
         case 0x22:	/* TADDccTV */
             ILLEGAL_INSTR;
             break;
         case 0x23:	/* tsubccTV */
             ILLEGAL_INSTR;
             break;
         case 0x24:	/* mulscc */
             {
               uint op1,op2;
               op1 =  ((psr.n ^ psr.v) ? 0x80000000 : 0) | ((R_RS1 & 0xfffffffe) >> 1);
               if ((y & 1) == 0)
                 op2 = 0;
               else
                 op2 = operand2;
               out = op1 + op2;
               y =  ((R_RS1 & 1) << 31) | ((y & 0xfffffffe) >> 1);
               psr.n = C31(out) ? 1 : 0;
               psr.z = out ? 0 : 1;
               psr.v =    (C31(op1) && C31(op2) && !C31(out))
                       || (!C31(op1) && !C31(op2) && C31(out));
               psr.c =    (C31(op1) && C31(op2)) 
                       || (!C31(out) && (C31(op1) || C31(op2)));
               regs[0] = 0;
               R_RD = out;
               UPDATE_PC;
             }
             break;
         case 0x25:	/* sll */
             R_RD = R_RS1 << (operand2 & 0x1f);
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x26:	/* srl */
             R_RD = R_RS1 >> (operand2 & 0x1f);
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x27:	/* sra */
             if (C31(R_RS1))
               out = ~(0xffffffff >> (operand2 & 0x1f));
             else
               out = 0;
             R_RD = (R_RS1 >> (operand2 & 0x1f)) | out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x28:	/* rdy */
             R_RD = y;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x29:	/* rdpsr */
             ILLEGAL_INSTR;
             break;
         case 0x2a:	/* rdwim */
             ILLEGAL_INSTR;
             break;
         case 0x2b:	/* rdtbr */
             ILLEGAL_INSTR;
             break;
         case 0x30:	/* wry */
             y = R_RS1 ^ operand2;
             UPDATE_PC;
             break;
         case 0x31:	/* wrpsr */
             ILLEGAL_INSTR;
             break;
         case 0x32:	/* wrwim */
             ILLEGAL_INSTR;
             break;
         case 0x33:	/* wrtbr */
             ILLEGAL_INSTR;
             break;
         case 0x34:
             switch (instr.format_fp.opf)
               {
             case 0x001:	/* fmovs */
               fregs.s[RD] = fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x005:	/* fnegs */
               fregs.i[RD] = fregs.i[RS2] ^ 0x80000000;
               UPDATE_PC;
               break;
             case 0x009:	/* fabs */
               fregs.i[RD] = fregs.i[RS2] & 0x7fffffff;
               UPDATE_PC;
               break;
             case 0x029:	/* fsqrts */
               fregs.d[RD] = sqrt_float (fregs.s[RS2]);
               UPDATE_PC;
               break;
             case 0x02a:	/* fsqrtd */
               fregs.d[RD >> 1] = sqrt_double (fregs.d[RS2 >> 1]);
               UPDATE_PC;
               break;
             case 0x02b:	/* fsqrtx *//* FIXME */
               fregs.d[RD >> 2] = sqrt_double (fregs.d[RS2 >> 2]);
               UPDATE_PC;
               break;
             case 0x041:	/* fadds */
               fregs.s[RD] = fregs.s[RS1] + fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x042:	/* faddd */
               fregs.d[RD >> 1] = fregs.d[RS1 >> 1] + fregs.d[RS2 >> 1];
               UPDATE_PC;
               break;
             case 0x043:	/* faddx */
               fregs.x[RD >> 2] = fregs.x[RS1 >> 2] + fregs.x[RS2 >> 2];
               UPDATE_PC;
               break;
             case 0x045:	/* fsubs */
               fregs.s[RD] = fregs.s[RS1] - fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x046:	/* fsubd */
               fregs.d[RD >> 1] = fregs.d[RS1 >> 1] - fregs.d[RS2 >> 1];
               UPDATE_PC;
               break;
             case 0x047:	/* fsubx */
               fregs.x[RD >> 2] = fregs.x[RS1 >> 2] - fregs.x[RS2 >> 2];
               UPDATE_PC;
               break;
             case 0x049:	/* fmuls */
               fregs.s[RD] = fregs.s[RS1] * fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x04a:	/* fmuld */
               fregs.d[RD >> 1] = fregs.d[RS1 >> 1] * fregs.d[RS2 >> 1];
               UPDATE_PC;
               break;
             case 0x04b:	/* fmulx */
               fregs.x[RD >> 2] = fregs.x[RS1 >> 2] * fregs.x[RS2 >> 2];
               UPDATE_PC;
               break;               
             case 0x04d:	/* fdivs */
               fregs.s[RD] = fregs.s[RS1] / fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x04e:	/* fdivd */
               fregs.d[RD >> 1] = fregs.d[RS1 >> 1] / fregs.d[RS2 >> 1];
               UPDATE_PC;
               break;
             case 0x04f:	/* fdivx */
               fregs.x[RD >> 2] = fregs.x[RS1 >> 2] / fregs.x[RS2 >> 2];
               UPDATE_PC;
               break;
             case 0x0c4:	/* fitos */
               fregs.s[RD] = (float)fregs.i[RS2];
               UPDATE_PC;
               break;
             case 0x0c6:	/* fdtos */
               fregs.s[RD] = (float)fregs.d[RS2 >> 1];
               UPDATE_PC;
               break;
             case 0x0c7:	/* fxtos */
               fregs.s[RD] = (float)fregs.x[RS2 >> 2];
               UPDATE_PC;
               break;
             case 0x0c8:	/* fitod */
               fregs.d[RD >> 1] = (double)fregs.i[RS2];
               UPDATE_PC;
               break;
             case 0x0c9:	/* fstod */
               fregs.d[RD >> 1] = (double)fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x0cb:	/* fxtod */
               fregs.d[RD >> 1] = (double)fregs.x[RS2 >> 2];
               UPDATE_PC;
               break;
             case 0x0cc:	/* fitox */
               fregs.x[RD >> 2] = (long double)fregs.i[RS2];
               UPDATE_PC;
               break;
             case 0x0cd:	/* fstox */
               fregs.x[RD >> 2] = (long double)fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x0ce:	/* fdtox */
               fregs.x[RD >> 2] = (long double)fregs.d[RS2 >> 1];
               UPDATE_PC;
               break;
             case 0x0d1:	/* fstoi */
               fregs.i[RD] = (uint)fregs.s[RS2];
               UPDATE_PC;
               break;
             case 0x0d2:	/* fdtoi */
               fregs.i[RD] = (uint)fregs.d[RS2 >> 1];
               UPDATE_PC;
               break;
             case 0x0d3:	/* fxtoi */
               fregs.i[RD] = (uint)fregs.x[RS2 >> 2];
               UPDATE_PC;
               break;
             default:
               ILLEGAL_INSTR;
               break;
               }
             break;
         case 0x35:
             switch (instr.format_fp.opf)
               {
             case 0x051:	/* fcmps */
               {
                 float sous;
                 sous = fregs.s[RS1] - fregs.s[RS2];
                 if (isnan_float(sous))
                   fpsr.fcc = 3;
                 else if (sous == 0)
                   fpsr.fcc = 0;
                 else if (sous < 0)
                   fpsr.fcc = 1;
                 else
                   fpsr.fcc = 2;
                 UPDATE_PC;
               }
               break;
             case 0x052:	/* fcmpd */
               {
                 double sous;
                 sous = fregs.d[RS1 >> 1] - fregs.d[RS2 >> 1];
                 if (isnan_double (sous))
                   fpsr.fcc = 3;
                 else if (sous == 0)
                   fpsr.fcc = 0;
                 else if (sous < 0)
                   fpsr.fcc = 1;
                 else
                   fpsr.fcc = 2;
                 UPDATE_PC;
               }
               break;
             case 0x053:	/* fcmpx */
               {
                 long double sous;
                 sous = fregs.s[RS1 >> 2] - fregs.s[RS2 >> 2];
                 if (isnan_extended (sous))
                   fpsr.fcc = 3;
                 else if (sous == 0)
                   fpsr.fcc = 0;
                 else if (sous < 0)
                   fpsr.fcc = 1;
                 else
                   fpsr.fcc = 2;
                 UPDATE_PC;
               }
               break;
             case 0x055:	/* fcmpes */
               {
                 float sous;
                 sous = fregs.s[RS1] - fregs.s[RS2];
                 if (isnan_float(sous))
                   abort();
                 else if (sous == 0)
                   fpsr.fcc = 0;
                 else if (sous < 0)
                   fpsr.fcc = 1;
                 else
                   fpsr.fcc = 2;
                 UPDATE_PC;
               }
               break;
             case 0x056:	/* fcmped */
               {
                 double sous;
                 sous = fregs.d[RS1 >> 1] - fregs.d[RS2 >> 1];
                 if (isnan_double(sous))
                   abort();
                 else if (sous == 0)
                   fpsr.fcc = 0;
                 else if (sous < 0)
                   fpsr.fcc = 1;
                 else
                   fpsr.fcc = 2;
                 UPDATE_PC;
               }
               break;
             case 0x057:	/* fcmpex */
               {
                 long double sous;
                 sous = fregs.s[RS1 >> 2] - fregs.s[RS2 >> 2];
                 if (isnan_extended(sous))
                   abort();
                 else if (sous == 0)
                   fpsr.fcc = 0;
                 else if (sous < 0)
                   fpsr.fcc = 1;
                 else
                   fpsr.fcc = 2;
                 UPDATE_PC;
               }
               break;

             default:
               ILLEGAL_INSTR;
               }
             break;
         case 0x38:	/* jmpl */
             out = R_RS1 + operand2;
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align((PTR)out, 4, 4);
	     chkr_check_exec((PTR)out);
	     chkr_in_checker = 0;
#endif
             R_RD = pc;
             pc = npc;
             npc = out;
             regs[0] = 0;
             break;
         case 0x39:	/* rett */
             ILLEGAL_INSTR;
             break;
         case 0x3a:	/* ticc */
             switch (instr.format_imm.rd & 0x7)
               {
             case 0x00:	/* tn */
                 flag = 0;
                 break;
             case 0x01:	/* te */
                 flag = psr.z;
                 break;
             case 0x02: /* tle */
                 flag = psr.z || (psr.n ^ psr.v);
                 break;
             case 0x03: /* tl */
                 flag = psr.n ^ psr.v;
                 break;
             case 0x04: /* tleu */
                 flag = psr.c || psr.z;
                 break;
             case 0x05: /* tcs */
                 flag = psr.c;
                 break;
             case 0x06: /* tneg */
                 flag = psr.n;
                 break;
             case 0x07: /* tvs */
                 flag = psr.v;
                 break;
             default:
                 abort();
               }
             if (instr.format_imm.rd & 0x8)
               flag = !flag;
             if (flag)
               do_trap(operand2);
             if (do_not_update_pc_after_trap)
               do_not_update_pc_after_trap = 0;
             else
               {
                 UPDATE_PC;
               }
             break;
         case 0x3b:	/* iflush */
             UPDATE_PC;
             break;
         case 0x3c:	/* save */
             out = R_RS1 + operand2;
             sim_memcpy((uint*)regs[SP], &regs[L0], 16 * sizeof(uint));
             sim_memcpy(&regs[I0], &regs[O0], 8 * sizeof(uint));
             R_RD = out;
#if 0
             sim_printf ("in: %08x %08x %08x %08x %08x %08x %08x %08x\n",
               regs[I0], regs[I1], regs[I2], regs[I3], regs[I4], regs[I5], regs[I6], regs[I7]);
#endif
             UPDATE_PC;
#ifdef DISASSEMBLER
             disassemble_level--;
#endif
             break;
         case 0x3d:	/* restore */
             out = R_RS1 + operand2;
             sim_memcpy(&regs[O0], &regs[I0], 8 * sizeof(uint));
             sim_memcpy(&regs[L0], (uint*)regs[SP], 8 * sizeof(uint));
             sim_memcpy(&regs[I0], (uint*)(regs[SP] + 8 * sizeof(uint)), 8 * sizeof(uint));
#if 0
             sim_printf ("in: %08x %08x %08x %08x %08x %08x %08x %08x\n",
               regs[I0], regs[I1], regs[I2], regs[I3], regs[I4], regs[I5], regs[I6], regs[I7]);
             sim_printf ("lo: %08x %08x %08x %08x %08x %08x %08x %08x\n",
               regs[L0], regs[L1], regs[L2], regs[L3], regs[L4], regs[L5], regs[L6], regs[L7]);
             sim_printf ("ou: %08x %08x %08x %08x %08x %08x %08x %08x\n",
               regs[O0], regs[O1], regs[O2], regs[O3], regs[O4], regs[O5], regs[O6], regs[O7]);
#endif
             R_RD = out;
             UPDATE_PC;
#ifdef DISASSEMBLER
             disassemble_level++;
#endif
             break;
         default:
             ILLEGAL_INSTR;
             break;
           }
         break;
     case 0x03:
         if (instr.format_imm.i)
           operand2 = instr.format_imm.imm13;
         else
           operand2 = regs[instr.format_rs2.rs2];
         switch (instr.format_imm.op3)
           {
         case 0x00:	/* ld */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 4, 4);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 4, CHKR_RO);
	     chkr_in_checker = 0;
#endif
             R_RD = *((uint*)(R_RS1 + operand2));
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x01:	/* ldub */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
	     chkr_check_addr((PTR)(R_RS1 + operand2), 1, CHKR_RO);
	     chkr_in_checker = 0;
#endif
             R_RD = *((uchar_t*)(R_RS1 + operand2));
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x02:	/* lduh */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 2, 2);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 2, CHKR_RO);
	     chkr_in_checker = 0;
#endif
             R_RD = *((ushort*)(R_RS1 + operand2));
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x03:	/* ldd */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 8, 8);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 8, CHKR_RO);
	     chkr_in_checker = 0;
#endif
             /* Avoid ldd %o0, 123, %o0 (... :-).  */
             {
               uint tmp;
               tmp = *((uint*)(R_RS1 + operand2));
               regs[instr.format_imm.rd + 1] = *((uint*)(R_RS1 + operand2 + 4));
               R_RD = tmp;
             }
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x04:	/* st */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 4, 4);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 4, CHKR_WO);
	     chkr_in_checker = 0;
#endif
             *((uint*)(R_RS1 + operand2)) = R_RD;
             UPDATE_PC;
             break;
         case 0x05:	/* stb */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
	     chkr_check_addr((PTR)(R_RS1 + operand2), 1, CHKR_WO);
	     chkr_in_checker = 0;
#endif
             *((char*)(R_RS1 + operand2)) = (char)R_RD;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x06:	/* sth */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 2, 2);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 2, CHKR_WO);
	     chkr_in_checker = 0;
#endif
             (signed int)*((short*)(R_RS1 + operand2)) = R_RD;
             UPDATE_PC;
             break;
         case 0x07:	/* std */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 8, 8);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 8, CHKR_WO);
	     chkr_in_checker = 0;
#endif
             *((uint*)(R_RS1 + operand2)) = R_RD;
             *((uint*)(R_RS1 + operand2 + 4)) = regs[instr.format_imm.rd + 1];
             UPDATE_PC;
             break;
         case 0x09:	/* ldsb */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
	     chkr_check_addr((PTR)(R_RS1 + operand2), 1, CHKR_RO);
	     chkr_in_checker = 0;
#endif
             R_RD = (signed int)*((char*)(R_RS1 + operand2));
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x0a:	/* ldsh */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 2, 2);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 2, CHKR_RO);
	     chkr_in_checker = 0;
#endif         
             R_RD = (signed int)*((short*)(R_RS1 + operand2));
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x0d:	/* ldstub */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
	     chkr_check_addr((PTR)(R_RS1 + operand2), 1, CHKR_RW);
	     chkr_in_checker = 0;
#endif
             R_RD = *((uchar_t*)(R_RS1 + operand2));
             *((uchar_t*)(R_RS1 + operand2)) = 0xff;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x0f:	/* swap */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 4, 4);
	     chkr_check_addr((PTR)(R_RS1 + operand2), 4, CHKR_RW);
	     chkr_in_checker = 0;
#endif
             out = R_RD;
             R_RD = *((uint*)(R_RS1 + operand2));
             *((uint*)(R_RS1 + operand2)) = out;
             regs[0] = 0;
             UPDATE_PC;
             break;
         case 0x10:	/* lda */
             ILLEGAL_INSTR;
             break;
         case 0x11:	/* lduba */
             ILLEGAL_INSTR;
             break;
         case 0x12:	/* ldsha */
             ILLEGAL_INSTR;
             break;
         case 0x13:	/* ldda */
             ILLEGAL_INSTR;
             break;
         case 0x14:	/* sta */
             ILLEGAL_INSTR;
             break;
         case 0x15:	/* stba */
             ILLEGAL_INSTR;
             break;
         case 0x16:	/* stha */
             ILLEGAL_INSTR;
             break;
         case 0x17:	/* stda */
             ILLEGAL_INSTR;
             break;
         case 0x19:	/* ldsba */
             ILLEGAL_INSTR;
             break;
         case 0x1a:	/* ldsha */
             ILLEGAL_INSTR;
             break;
         case 0x1d:	/* ldstuba */
             ILLEGAL_INSTR;
             break;
         case 0x1f:	/* swapa */
             ILLEGAL_INSTR;
             break;
         case 0x20:	/* ldf */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 4, 4);
	     chkr_check_addr((PTR)(R_RS1 + operand2), sizeof(float), CHKR_RO);
	     chkr_in_checker = 0;
#endif
             fregs.s[RD] = *((float*)(R_RS1 + operand2));
             UPDATE_PC;
             break;
         case 0x21:	/* ldfsr */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 4, 4);
	     chkr_check_addr((PTR)(R_RS1 + operand2), sizeof(uint), CHKR_RO);
	     chkr_in_checker = 0;
#endif
             fpsr = *((struct fpsr*)(R_RS1 + operand2));
             UPDATE_PC;
             break;
         case 0x23:	/* lddf */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 8, 8);
	     chkr_check_addr((PTR)(R_RS1 + operand2), sizeof (double), CHKR_RO);
	     chkr_in_checker = 0;
#endif
             fregs.d[RD >> 1] = *((double*)(R_RS1 + operand2));
             UPDATE_PC;
             break;
         case 0x24:	/* stf */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 4, 4);
	     chkr_check_addr((PTR)(R_RS1 + operand2), sizeof(float), CHKR_WO);
	     chkr_in_checker = 0;
#endif
             *((float*)(R_RS1 + operand2)) = fregs.s[RD];
             UPDATE_PC;
             break;
         case 0x25:	/* stfsr */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 4, 4);
	     chkr_check_addr((PTR)(R_RS1 + operand2), sizeof(uint), CHKR_WO);
	     chkr_in_checker = 0;
#endif
             *((uint*)(R_RS1 + operand2)) = *((uint*)&fpsr);
             UPDATE_PC;
             break;
         case 0x26:	/* stdfq */
             ILLEGAL_INSTR;
             break;
         case 0x27:	/* stdf */
#ifdef SIMCHECKER
             chkr_in_checker = 1;
             chkr_check_align ((PTR)(R_RS1 + operand2), 8, 8);
	     chkr_check_addr((PTR)(R_RS1 + operand2), sizeof(double), CHKR_WO);
	     chkr_in_checker = 0;
#endif
             *((double*)(R_RS1 + operand2)) = fregs.d[RD >> 1];
             UPDATE_PC;
             break;
         case 0x30:	/* ldc */
             ILLEGAL_INSTR;
             break;
         case 0x31:	/* ldcsr */
             ILLEGAL_INSTR;
             break;
         case 0x33:	/* lddc */
             ILLEGAL_INSTR;
             break;
         case 0x34:	/* stc */
             ILLEGAL_INSTR;
             break;
         case 0x35:	/* stcsr */
             ILLEGAL_INSTR;
             break;
         case 0x36:	/* stdcq */
             ILLEGAL_INSTR;
             break;
         case 0x37:	/* stdc */
             ILLEGAL_INSTR;
             break;
         default:
             ILLEGAL_INSTR;
             break;
           }
         break;
     default:
         ILLEGAL_INSTR;
         break;
       }
   }
}

int skip_syscall_flag;
void skip_syscall (void);
void make_syscall (void);

void
skip_syscall (void)
{
 skip_syscall_flag = 1;
}

void
make_syscall (void)
{
  int flags = 0;
  asm volatile (
        "ld [%1 + 32], %%o0\n\t"
        "ld [%1 + 36], %%o1\n\t"
        "ld [%1 + 40], %%o2\n\t"
        "ld [%1 + 44], %%o3\n\t"
        "ld [%1 + 48], %%o4\n\t"
        "ld [%1 + 52], %%o5\n\t"
        "ld [%1 +  4], %%g1\n\t"
        "ta 8\n\t"
        "st %%o0, [%1 + 32]\n\t"
        "st %%o1, [%1 + 36]\n\t"
        "st %%o2, [%1 + 40]\n\t"
        "st %%o3, [%1 + 44]\n\t"
        "st %%o4, [%1 + 48]\n\t"
        "st %%o5, [%1 + 52]\n\t"
        "be,a 1f\n\t"
        "or %0, 1, %0\n\t"
        "1: bcs,a 2f\n\t"
        "or %0, 2, %0\n\t"
        "2: bneg,a 3f\n\t"
        "or %0, 4, %0\n\t"
        "3: bvs,a 4f\n\t"
        "or %0, 8, %0\n\t"
        "4: nop\n\t"
        : "=r"(flags)
        : "r" (&regs), "0"(flags)
        : "o0", "o1", "o2", "o3", "o4", "o5", "g1");
  psr.z = (flags & 1) ? 1 : 0;
  psr.c = (flags & 2) ? 1 : 0;
  psr.n = (flags & 4) ? 1 : 0;
  psr.v = (flags & 8) ? 1 : 0;
  
  /* Never again ... */
  skip_syscall_flag = 1;
}

void
do_trap (int num)
{
  switch (num)
    {
  case 1:	/* Breakpoint */
      sim_stop = 1;
      sim_trap = SIGTRAP;
      do_not_update_pc_after_trap = 1;
#if 0
      chkr_printf ("Breakpoint at pc=0x%08x npc=0x%08x\n", pc, npc);
      asm volatile ("ta 1\n\t");
#endif
      break;
  case 8:	/* Syscall */
      chkr_in_checker = 1;
      if (flag_verbose)
        chkr_printf ("Syscall: %s (arg1 = 0x%08x)\n", sysnames[regs[G1]], regs[O0]);
      skip_syscall_flag = 0;
      check_syscall (regs);
      if (!skip_syscall_flag)
        make_syscall ();
      chkr_in_checker = 0;
      break;
  case 3:	/* Save windows. Always done. */
      break;
  case 0x24:	/* See <sys/traps.h> */
  case 0x25:
  case 0x27:
      break;
#ifdef SIMCHECKER
  case 121:
      do_trap_121 ();
      break;
#endif
  default:
      sim_printf ("Trap %d\n", num);
      abort();
    }
}

#if 0
static const char count_tab[] =
{
  0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
};

#define find_last_set(x, res)			\
{						\
  if ((x & 0x0000ffffUL) == x)			\
    {						\
      if ((x & 0x000000ffUL) == x)		\
        res = count_tab[x];			\
      else					\
        res = 8 + count_tab[x >> 8];		\
    }						\
  else						\
    {						\
      if (x & 0x00ffffffUL) == x)		\
        res = 16 + count_tab[x >> 16];		\
      else					\
        res = 24 + count_tab[x >> 24];		\
    }						\
}

#define lfind_last_set(x, res)			\
do						\
  {						\
    uint i;					\
    if ((x & 0x00000000ffffffffULL) == x)	\
      {						\
        i = x & 0x00000000ffffffffULL;		\
        find_last_set (i, res);			\
      }						\
    else					\
      {						\
        i = x >> 32;				\
        find_last_set (i, res);			\
        res += 32;				\
      }						\
  }
#endif
  
static uint
udiv (unsigned long long dividend, uint divi, uint *quotient)
{
  unsigned long long divisor = divi;
  int shift;
  uint res;
  
  if (divi == 0)
    return 1 / divi;

  if ((long long)dividend < 0)
    {
      for (shift = 0; (long long)divisor > 0; shift++)
        divisor <<= 1;
      if (divisor > dividend)
        divisor >>= 1;
    }
  else
    {
      for (shift = 0; (divisor << 1) <= dividend; shift++)
        divisor <<= 1;
    }
  if (shift > 32)
    {
      *quotient = 0xffffffffUL;
      return 1;
    }
  for (res = 0; shift >= 0; shift--)
    {
      res <<= 1;
      if (divisor <= dividend)
	{
	  res |= 1;
	  dividend -= divisor;
	}
      divisor >>= 1;
    }
  *quotient = res;
  return 0;
}

static void
umul (uint a, uint b, uint *high, uint *low)
{
  unsigned short ah, al, bh, bl;
  unsigned long long res;
  al = a & 0xffff;
  ah = a >> 16;
  bl = b & 0xffff;
  bh = b >> 16;
  res = al*bl + ((unsigned long long)(ah*bl + al*bh) << 16)
    + ((unsigned long long)(ah*bh) << 32);
  *low = (uint)(res & 0xffffffff);
  *high = (uint)(res >> 32);
}

static void
smul (int a, int b, uint *high, uint *low)
{
  unsigned short ah, al, bh, bl;
  signed long long res;
  int sign = 0;
  if (a < 0)
    {
      sign = ~sign;
      a = -a;
    }
  if (b < 0)
    {
      sign = ~sign;
      b = -b;
    }
  al = a & 0xffff;
  ah = a >> 16;
  bl = b & 0xffff;
  bh = b >> 16;
  res = al*bl + ((unsigned long long)(ah*bl + al*bh) << 16)
    + ((unsigned long long)(ah*bh) << 32);
  if (sign)
    res = -res;
  *low = (uint)(res & 0xffffffff);
  *high = (uint)(res >> 32);
}
