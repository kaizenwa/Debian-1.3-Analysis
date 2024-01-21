/* Single stepper for Sparc Solaris2.
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

#include <sys/types.h>
#include <sys/procfs.h>
#include <sys/syscall.h>
#include <sys/ucontext.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include "instr.h"

void child(char *prog[]);
void disp_prstatus(prstatus_t *pr);
void disassemble(union Instr instr);
extern char *sysnames[];
int flag_verbose = 0;
int sstep_flag = 0;
int disassemble_level = 1;
int no_dis_level = 0;
char *progname;
int file;

FILE *out;

uint pc;
uint npc;
struct psr psr;
uint regs[32];
uint y;
/* Registers of the Floating Point Unit. */
union Fregs fregs;

/* Floating Point Status Register. */
struct fpsr fpsr;

void disp_syscall (prstatus_t *pr);

void
usage (void)
{
  fprintf (stderr, "Usage: %s [-v] [-S] [-d] [-n] [-o file] prog [args]\n", progname);
  fprintf (stderr, "-S      single step\n");
  fprintf (stderr, "-d      disassemble level\n");
  fprintf (stderr, "-n      no disassemble level\n");
  exit (2);
}

int
main (int argc, char *argv[])
{
 int pid;
 char name[40];
 int retval;
 prrun_t prrun;
 prstatus_t prstatus;
 int c;
 sysset_t execset;
 fltset_t fault;
 int flag_time = 0;
 
 progname = argv[0];
 out = stdout;
 
 while ((c = getopt (argc, argv, "dvSo:nt")) != EOF)
   switch (c)
     {
     case 'v':
       flag_verbose++;
       break;
     case 'S':
       sstep_flag = 1;
       break;
     case 'd':
       disassemble_level++;
       break;
     case 'n':
       no_dis_level = 1;
       break;
     case 't':
       flag_time = 1;
       break;
     case 'o':
       if (out != stdout)
         {
           fprintf (stderr, "-o deja utilisee.\n");
           break;
         }
       out = fopen (optarg, "w");
       if (out == NULL)
         {
           fprintf (stderr, "Impossible d'ouvrir `%s'\n", optarg);
           out = stdout;
         }
       break;
     case '?':
        usage ();
     }
 
 if (optind >= argc)
   usage ();
   
 pid = fork ();
 if (pid == 0)
   child (&argv[optind]);
   
 sprintf (name, "/proc/%05d", pid);
 file = open(name, O_RDWR);
 if (flag_verbose)
   perror ("open");

 /* Stop as soon as possible the child.  */
 ioctl (file, PIOCSTOP, &prstatus);
 if (flag_verbose)
    perror ("PIOCSTOP ioctl");
 disp_prstatus (&prstatus);

 /* Set the Kill-On-Last-Close flag. */
 c = PR_KLC | PR_RLC;
 ioctl (file, PIOCSET, &c);
 if (flag_verbose)
   perror ("PIOCSET");

 premptyset (&execset);
 if (flag_time) 
   praddset (&execset, SYS_time);
 else
   {
     praddset (&execset, SYS_exec);
     praddset (&execset, SYS_execve);
   }

 ioctl (file, PIOCSEXIT, &execset);
 perror ("PIOCSEXIT ioctl");
 
 premptyset (&execset);
 /* praddset (&execset, SYS_context); */
 /* praddset (&execset, SYS_close); */
 praddset (&execset, SYS_time);
 /* prfillset (&execset); */
 ioctl (file, PIOCSENTRY, &execset);
 
 premptyset (&fault);
 praddset (&fault, FLTTRACE);
 praddset (&fault, FLTILL);
 ioctl (file, PIOCSFAULT, &fault);
 perror ("PIOCSFAULT ioctl");

 ioctl (file, PIOCRUN,  &prrun);
 if (flag_verbose)
   perror ("ioctl PIOCRUN");
 ioctl (file, PIOCWSTOP, &prstatus);
 if (flag_verbose)
   perror ("ioctl PIOCWSTOP");
 disp_prstatus (&prstatus);

 prfillset (&execset);
 ioctl (file, PIOCSENTRY, &execset);

 ioctl (file, PIOCWSTOP, &prstatus);
 if (flag_verbose)
   perror ("ioctl PIOCWSTOP");
 disp_prstatus (&prstatus);
 
 retval = 0;
 prrun.pr_flags = (sstep_flag ? PRSTEP : 0) | PRCFAULT;
 while (!retval)
   {
     retval = ioctl (file, PIOCRUN, &prrun);
     if (flag_verbose)
       perror ("ioctl PIOCRUN");
     retval = ioctl (file, PIOCWSTOP, &prstatus);
     if (flag_verbose)
       perror ("ioctl PIOCWSTOP");
     disp_prstatus (&prstatus);
   }
 exit(0);
}

void
child (char *prog[])
{
 int pid = getpid ();
 
 fprintf (out, "%d: stopping `%s''\n", pid, prog[0]);
 sleep (1);
 
 fprintf (out, "%d: executing `%s''\n", pid, prog[0]);
#if 0
 putenv ("LD_PRELOAD=/usr/home/tristan/Checker-0.6/tmp/checker.so.1");
#endif
 execv (prog[0], prog);
 fprintf (out, "%d: fail to execute.\n", pid);
 exit (1);
}

struct bit_names
{
  int bit;
  char *name;
};

struct bit_names pr_flags_name[] =
{
  {PR_STOPPED,		"STOPPED"},
  {PR_ISTOP,		"ISTOP"},
  {PR_DSTOP,		"DSTOP"},
  {PR_ASLEEP,		"ASLEEP"},
  {PR_PCINVAL,		"PCINVAL"},
  {PR_ISSYS,		"ISSYS"},
  {PR_PTRACE,		"PTRACE"},
  {PR_FORK,		"FORK"},
  {PR_RLC,		"RLC"},
  {PR_KLC,		"KLC"},
  {PR_ASYNC,		"ASYNC"},
  {PR_PCOMPAT,		"PCOMPAT"},
  {0 , 0}
};

struct bit_names pr_why_name[] =
{
  {PR_REQUESTED,	"REQUESTED"},
  {PR_SIGNALLED,	"SIGNALLED"},
  {PR_FAULTED,		"FAULTED"},
  {PR_SYSENTRY,		"SYSENTRY"},
  {PR_SYSEXIT,		"SYSEXIT"},
  {PR_JOBCONTROL,	"JOBCONTROL"},
  {PR_SUSPENDED,	"SUSPENDED"},
  {0, 0}
};

void
dump_bit_name(long bits, struct bit_names *names)
{
 while (names->name)
   {
     if (bits & names->bit)
       fprintf(out, " %s", names->name);
     names++;
   }
}

void
dump_val_name(long val, struct bit_names *names)
{
 while (names->name)
   {
     if (val == names->bit)
       {
         fprintf(out, " %s", names->name);
         return;
       }
     names++;
   }
}
  
void
disp_prstatus (prstatus_t *pr)
{
  union Instr instr;
  
  if (flag_verbose)
    {
      fprintf (out, "pr_flags: ");
      dump_bit_name (pr->pr_flags, pr_flags_name);
      fprintf (out, "\npr_why: ");
      dump_val_name (pr->pr_why, pr_why_name);
      fprintf (out, "\n");
    }
  switch (pr->pr_why)
    {
  case PR_SIGNALLED:
      fprintf (out, "signal: %d\n", pr->pr_what);
      break;
  case PR_SYSENTRY:
  case PR_SYSEXIT:
      disp_syscall (pr);
      break;
  case PR_FAULTED:
      {
        if (pr->pr_what == FLTILL && pr->pr_instr == 0x91d02079)
          {
            disassemble_level = pr->pr_reg[R_O0];
            ioctl (file, PIOCCFAULT);
            if (flag_verbose)
              perror ("ioctl PIOCCFAULT");
            pr->pr_reg[R_PC] += 4;
            pr->pr_reg[R_nPC] += 4;
            ioctl (file, PIOCSREG, &pr->pr_reg);
            if (flag_verbose)
              perror ("ioctl PIOCSREG");
          }
        if (disassemble_level > 0)
          {
            pc = pr->pr_reg[R_PC];
            npc = pr->pr_reg[R_nPC];
            *((int*)(&psr)) = pr->pr_reg[R_PSR];
            regs[G0] = pr->pr_reg[R_G0];
            regs[G1] = pr->pr_reg[R_G1];
            regs[G2] = pr->pr_reg[R_G2];
            regs[G3] = pr->pr_reg[R_G3];
            regs[G4] = pr->pr_reg[R_G4];
            regs[G5] = pr->pr_reg[R_G5];
            regs[G6] = pr->pr_reg[R_G6];
            regs[G7] = pr->pr_reg[R_G7];
            regs[I0] = pr->pr_reg[R_I0];
            regs[I1] = pr->pr_reg[R_I1];
            regs[I2] = pr->pr_reg[R_I2];
            regs[I3] = pr->pr_reg[R_I3];
            regs[I4] = pr->pr_reg[R_I4];
            regs[I5] = pr->pr_reg[R_I5];
            regs[I6] = pr->pr_reg[R_I6];
            regs[I7] = pr->pr_reg[R_I7];
            regs[L0] = pr->pr_reg[R_L0];
            regs[L1] = pr->pr_reg[R_L1];
            regs[L2] = pr->pr_reg[R_L2];
            regs[L3] = pr->pr_reg[R_L3];
            regs[L4] = pr->pr_reg[R_L4];
            regs[L5] = pr->pr_reg[R_L5];
            regs[L6] = pr->pr_reg[R_L6];
            regs[L7] = pr->pr_reg[R_L7];
            regs[O0] = pr->pr_reg[R_O0];
            regs[O1] = pr->pr_reg[R_O1];
            regs[O2] = pr->pr_reg[R_O2];
            regs[O3] = pr->pr_reg[R_O3];
            regs[O4] = pr->pr_reg[R_O4];
            regs[O5] = pr->pr_reg[R_O5];
            regs[O6] = pr->pr_reg[R_O6];
            regs[O7] = pr->pr_reg[R_O7];
          }
        instr.word = pr->pr_instr;
        if (disassemble_level > 0
            && (   (instr.word & 0xc1f80000) == 0x81a00000
                || (instr.word & 0xc1f80000) == 0x81a80000))
          {
            prfpregset_t fp;
            int i;
            ioctl (file, PIOCGFPREG, &fp);
            if (flag_verbose)
              perror ("ioctl PIOCGFPREG");
            for (i = 0; i < 32; i++)
              fregs.i[i] = fp.pr_fr.pr_regs[i];
            fpsr = *((struct fpsr*)(&fp.pr_fsr));
            print_fpsr ();
          }
        if (disassemble_level > 0)
          {
/*            printf ("0x%08x ", instr.word); */
            disassemble(instr);
          }
	if (!no_dis_level)
	  {
	    if ((instr.word & 0xc1f80000) == 0x81e80000)
	      disassemble_level++;	/* restore */
	    else if ((instr.word & 0xc1f80000) == 0x81e00000)
	      disassemble_level--;	/* save */
	  }
      }
    }
}

void
read_child (off_t offset, void *ptr, int len)
{
  lseek (file, offset, SEEK_SET);
  read (file, ptr, len);
}

void
disp_context (ucontext_t *ucp)
{
 fprintf (out, "uc_flags = 0x%08lx\n", ucp->uc_flags);
 fprintf (out, "uc_link = 0x%08x\n", (int)ucp->uc_link);
 fprintf (out, "uc_stack.ss_sp = 0x%08x\n", (int)ucp->uc_stack.ss_sp);
 fprintf (out, "uc_stack.ss_size = 0x%08x\n", (int)ucp->uc_stack.ss_size); 
 fprintf (out, "uc_stack.ss_flags = 0x%08x\n", (int)ucp->uc_stack.ss_flags);
 fprintf (out, "uc_mcontext.gregs[REG_O6] = 0x%08x\n", ucp->uc_mcontext.gregs[REG_O6]);
 fprintf (out, "uc_mcontext.gregs[REG_PC] = 0x%08x\n", ucp->uc_mcontext.gregs[REG_PC]);
 fprintf (out, "uc_mcontext.gregs[REG_nPC] = 0x%08x\n", ucp->uc_mcontext.gregs[REG_nPC]);
}

void
disp_syscall (prstatus_t *pr)
{
 int i;

 fprintf (out, "syscall: %d %s (", pr->pr_what, sysnames[pr->pr_what]);
 for (i = 0; i < pr->pr_nsysarg; i++)
   {
     if (i > 0)
       fprintf (out, ", ");
     fprintf (out, "0x%08x", (uint)pr->pr_sysarg[i]);
   }
 fprintf (out, ")\n");
 switch (pr->pr_what)
   {
   case SYS_context:
     {
       ucontext_t ucp;
       if (pr->pr_sysarg[0] == 0)
         fprintf (out, "getcontext (0x%08x)\n", (uint)pr->pr_sysarg[1]);
       else if (pr->pr_sysarg[0] == 1)
         fprintf (out, "setcontext (0x%08x)\n", (uint)pr->pr_sysarg[1]);
       read_child (pr->pr_sysarg[1], &ucp, sizeof (ucontext_t));
       disp_context (&ucp);
     }
     break;
   }
}

