/* Remote debugger server for simchecker.
   Copyright (C) 1995 Tristan Gingold.
   This code was adapted from: */
/* Main code for remote server for GDB.
   Copyright (C) 1989, 1993 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define NEED_MM
#include "instr.h"
#include "checker.h"
#include <sys/stropts.h>
#include <sys/file.h>

#define	G0_REGNUM 0             /* %g0 */
#define	G1_REGNUM 1		/* %g1 */
#define O0_REGNUM 8		/* %o0 */
#define	SP_REGNUM 14		/* Contains address of top of stack, \
				   which is also the bottom of the frame.  */
#define	RP_REGNUM 15		/* Contains return address value, *before* \
				   any windows get switched.  */
#define	O7_REGNUM 15		/* Last local reg not saved on stack frame */
#define	L0_REGNUM 16		/* First local reg that's saved on stack frame
				   rather than in machine registers */
#define	I0_REGNUM 24		/* %i0 */
#define	FP_REGNUM 30		/* Contains address of executing stack frame */
#define	I7_REGNUM 31		/* Last local reg saved on stack frame */
#define	FP0_REGNUM 32		/* Floating point register 0 */
#define	Y_REGNUM 64		/* Temp register for multiplication, etc.  */
#define	PS_REGNUM 65		/* Contains processor status */
#define	WIM_REGNUM 66		/* Window Invalid Mask (not really supported) */
#define	TBR_REGNUM 67		/* Trap Base Register (not really supported) */
#define	PC_REGNUM 68		/* Contains program counter */
#define	NPC_REGNUM 69           /* Contains next PC */
#define	FPS_REGNUM 70		/* Floating point status register */
#define	CPS_REGNUM 71		/* Coprocessor status register */

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  On the sparc, `registers'
   contains the ins and locals, even though they are saved on the
   stack rather than with the other registers, and this causes hair
   and confusion in places like pop_frame.  It probably would be
   better to remove the ins and locals from `registers', make sure
   that get_saved_register can get them from the stack (even in the
   innermost frame), and make this the way to access them.  For the
   frame pointer we would do that via TARGET_READ_FP.  */

#define REGISTER_BYTES (32*4+32*4+8*4)

/* Index within `registers' of the first byte of the space for
   register N.  */
/* ?? */
#define REGISTER_BYTE(N)  ((N)*4)

/* On the SPARC, all regs are 4 bytes.  */

#define REGISTER_RAW_SIZE(N) (4)


int gdbserver_desc;
int quit_flag = 0;
extern int sim_trap;
/* KKHHRRAASSSS & STRESSSS  @ * # ... */
char registers[REGISTER_BYTES] __attribute__ ((aligned (4)));

static void
perror (char *mes)
{
 chkr_printf ("gdb_server_sim: errno=%d: %s\n", chkr_errno, mes);
}

/* Convert hex digit A to a number.  */

static int
fromhex (int a)
{
  if (a >= '0' && a <= '9')
    return a - '0';
  else if (a >= 'a' && a <= 'f')
    return a - 'a' + 10;
  else
    {
      chkr_printf ("gdb_server_sim(): Reply contains invalid hex digit\n");
      return -1;
    }
}

/* Convert number NIB to a hex digit.  */

static int
tohex (int nib)
{
  if (nib < 10)
    return '0' + nib;
  else
    return 'a' + nib - 10;
}

/* Send a packet to the remote machine, with error checking.
   The data of the packet is in BUF.  Returns >= 0 on success, -1 otherwise. */

int
putpkt (char *buf)
{
  int i;
  unsigned char csum = 0;
  char buf2[2000];
  char buf3[1];
  int cnt = strlen (buf);
  char *p;

  /* Copy the packet into buffer BUF2, encapsulating it
     and giving it a checksum.  */

  p = buf2;
  *p++ = '$';

  for (i = 0; i < cnt; i++)
    {
      csum += buf[i];
      *p++ = buf[i];
    }
  *p++ = '#';
  *p++ = tohex ((csum >> 4) & 0xf);
  *p++ = tohex (csum & 0xf);

  /* Send it over and over until we get a positive ack.  */

  do
    {
      int cc;

      if (write (gdbserver_desc, buf2, p - buf2) != p - buf2)
	{
	  perror ("putpkt(write)");
	  return -1;
	}

      cc = read (gdbserver_desc, buf3, 1);
      if (cc <= 0)
	{
	  if (cc == 0)
	    {
	      chkr_printf ("putpkt(read): Got EOF\n");
	      _exit (253);
	    }
	  else
	    perror ("putpkt(read)");

	  return -1;
	}
    }
  while (buf3[0] != '+');

  return 1;			/* Success! */
}

#if 0
/* Come here when we get an input interrupt from the remote side.  This
   interrupt should only be active while we are waiting for the child to do
   something.  About the only thing that should come through is a ^C, which
   will cause us to send a SIGINT to the child.  */

static void
input_interrupt (void)
{
  int cc;
  char c;

  cc = read (gdbserver_desc, &c, 1);

  if (cc != 1 || c != '\003')
    {
      fprintf(stderr, "input_interrupt, cc = %d c = %d\n", cc, c);
      return;
    }

  kill (inferior_pid, SIGINT);
}

void
enable_async_io (void)
{
  signal (SIGIO, input_interrupt);
}

void
disable_async_io (void)
{
  signal (SIGIO, SIG_IGN);
}
#endif

/* Returns next char from remote GDB.  -1 if error.  */

static int
readchar (void)
{
  static char buf[1024];
  static int bufcnt = 0;
  static char *bufp;

  if (bufcnt-- > 0)
    return *bufp++ & 0x7f;

  bufcnt = read (gdbserver_desc, buf, sizeof (buf));

  if (bufcnt <= 0)
    {
      if (bufcnt == 0)
        {
	  chkr_printf ("readchar: Got EOF\n");
	  _exit (253);
	}
      else
	perror ("readchar");

      return -1;
    }

  bufp = buf;
  bufcnt--;
  return *bufp++ & 0x7f;
}

/* Read a packet from the remote machine, with error checking,
   and store it in BUF.  Returns length of packet, or negative if error. */

int
getpkt (char *buf)
{
  char *bp;
  unsigned char csum, c1, c2;
  int c;

  while (1)
    {
      csum = 0;

      while (1)
	{
	  c = readchar ();
	  if (c == '$')
	    break;
	  if (c < 0)
	    return -1;
	}

      bp = buf;
      while (1)
	{
	  c = readchar ();
	  if (c < 0)
	    return -1;
	  if (c == '#')
	    break;
	  *bp++ = c;
	  csum += c;
	}
      *bp = 0;

      c1 = fromhex (readchar ());
      c2 = fromhex (readchar ());
      if (csum == (c1 << 4) + c2)
	break;

      chkr_printf ("Bad checksum, sentsum=0x%x, csum=0x%x, buf=%s\n",
	       (c1 << 4) + c2, csum, buf);
      write (gdbserver_desc, "-", 1);
    }

  write (gdbserver_desc, "+", 1);
  return bp - buf;
}

void
write_ok (char *buf)
{
  buf[0] = 'O';
  buf[1] = 'k';
  buf[2] = '\0';
}

void
write_enn (char *buf)
{
  buf[0] = 'E';
  buf[1] = 'N';
  buf[2] = 'N';
  buf[3] = '\0';
}

void
convert_int_to_ascii (char *from, char *to, int n)
{
  int nib;
  char ch;
  while (n--)
    {
      ch = *from++;
      nib = ((ch & 0xf0) >> 4) & 0x0f;
      *to++ = tohex (nib);
      nib = ch & 0x0f;
      *to++ = tohex (nib);
    }
  *to++ = 0;
}


void
convert_ascii_to_int (char *from, char *to, int n)
{
  int nib1, nib2;
  while (n--)
    {
      nib1 = fromhex (*from++);
      nib2 = fromhex (*from++);
      *to++ = (((nib1 & 0x0f) << 4) & 0xf0) | (nib2 & 0x0f);
    }
}

static char *
outreg (int regno, char *buf)
{
  *buf++ = tohex (regno >> 4);
  *buf++ = tohex (regno & 0xf);
  *buf++ = ':';
  convert_int_to_ascii (&registers[REGISTER_BYTE (regno)], buf, 4);
  buf += 8;
  *buf++ = ';';

  return buf;
}

void
prepare_resume_reply (char *buf, unsigned char signal)
{
  int nib;

  *buf++ = 'T';

  nib = ((signal & 0xf0) >> 4);
  *buf++ = tohex (nib);
  nib = signal & 0x0f;
  *buf++ = tohex (nib);

  buf = outreg (PC_REGNUM, buf);
  buf = outreg (FP_REGNUM, buf);
  buf = outreg (SP_REGNUM, buf);
  buf = outreg (NPC_REGNUM, buf);
  buf = outreg (O7_REGNUM, buf);

  *buf++ = 0;
}

void
decode_m_packet (char *from, unsigned int *mem_addr_ptr, unsigned int *len_ptr)
{
  int i = 0, j = 0;
  char ch;
  *mem_addr_ptr = *len_ptr = 0;

  while ((ch = from[i++]) != ',')
    {
      *mem_addr_ptr = *mem_addr_ptr << 4;
      *mem_addr_ptr |= fromhex (ch) & 0x0f;
    }

  for (j = 0; j < 4; j++)
    {
      if ((ch = from[i++]) == 0)
	break;
      *len_ptr = *len_ptr << 4;
      *len_ptr |= fromhex (ch) & 0x0f;
    }
}

void
decode_M_packet (char *from, unsigned int *mem_addr_ptr, unsigned int *len_ptr, char *to)
{
  int i = 0;
  char ch;
  *mem_addr_ptr = *len_ptr = 0;

  while ((ch = from[i++]) != ',')
    {
      *mem_addr_ptr = *mem_addr_ptr << 4;
      *mem_addr_ptr |= fromhex (ch) & 0x0f;
    }

  while ((ch = from[i++]) != ':')
    {
      *len_ptr = *len_ptr << 4;
      *len_ptr |= fromhex (ch) & 0x0f;
    }

  convert_ascii_to_int (&from[i++], to, *len_ptr);
}

#if 0
/* Wait for process, returns status */

unsigned char
mywait (char *status)
{
  int pid;
  union wait w;

  pid = wait (&w);
  if (pid != inferior_pid)
    perror_with_name ("wait");

  if (WIFEXITED (w))
    {
      fprintf (stderr, "\nChild exited with retcode = %x \n", WEXITSTATUS (w));
      *status = 'E';
      return ((unsigned char) WEXITSTATUS (w));
    }
  else if (!WIFSTOPPED (w))
    {
      fprintf (stderr, "\nChild terminated with signal = %x \n", WTERMSIG (w));
      *status = 'T';
      return ((unsigned char) WTERMSIG (w));
    }

  fetch_inferior_registers (0);

  *status = 'S';
  return ((unsigned char) WSTOPSIG (w));
}

/* Resume execution of the inferior process.
   If STEP is nonzero, single-step it.
   If SIGNAL is nonzero, give it that signal.  */

void
myresume (int step, int signal)
{
  errno = 0;
  ptrace (step ? PTRACE_SINGLESTEP : PTRACE_CONT, inferior_pid, 1, signal);
  if (errno)
    perror_with_name ("ptrace");
}
#endif

/* Fetch one or more registers from the inferior.  REGNO == -1 to get
   them all.  We actually fetch more than requested, when convenient,
   marking them as valid so we won't fetch them again.  */

void
fetch_inferior_registers (int ignored)
{
  memcpy (&registers[REGISTER_BYTE (G1_REGNUM)], &regs[G1],
  		7 * REGISTER_RAW_SIZE (G0_REGNUM));
  memcpy (&registers[REGISTER_BYTE (O0_REGNUM)], &regs[O0],
  		8 * REGISTER_RAW_SIZE (G0_REGNUM));
  memcpy (&registers[REGISTER_BYTE (I0_REGNUM)], &regs[I0],
  		8 * REGISTER_RAW_SIZE (G0_REGNUM));
#if 0
  chkr_printf ("&psr: 0x%08x\n", &psr);
  chkr_printf ("psr= 0x%08x\n", *((int*)(&psr)));
  chkr_printf ("registers: 0x%08x\n", registers);
#endif  
  *(int *)&registers[REGISTER_BYTE (PS_REGNUM)] = *((int*)(&psr));
  *(int *)&registers[REGISTER_BYTE (PC_REGNUM)] = pc;
  *(int *)&registers[REGISTER_BYTE (NPC_REGNUM)] = npc;
  *(int *)&registers[REGISTER_BYTE (Y_REGNUM)] = y;
  /* FIXME: fp */
}

/* Store our register values back into the inferior.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

void
store_inferior_registers (int ignored)
{
  memcpy (&regs[G1], &registers[REGISTER_BYTE (G1_REGNUM)],
  		7 * REGISTER_RAW_SIZE (G0_REGNUM));
  memcpy (&regs[O0],&registers[REGISTER_BYTE (O0_REGNUM)],
  		8 * REGISTER_RAW_SIZE (G0_REGNUM));
  memcpy (&regs[I0],&registers[REGISTER_BYTE (I0_REGNUM)],
  		8 * REGISTER_RAW_SIZE (G0_REGNUM));
  psr = *(struct psr *)&registers[REGISTER_BYTE (PS_REGNUM)];
  pc = *(int *)&registers[REGISTER_BYTE (PC_REGNUM)];
  npc = *(int *)&registers[REGISTER_BYTE (NPC_REGNUM)];
  y = *(int *)&registers[REGISTER_BYTE (Y_REGNUM)];
}

/* Copy LEN bytes from inferior's memory starting at MEMADDR
   to debugger memory starting at MYADDR.  */

void
read_inferior_memory (int memaddr, char *myaddr, int len)
{
  memcpy (myaddr, (char *)memaddr, len);
}

/* Copy LEN bytes of data from debugger memory at MYADDR
   to inferior's memory at MEMADDR.
   On failure (cannot write the inferior)
   returns the value of errno.  */

int
write_inferior_memory (int memaddr, char *myaddr, int len)
{
  memcpy ((char *)memaddr, myaddr, len);
  return 0;
}

#if 0
void
initialize (void)
{
  inferior_pid = 0;
}

int
have_inferior_p (void)
{
  return inferior_pid != 0;
}
#endif

void
gdb_server_sim (void)
{
  char ch, status, own_buf[2000], mem_buf[2000];
  int i = 0;
  unsigned int mem_addr, len;

  /* GDB can set breakpoints. */
  mem_addr = objects->org & ~0xfff;
  mprotect ((char*)mem_addr, objects->end - mem_addr,
  	    PROT_READ | PROT_WRITE | PROT_EXEC);
  	    
  /* We are now stopped at the first instruction of the target process */

  while (1)
    {
      fetch_inferior_registers (0);
      sim_trap = SIGTRAP;
      status = 'S';
      while (getpkt (own_buf) > 0)
	{
	  i = 0;
	  ch = own_buf[i++];
	  switch (ch)
	    {
	    case '?':
	      prepare_resume_reply (own_buf, sim_trap);
	      break;
	    case 'g':
	      convert_int_to_ascii (registers, own_buf, REGISTER_BYTES);
	      break;
	    case 'G':
	      convert_ascii_to_int (&own_buf[1], registers, REGISTER_BYTES);
	      store_inferior_registers (-1);
	      write_ok (own_buf);
	      break;
	    case 'm':
	      decode_m_packet (&own_buf[1], &mem_addr, &len);
	      read_inferior_memory (mem_addr, mem_buf, len);
	      convert_int_to_ascii (mem_buf, own_buf, len);
	      break;
	    case 'M':
	      decode_M_packet (&own_buf[1], &mem_addr, &len, mem_buf);
	      if (write_inferior_memory (mem_addr, mem_buf, len) == 0)
		write_ok (own_buf);
	      else
		write_enn (own_buf);
	      break;
	    case 'c':
	      ioctl (gdbserver_desc, I_SETSIG, S_RDNORM);
	      sim (1);
	      ioctl (gdbserver_desc, I_SETSIG, 0);
/*	      signal = mywait (&status); */
              fetch_inferior_registers (0);
	      prepare_resume_reply (own_buf, sim_trap);
	      break;
	    case 's':
	      sim (0);
/*	      myresume (1, 0);
	      signal = mywait (&status); */
	      fetch_inferior_registers (0);
	      prepare_resume_reply (own_buf, sim_trap);
	      break;
	    case 'k':
/*	      fprintf (stderr, "Killing inferior\n"); */
	      _exit (251);
	      break;
	    case 'q':
	      if (!strcmp (&own_buf[i], "Offsets"))
#if 0
	        {
	          char *buf;
	          int offset;
	          strcpy (own_buf, "Text=");
	          buf = &own_buf[5];
	          offset = objects->org;
	          convert_int_to_ascii (&offset, buf, 8);
	          strcpy (buf + 9, ";Data=");
	          buf += 15;
	          offset = objects->next->org;
	          convert_int_to_ascii (&offset, buf, 8);
	          strcpy (buf + 9, ";Bss=");
	          buf += 14;
	          convert_int_to_ascii (&offset, buf, 8);
	          buf[9] = 0;
	        }
#else
		own_buf[0] = 0;
#endif
	      else
	        write_enn (own_buf);
	      break;
	    default:
	      chkr_printf ("gdb_server_sim(): Unknown option `%c' chosen by master\n", ch);
	      write_enn (own_buf);
	      break;
	    }

	  putpkt (own_buf);
	}

#if 0
      /* We come here when getpkt fails.  Close the connection, and re-open it
         at the top of the loop.  */

/*      fprintf (stderr, "Remote side has terminated connection.  GDBserver will reopen the connection.\n"); */

      remote_close ();
#endif
      _exit (252);
    }
}
