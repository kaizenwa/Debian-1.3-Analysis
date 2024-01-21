/*
 * DOSEMU debugger,  1995 Max Parke <mhp@light.lightlink.com>
 *
 * This is file mhpdbg.h
 *
 * changes:
 *
 *   16Sep95 Hans Lermen <lermen@elserv.ffm.fgan.de>
 */

#ifndef MHPDBG_H
#define MHPDBG_H

#include "extern.h"

#if 0  /* now defined in include/vm86plus */
#define VM86_TRAP 4	  /* (vm86 return) TRAP */
#endif

#define DBG_INIT 0
#define DBG_INTx 1
#define DBG_TRAP 2
#define DBG_POLL 3
#define DBG_GPF  4
#define DBG_INTxDPMI 5

unsigned int mhp_debug(unsigned int, unsigned int, unsigned int);
void mhp_send(void);
void mhp_input();
void mhp_close();
void mhp_printf(const char *,...);
int mhp_getaxlist_value(int v, int mask);
 
#define MHP_BUFFERSIZE 8192
struct mhpdbg
{
   unsigned char sendbuf[MHP_BUFFERSIZE];
   unsigned char recvbuf[MHP_BUFFERSIZE];
   int sendptr;
   int nbytes;
   int active;
   int flags;
   
   int fdin,fdout;
};

EXTERN struct mhpdbg mhpdbg;


#ifdef MHP_PRIVATE

#define DBG_TYPE(val)	       ((val) & 0xff)
#define DBG_ARG(val)	       ((val) >> 8)
#define SRSIZE MHP_BUFFERSIZE
#define MYPORT 3456
#define IBUFS 100
#define MAXARG 16
#define MAXBP 64
#define MAXSYM 3500

void mhp_cmd(const char *);
void mhp_bpset(void);
void mhp_bpclr(void);
int  mhp_bpchk(unsigned char *);
int mhp_setbp(unsigned long seekval);
int mhp_clearbp(unsigned long seekval);

struct brkentry {
   unsigned char * brkaddr;
   unsigned char opcode;
   char is_dpmi;
};

struct cmd_db {
   char cmdname[12];
   void (*cmdproc)(int, char *[]);
};


struct segoff {
  unsigned short off,seg;
};

struct mhpdbg_4bpar
{
  unsigned short env;
  struct segoff
    commandline_ptr,
    fcb1_ptr,
    fcb2_ptr,
    sssp,
    csip;
};

#define PAR4b_addr(x) ((char *)( ((long)mhpdbgc.bpload_par->##x##.seg << 4) \
                        + mhpdbgc.bpload_par->##x##.off ))
struct mhpdbgc
{
   int stopped;
   int currcode;
   int trapcmd;
   int bpload;
   int bpload_bp;
   int int21_count;
   struct mhpdbg_4bpar *bpload_par;
   char bpload_cmd[128];
   char bpload_cmdline[132];
   char intxxtab[256];
   struct brkentry brktab[MAXBP];
};

struct symbol_entry {
   unsigned int  addr;
   unsigned char type;
   unsigned char name[49];
};

struct symbl2_entry {
   unsigned short seg;
   unsigned short off;
   unsigned char type;
   unsigned char name[49];
};
#endif	 /* MHP_PRIVATE */

#endif	 /* MHPDBG_H */
