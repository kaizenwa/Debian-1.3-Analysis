/* this is for the DPMI support */
#ifndef DPMI_H
#define DPMI_H

#define DPMI_VERSION   		0x00	/* major version 0 */
#define DPMI_DRIVER_VERSION	0x5a	/* minor version 0.90 */

#define DPMI_MAX_CLIENTS	8	/* maximal number of clients */

#define DPMI_page_size		4096	/* 4096 bytes per page */

#define DPMI_pm_stack_size	0x1000	/* locked protected mode stack for exceptions, */
					/* hardware interrupts, software interrups 0x1c, */
					/* 0x23, 0x24 and real mode callbacks */

#define DPMI_max_rec_rm_func	16	/* max number of recursive real mode functions */
#define DPMI_rm_stack_size	0x0200	/* real mode stack size */

#define DPMI_private_paragraphs	((DPMI_max_rec_rm_func * DPMI_rm_stack_size)>>4)
					/* private data for DPMI server */

#ifdef __linux__
#define UCODESEL 0x23
#define UDATASEL 0x2b
#endif
#ifdef __NetBSD__
#define	UCODESEL GSEL(GUCODE_SEL, SEL_UPL)
#define	UDATASEL GSEL(GUDATA_SEL, SEL_UPL)
#endif

EXTERN int in_dpmi INIT(0);        /* Set to 1 when running under DPMI */
#define current_client (in_dpmi-1)
EXTERN int in_win31 INIT(0);       /* Set to 1 when running Windows 3.1 */
EXTERN int dpmi_eflags INIT(0);    /* used for virtuell interruptflag and pending interrupts */
EXTERN int in_dpmi_dos_int INIT(0);
EXTERN int in_dpmi_pm_int INIT(0);
EXTERN int dpmi_mhp_TF INIT(0);
EXTERN unsigned char dpmi_mhp_intxxtab[256] INIT({0});

void dpmi_get_entry_point();

#ifdef __linux__
void dpmi_fault(struct sigcontext_struct *);
#endif
#ifdef __NetBSD__
void dpmi_fault(struct sigcontext *, int);
#endif
void dpmi_realmode_hlt(unsigned char *);
void run_pm_int(int);
void run_pm_mouse();
void fake_pm_int(void);

#ifdef __linux__
int dpmi_mhp_regs(void);
void dpmi_mhp_getcseip(unsigned int *seg, unsigned int *off);
void dpmi_mhp_getssesp(unsigned int *seg, unsigned int *off);
int dpmi_mhp_get_selector_size(int sel);
int dpmi_mhp_getcsdefault(void);
int dpmi_mhp_setTF(int on);
void dpmi_mhp_GetDescriptor(unsigned short selector, unsigned long *lp);
int dpmi_mhp_getselbase(unsigned short selector);
unsigned long dpmi_mhp_getreg(int regnum);
void dpmi_mhp_setreg(int regnum, unsigned long val);
#endif

/* this is used like: SEL_ADR(_ss, _esp) */
#define SEL_ADR(seg, reg) \
({ unsigned long __res; \
  if (!((seg) & 0x0004)) { \
    /* GTD */ \
    __res = (unsigned long) reg; \
  } else { \
    /* LDT */ \
    if (Segments[seg>>3].is_32) \
      __res = (unsigned long) (GetSegmentBaseAddress(seg) + reg ); \
    else \
      __res = (unsigned long) (GetSegmentBaseAddress(seg) + *((unsigned short *)&(reg)) ); \
  } \
__res; })

#define HLT_OFF(addr) ((unsigned long)addr-(unsigned long)DPMI_dummy_start)

typedef struct interrupt_descriptor_s
{
    unsigned long	offset;
    unsigned short	selector, __selectorh;
} INTDESC;

typedef struct segment_descriptor_s
{
    unsigned long	base_addr;	/* Pointer to segment in flat memory */
    unsigned int	limit;		/* Limit of Segment */
    unsigned int	type:2;
    unsigned int	is_32:1;	/* one for is 32-bit Segment */
    unsigned int	readonly:1;	/* one for read only Segments */	
    unsigned int	is_big:1;	/* Granularity */
    unsigned int	not_present:1;		
    unsigned int	useable:1;		
    unsigned int	used;		/* Segment in use by client # */
} SEGDESC;

#define MAX_SELECTORS	8192

extern SEGDESC Segments[];

struct RealModeCallStructure {
  unsigned long edi;
  unsigned long esi;
  unsigned long ebp;
  unsigned long esp;
  unsigned long ebx;
  unsigned long edx;
  unsigned long ecx;
  unsigned long eax;
  unsigned short flags;
  unsigned short es;
  unsigned short ds;
  unsigned short fs;
  unsigned short gs;
  unsigned short ip;
  unsigned short cs;
  unsigned short sp;
  unsigned short ss;
};

typedef struct {
    unsigned short selector;
    unsigned long  offset;
    unsigned short rmreg_selector;
    unsigned long  rmreg_offset;
    struct RealModeCallStructure *rmreg;
    unsigned rm_ss_selector;
} RealModeCallBack;

typedef struct dpmi_pm_block_stuct {
  struct   dpmi_pm_block_stuct *next;
  unsigned long handle;
  unsigned long size;
  void     *base;
} dpmi_pm_block;

dpmi_pm_block* DPMImalloc(unsigned long size);
dpmi_pm_block* DPMImallocFixed(unsigned long base, unsigned long size);
int DPMIfree(unsigned long handle);
dpmi_pm_block *DPMIrealloc(unsigned long handle, unsigned long size);
void DPMIfreeAll(void);
unsigned long base2handle(void *);
dpmi_pm_block *lookup_pm_block(unsigned long h);
int
DPMIMapConventionalMemory(dpmi_pm_block *block, unsigned long offset,
			  unsigned long low_addr, unsigned long cnt);

#define DPMI_show_state \
    D_printf("eip: 0x%08lx  esp: 0x%08lx  eflags: 0x%08lx\n" \
	     "trapno: 0x%02lx  errorcode: 0x%08lx  cr2: 0x%08lx\n" \
	     "cs: 0x%04x  ds: 0x%04x  es: 0x%04x  ss: 0x%04x  fs: 0x%04x  gs: 0x%04x\n", \
	     _eip, _esp, _eflags, _trapno, _err, _cr2, _cs, _ds, _es, _ss, _fs, _gs); \
    D_printf("EAX: %08lx  EBX: %08lx  ECX: %08lx  EDX: %08lx\n", \
	     _eax, _ebx, _ecx, _edx); \
    D_printf("ESI: %08lx  EDI: %08lx  EBP: %08lx\n", \
	     _esi, _edi, _ebp); \
    /* display the 10 bytes before and after CS:EIP.  the -> points \
     * to the byte at address CS:EIP \
     */ \
    if (!((_cs) & 0x0004)) { \
      /* GTD */ \
      csp2 = (unsigned char *) _eip - 10; \
    } \
    else { \
      /* LDT */ \
      csp2 = (unsigned char *) (GetSegmentBaseAddress(_cs) + _eip) - 10; \
    } \
    D_printf("OPS  : "); \
    for (i = 0; i < 10; i++) \
      D_printf("%02x ", *csp2++); \
    D_printf("-> "); \
    for (i = 0; i < 10; i++) \
      D_printf("%02x ", *csp2++); \
    D_printf("\n"); \
    if (!((_ss) & 0x0004)) { \
      /* GTD */ \
      ssp2 = (unsigned char *) _esp - 10; \
    } \
    else { \
      /* LDT */ \
      if (Segments[_ss>>3].is_32) \
	ssp2 = (unsigned char *) (GetSegmentBaseAddress(_ss) + _esp ) - 10; \
      else \
	ssp2 = (unsigned char *) (GetSegmentBaseAddress(_ss) + _LWORD(esp) ) - 10; \
    } \
    D_printf("STACK: "); \
    for (i = 0; i < 10; i++) \
      D_printf("%02x ", *ssp2++); \
    D_printf("-> "); \
    for (i = 0; i < 10; i++) \
      D_printf("%02x ", *ssp2++); \
    D_printf("\n");

extern void dpmi_sigio(struct sigcontext_struct *scp);
extern void run_dpmi(void);

#endif /* DPMI_H */
