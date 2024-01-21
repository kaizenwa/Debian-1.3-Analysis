/*
 * ibcs trace -- mask of trace values
 *
 * $Id: trace.h,v 1.9 1995/11/16 15:34:23 mike Exp $
 * $Source: /usr/CVS/ibcs/include/ibcs/trace.h,v $
 */
#define	TRACE_API	0x00000001 /* all call/return values */
#define	TRACE_IOCTL	0x00000002 /* all ioctl calls */
#define	TRACE_IOCTL_F	0x00000004 /* ioctl calls that fail */
#define	TRACE_SIGNAL	0x00000008 /* all signal calls */
#define	TRACE_SIGNAL_F	0x00000010 /* signal calls that fail */
#define	TRACE_SOCKSYS	0x00000020 /* socksys and spx devices */
#define	TRACE_COFF_LD	0x00000040 /* COFF loader */
#define	TRACE_ELF_LD	0x00000080 /* ELF loader */
#define	TRACE_XOUT_LD	0x00000100 /* XOUT loader */
#define	TRACE_XOUT_DB	0x00000200 /* XOUT loader blocks before launch */
#define	TRACE_STREAMS	0x00000400 /* STREAMS faking */
#define	TRACE_FUNC	0x10000000 /* trace this function */

extern int ibcs_trace;
extern IBCS_func *ibcs_func_p;

extern int ibcs_trace_set(int arg);
extern int ibcs_trace_func(unsigned int per, int func, int val);
