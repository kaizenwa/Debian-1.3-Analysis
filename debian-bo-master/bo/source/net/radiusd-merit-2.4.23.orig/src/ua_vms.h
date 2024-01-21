#ifndef UA_vms_h
/*E
1* UA_vms
 *
 * vms.h - VMS specific definitions (UA)
 * Last modified 16-Sep-1987/chj
 *
 * Common VMS header file.  Automatically includes some useful
 * VMS definition files, and, typedefs things for a VAX.
 *
2*  includes
E*/

/*E+
3* UA */

#include ssdef			/* system service status codes */
#include stsdef			/* status code definitions */
/*E-*/

/*E+
3* DEC
 * DEC supplied definition modules that are always included
 * are located in default C text library on SYS$LIBRARY */

#include descrip		/* descriptor definitions */
/*E-*/

/*E+
2* typedefs
 * data types for VMS */

typedef unsigned int QUAD[2];	/* VAX quadword */
typedef unsigned int LONG;	/* vax longword */
typedef unsigned int ADDRESS;	/* address quantity */
typedef unsigned short int WORD;/* 16 bit word */
typedef unsigned char BYTE;	/* 8 bits */
typedef unsigned char LOGICAL;	/* cheapest */
typedef int     (*INTFUNC) ();	/* pointer to integer function */
typedef struct _item
{
	short           length;	/* length of data buffer */
	short           code;	/* item code */
	BYTE           *buffer;	/* data buffer */
	int            *retlen;	/* returned item length */
}               ITEM;

extern unsigned int zero;
extern unsigned int one;
extern unsigned int blank;

/*E-*/

/*E
2* macros
 * Handy macros for doing things to VMS data.
 *
 * iferr(exp)		1 if exp represents error condition
 * ifsucc(exp)		1 if exp represents success
 * inhibit_msg(code)	VMS status code with message output inhibited
 * sbit(pos,base)	sets a bit to 1
 * cbit(pos,base)	clears a bit (sets bit to 0)
 * tbit(pos,base)	returns a bit value (1 or 0)
 *
 * dsc$descriptor_d(name) initialized (to NIL) dynamic string
 *
3* DEC_macros
E*/

/*E+*/
#define $VMS_STATUS_CODE(code) 	(((code) & STS$M_CODE) >> STS$V_CODE)
#define $VMS_STATUS_COND_ID(code) (((code) & STS$M_COND_ID) >> STS$V_COND_ID)
#define $VMS_STATUS_CONTROL(code)  (((code) & STS$M_CONTROL) >> STS$V_CONTROL)
#define $VMS_STATUS_CUST_DEF(code)  (((code) & STS$M_CUST_DEF) >> STS$V_CUST_DEF)
#define $VMS_STATUS_FAC_NO(code)  (((code) & STS$M_FAC_NO) >> STS$V_FAC_NO)
#define $VMS_STATUS_FAC_SP(code)  (((code) & STS$M_FAC_SP) >> STS$V_FAC_SP)
#define $VMS_STATUS_INHIB_MSG(code) (((code) & STS$M_INHIB_MSG) >> STS$V_INHIB_MSG)
#define $VMS_STATUS_MSG_NO(code)  (((code) & STS$M_MSG_NO) >> STS$V_MSG_NO)
#define $VMS_STATUS_SEVERITY(code)  (((code) & STS$M_SEVERITY) >> STS$V_SEVERITY)
#define $VMS_STATUS_SUCCESS(code)  (((code) & STS$M_SUCCESS) >> STS$V_SUCCESS)
/*E-*/

/*E
3* UA_macros
E*/

/*E+*/
#define iferr(s) if (!($VMS_STATUS_SUCCESS(s)))
#define _iferr(s) if (!($VMS_STATUS_SUCCESS(s)))
#define ifsucc(s) if (($VMS_STATUS_SUCCESS(s)))
#define _ifsucc(s) if (($VMS_STATUS_SUCCESS(s)))

#define inhibit_msg(c) (c | STS$M_INHIB_MSG)
#define _inhibit_msg(c) (c | STS$M_INHIB_MSG)
#define	error(s) (!($VMS_STATUS_SUCCESS(s)))
#define	_error(s) (!($VMS_STATUS_SUCCESS(s)))
#define success(s) (($VMS_STATUS_SUCCESS(s)))
#define _success(s) (($VMS_STATUS_SUCCESS(s)))

#define sbit(p,b) (lib$insv(&1,&p,&1,b))	/* set a bit */
#define _sbit(p,b) (lib$insv(&1,&p,&1,b))	/* set a bit */
#define cbit(p,b) (lib$insv(&0,&p,&1,b))	/* clear a bit */
#define _cbit(p,b) (lib$insv(&0,&p,&1,b))	/* clear a bit */
#define tbit(p,b) (lib$extv(&p,&1,b))	/* test a bit */
#define _tbit(p,b) (lib$extv(&p,&1,b))	/* test a bit */

#define dsc$descriptor_d(a) struct dsc$descriptor_s a = \
	{0,DSC$K_DTYPE_T,DSC$K_CLASS_D,0}

#define _zero(s,d)	LIB$MOVC5(&zero,&zero,&zero,&s,d)
#define	_copy(l, s, d) \
			LIB$MOVC5(&l, s, &zero, &l, d)

#define	_zcopy(sl,sb,dl,db)	LIB$MOVC5(&sl,sb,&zero,&dl,db)
#define	_bcopy(sl,sb,dl,db)	LIB$MOVC5(&sl,sb,&blank,&dl,db)
#define	_xcopy(x,sl,sb,dl,db)	LIB$MOVC5(&sl,sb,&x,&dl,db)

/* macro definition for converting string to descriptor */

#define str2desc( desc , string ) \
    { desc.dsc$w_length = strlen(string); \
      desc.dsc$a_pointer = string; \
      desc.dsc$b_class = DSC$K_CLASS_S; \
      desc.dsc$b_dtype = DSC$K_DTYPE_T; \
    }

/* macro for converting descriptor to string */

#define desc2str( string , desc ) \
    { _copy(desc.dsc$w_length, desc.dsc$a_pointer, string); \
      string[desc.dsc$w_length] = '\0'; \
    }
/*E-*/

/*E+
2* defines
 * useful definitions */

#define	TRUE	1
#define	FALSE	0
#define	YES	1
#define	NO	0
#define	NIL	0
/*E-*/


#define	UA_vms_h	1	/* TRUE when UA_vms is included */
#endif				/* ifndef UA_vms */

/* end vms.h */
