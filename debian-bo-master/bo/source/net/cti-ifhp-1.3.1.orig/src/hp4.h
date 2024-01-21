/*
 ===========================================================================
 =                                                                         =
 =  (C) Copyright 1995, Computer Technology Institute (CTI)                =
 =                                                                         =
 =  Permission to use, copy, modify and distribute this software and       =
 =  its documentation for non-commercial use and without fee is hereby     =
 =  granted, provided that the above copyright notice appear in all        =
 =  copies and that both that copyright notice and this permission         =
 =  notice and warranty disclaimer appear in supporting documentation,     =
 =  and that the name of Computer Technology Institute (CTI) or any of     =
 =  its entities not be used in advertising or publicity pertaining to     =
 =  distribution of the software and its documentation without specific    =
 =  prior permission.                                                      =
 =                                                                         =
 =  Computer Technology Institute (CTI) disclaims all warranties with      =
 =  regard to this software and makes no representations about the         =
 =  suitability of this software and its documentation for any purpose.    =
 =  It is provided "as is" without expressed or implied warranty.          =
 =                                                                         =
 ===========================================================================
 =                                                                         =
 =                        Project CTI-Print                                =
 =                                                                         =
 = File:                                                                   =
 =   hp4.h                                                               =
 =                                                                         =
 = Synopsis:                                                               =
 =   This file contains mainly the control sequences to be sent to printer =
 =                                                                         =
 = Author:                                                                 =
 =   Panos Dimakopoulos, Systems Programmer,                                =
 =   Computer Technology Institute,                                        =
 =   Division of Computing Facilities,                                     =
 =   P.O. Box 1122,                                                        =
 =   261 10  Patras,                                                       =
 =   Greece                                                                =
 =   (e-mail: dimakop@cti.gr)                                              =
 =   Tel: +30 61 992061                                                    =
 =   Fax: +30 61 993973                                                    =
 =                                                                         =
 ===========================================================================
*/


/****************************************************************************
 * Modification History:
 * 	Revision 1.1	95/01/12	11:45:37
 *		 Creation.
 * 	Revision 1.2	95/01/25	15:01:36
 *		 Landscape and portrait strings defined.
 * 	Revision 1.3	95/02/06	20:07:39
 *		 Landscape and portrait strings revised together with
 *		 improving the margin and pitch settings.
 * 	Revision 1.4	95/02/10	14:03:48
 *		 Full CTI address.
 * 	Revision 1.5	95/02/14	13:54:46
 *		 New Copyright notice.
 *
 */

#define idheadr  "@(#) hp4.h 95/02/14 - V 1.5" ;

/*
 *	PJL Command strings.
 */
#define UEL		"\033%-12345X"
#define	UELPJL		"\033%-12345X@PJL \r\n"

#define USTPJLDEV	"@PJL USTATUS DEVICE = ON \r\n"
#define USTPJLJOB	"@PJL USTATUS JOB = ON \r\n"
#define USTPJLPAGE	"@PJL USTATUS PAGE = ON \r\n"
#define USTPJLTIMED	"@PJL USTATUS TIMED = %d \r\n"
#define USTPJLOFF	"@PJL USTATUSOFF \r\n"

#define RESETPJL	"@PJL RESET \r\n"



#define INFOSTATUS	"@PJL INFO STATUS \r\n"
#define INFOPAGECOUNT	"@PJL INFO PAGECOUNT \r\n"
#define SIMPLXPJL	"@PJL SET DUPLEX = OFF \r\n"
#define DUPLXPJL	"@PJL SET DUPLEX = ON \r\n"

#define PORTRPJL	"@PJL SET ORIENTATION = PORTRAIT \r\n"
#define LANDSPJL	"@PJL SET ORIENTATION = LANDSCAPE \r\n"

#define POSTON		"@PJL ENTER LANGUAGE = POSTSCRIPT \r\n"
#define PCLON		"@PJL ENTER LANGUAGE = PCL \r\n"


/*
 *	PCL Command strings.
 */
#define PCLRESETSTR	"\033E"
#define INITSTR		"\033&s0C"
#define CRLFSTR		"\033&k2G"
#define SELFONT		"\033(12G\033(s0p11h12v0s0b3T"
#define OLDSELFONT	"\033(12G\033(s0p11h12v0s0b3T"
#define SELFID		"\033(963X"
#define DELFONTS	"\033*c0F"
#define PGSTR		"\033&l%dO\033&a%dM\033&a0R\033&l%dF\033&a0L"
#define PGSTRDPL	"\033&l%dO\033&a%dM\033&a0R\033&l%dF\033&a5L"
#define PGRESET		"\0339"
#define ASSIGN		"\033*c963D"
#define MKPRIM		"\033(963X"
#define MKPERM		"\033*c5F"
#define PITCH       "\033&k10.4000H"
#define PITCH80		"\033&k10.4000H"
#define PITCH132	"\033&k6.6079H"
#define PITCHLND	"\033&k10.0000H"
#define SHIFT		"\033&l360U"
#define SHIFTDPL	"\033&l150U"
#define WRAPON		"\033&s0C"
#define FFEED		"\014"
#define EOFSTR		"\004"
#define UNABLE  	"\033(8U\033(s1p24v0s0b4148T\033*p100x1200YCan \
not print PostScript\033*p100x1400Ywithout the Cartridge"
