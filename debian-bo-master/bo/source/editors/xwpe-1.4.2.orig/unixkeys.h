/* unixkeys.h						  */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#ifndef DJGPP
#define WE_HELP_ED  "we_help"
#define WE_HELP_PE  "we_help_pe"
#define WE_HELP_CO  "we_help_co"
#define SYNFILE     "we_synt_defs"
#define WASTEBASKET ".wastebasket"
#define BUFFER_NAME "Buffer"   /*  Clipboard in Buffer geaendert  */
#define WPE_LIB_DIR "lib/wpe"
#define HOME_WPE_DIR ".wpe"
#define XOPTFILE ".wpe/xwperc"
#define OPTFILE  ".wpe/wperc"
#define SYSOPTFILE  "wperc"
#define SUDIR "*"
#define MAXREC 15
#ifndef INFO_FILE
#define INFO_FILE "/usr/local/info"
#endif
#ifndef DEF_SHELL
#define DEF_SHELL "sh"
#endif
#else
#define WE_HELP_ED  "we.hlp"
#define WE_HELP_PE  "we_pe.hlp"
#define WE_HELP_CO  ".we_co.hlp"
#define SYNFILE     "we_synt.def"
#define WASTEBASKET "waste.tmp"
#define BUFFER_NAME "Buffer"   /*  Clipboard in Buffer geaendert  */
#define WPE_LIB_DIR "lib/wpe"
#define XOPTFILE "xwpe.opt"
#define OPTFILE  "wpe.opt"
#define SYSOPTFILE  "wpe.opt"
#define SUDIR "*"
#define MAXREC 15
#ifndef INFO_FILE
#define INFO_FILE "/djgpp/info"
#endif
#endif

/*   Zeichen  */

#ifndef DJGPP
extern char MCI, MCA, RD1, RD2, RD3, RD4, RD5, RD6;
extern char RE1, RE2, RE3, RE4, RE5, RE6, WBT;
#else
unsigned char MCI, MCA, RD1, RD2, RD3, RD4, RD5, RD6, WBT;
#endif
/*  extern int FBESNR, FBNSNT, FBNSFT, FBFRFT, FFESNR, FBWS;  */

/*
#ifdef XWINDOW
#define MCI  2
#define MCA  1
#define RD1  13
#define RD2  12
#define RD3  14
#define RD4  11
#define RD5  18
#define RD6  25
#define WBT  2

#else
#define MCI '+'
#define MCA '0'
#define RD1 '+'
#define RD2 '+'
#define RD3 '+'
#define RD4 '+'
#define RD5 '-'
#define RD6 '|'
#define WBT '#'
#endif
*/
#ifndef DJGPP
#define MCU '^'
#define MCD 'v'
#define MCL '<'
#define MCR '>'
#define WZY 'z'
#define WZN 'Z'
#define PWR '$'
#define PNL '\\'
#define SCR 20
#define SCD 16
#define WSW 'v'
/*
#define RE1 '.'
#define RE2 '.'
#define RE3 '.'
#define RE4 '.'
#define RE5 '.'
#define RE6 ':'
*/
#define DIRC '/'
#define DIRS "/"
#define SWSYM '+'
#define PTHD ':'
#else
#define MCU 0x1e
#define MCD 0x1f
#define MCL 0x11
#define MCR 0x10
#define WZY 0x12
#define WZN 0x18
#define PWR 0x14    /*  0xbc; */
#define PNL 0xf9    /*  0xd9; */
#define RE1 0xda
#define RE2 0xbf
#define RE3 0xc0
#define RE4 0xd9
#define RE5 0xc4
#define RE6 0xb3
#define SCR 0xdc
#define SCD 0xdf
#define WSW 0x19
#define DIRC '\\'
#define DIRS "\\"
#define SWSYM 7
#define PTHD ';'
#endif
/*  Sonder-Farben  */
#define SHDCOL 8
/*
#ifndef XWINDOW

#define FBESNR f->fb->es.fb
#define FBNSNT f->fb->ns.fb
#define FBNSFT f->fb->ns.fb
#define FBFRFT f->fb->fr.fb
#define FFESNR f->es.fb
#define FBWS 0

#else

#define FBESNR f->fb->es.f+16*f->fb->nr.b
#define FBNSNT f->fb->ns.f+16*f->fb->nt.b
#define FBNSFT f->fb->ns.f+16*f->fb->ft.b
#define FBFRFT f->fb->fr.f+16*f->fb->ft.b
#define FFESNR f->es.f+16*f->nr.b
#define FBWS 7

#endif
*/
/*
#define CUP 327
#define CDO 335
#define CLE 330
#define CRI 332
#define CCLE 370
#define CCRI 371
#define EINFG 337
#define ENTF 338
#define POS1 326
#define ENDE 334
#define BUP 328
#define BDO 336
#define CBUP 387
#define CBDO 373
#define CPS1 374
#define CEND 372
#define CEINFG 851
#define CENTF  852
*/
#define STOP  421
#define AGAIN 422
#define PROPS 423
#define UNDO  424
#define FRONT 425
#define COPY  426
#define OPEN  427
#define PASTE 428
#define FID   429
#define CUT   430
#define HELP  431

#define DWR 20       /*  ctrl t  */
#define DWL 18       /*  ctrl r  */
#define DNDL 17      /*  ctrl q  */
#define DGZ 25       /*  ctrl y  */

#define CR 13
#define WR 10
#define ESC 27
#define DC 8
#define TAB 9
/*
#define F1 314
#define F2 315
#define F3 316
#define F4 317
#define F5 318
#define F6 319
#define F7 320
#define F8 321
#define F9 322
#define F10 323

#define AF1 359
#define AF2 360
#define AF3 361
#define AF4 362
#define AF5 363
#define AF6 364
#define AF7 365
#define AF8 366
#define AF9 367
#define AF10 368

#define CF1 349
#define CF2 350
#define CF3 351
#define CF4 352
#define CF5 353
#define CF6 354
#define CF7 355
#define CF8 356
#define CF9 357
#define CF10 358
*/
#define SCLE CLE+512
#define SCRI CRI+512
#define SCUP CUP+512
#define SCDO CDO+512

