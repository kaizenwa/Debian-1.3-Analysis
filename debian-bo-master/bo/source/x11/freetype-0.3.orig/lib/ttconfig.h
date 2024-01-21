/*******************************************************************
 *
 *  TTConfig.h                                                1.0   
 *
 *    Configuration settings header file (spec only).            
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  Notes :
 *
 *    All the configuration #define have been gathered in this file
 *    to allow easy check and modification.       
 *
 ******************************************************************/

#ifndef TTCONFIG_H
#define TTCONFIG_H

/* ------------ general debugging -------------------------------------- */

/* Define DEBUG if you want the program to output a series of messages   */
/* to stderr regarding its behaviour. Only useful during development.    */

/* #define DEBUG */


/* ------------ arithmetic and processor support - ttcalc, ttraster ---- */
#define FT_LITTLE_ENDIAN 1234
#define FT_BIG_ENDIAN 4321

/* Define FT_BYTE_ORDER to match your processor's architecture.     */
/* i.e. Intel x86 is little endian and Motorola m68k is big endian. */ 

#ifndef FT_BYTE_ORDER
#define FT_BYTE_ORDER  FT_LITTLE_ENDIAN
#endif

/* Define ONE_COMPLEMENT if this matches your processor's artihmetic.    */
/* The default is 2's complement. 1's complement is not supported yet.   */

/* #define ONE_COMPLEMENT */


/* Define BOUND_CALC if you want bounded calculations in ttcalc.    */
/*                                                                  */
/* Bounded calculations mean that in case of overflow a Mul, Div    */
/* or MulDiv operation will return +/- MaxInt32                     */

/* #define BOUND_CALC */


/* define BUS_ERROR if your processor is unable to access words that */
/* are not aligned to their respective size (i.e. a 4byte dword      */
/* beginning at address 3 will result in a bus error on a Sun)       */

/* This may speed up a bit some parts of the engine */

/* #define BUS_ERROR */

/* define ALIGNMENT to your processor/environment preferred alignment */
/* size. Usually 2 or 4 bytes (8 bytes in some 64-bits systems ;-)    */

#define ALIGNMENT 4


/* ------------ rasterizer configuration ----- ttraster ----------------- */

/* Define SECURE if you want to use the 'MulDiv' function from 'ttcalc'.  */
/* (it computes (A*B)/C with 64 bits intermediate accuracy. However, for  */
/* 99.9% of screen display, this operation can be done directly with      */
/* good accuracy, because 'B' is only a 6bit integer).                    */
/*                                                                        */
/* Note that some compilers can manage directly 'a*b/c' with intermediate */
/* accuracy ( GCC can use long longs, for example ). Using the unsecure   */
/* definition of Muliv would then be sufficient.                          */
/*                                                                        */
/* The SECURE option is probably a good option for 16 bit compilers.      */

/* #define SECURE */


/* Define DEBUG3 if you want to generate a debug version of 'zoom.exe'. */
/* This will progressively draw the glyphs while the computations are   */
/* done directly on the graphics screen... (inverted glyphs)            */
/*                                                                      */
/* Note that DEBUG3 should only be used for debugging while in mono     */
/* mode, as it does not support grayscaling.                            */

/* #define DEBUG3 */


/* The TrueType specs stipulate that the filled regions delimited by */
/* the contours must be to the right of the drawing orientation.     */
/* Unfortunately, a lot of cheapo fonts do not respect this rule.    */
/*                                                                   */
/* Defining REVERSE builds an engine that manages all cases.         */
/* Not defining it will only draw 'valid' glyphs & contours.         */

#define REVERSE
/* We want to draw all kinds of glyphs, even incorrect ones .. */


/* --------------- automatic setup -- don't touch ------------------ */

#if FT_BYTE_ORDER == FT_BIG_ENDIAN
#ifndef BUS_ERROR

/* Some big-endian machines that are not alignment-sensitive may   */
/* benefit from an easier access to the data found in the TrueType */
/* files. (used in ttfile.c)                                       */
#define LOOSE_ACCESS

#endif /* !BUS_ERROR */
#endif /* FT_BYTE_ORDER */

#endif /* TTCONFIG_H */
