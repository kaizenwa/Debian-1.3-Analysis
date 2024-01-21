/* -*-C-*-
*******************************************************************************
*
* File:         util.h
* RCS:          $Header: util.h,v 1.4 93/04/26 11:18:27 chip Exp $
* Description:  Header file for util.c
* Author:       Chip Chapin
* Created:      Thu Dec 24 11:27:53 1992
* Modified:     Mon Apr 26 11:14:17 1993 (Chip Chapin) chip@hpclbis
* Language:     C
* Package:      N/A
* Status:       Experimental (Do Not Distribute)
*
*******************************************************************************
*
* Revisions:
*
* Fri Apr 23 10:33:49 1993 (Chip Chapin) chip@hpclbis
*  Added Univ_Int stuff.
*******************************************************************************
* $Log:	util.h,v $
 * Revision 1.4  93/04/26  11:18:27  11:18:27  chip (Chip Chapin)
 * Release 4.00
 * Public release of portable datafile version.
 * 
*
*/


#define get_nonblank(S)    while (isspace(*(S))) (S)++;

    
/*
  The Univ_Int data type is stored as a sequence of four bytes, from
  high-order to low-order.  This allows data files to be independent
  of the native byte-order of any machine.
 */
typedef unsigned char Univ_Int[4];/* Universal, byte-order-independent integer*/
typedef unsigned char Short_Univ_Int[2]; /* shorter Univ_Int */

int univ2int();
int shortuniv2int();


FILE *findfile();

