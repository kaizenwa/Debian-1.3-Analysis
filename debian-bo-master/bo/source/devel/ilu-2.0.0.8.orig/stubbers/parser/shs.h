/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

$Id: shs.h,v 1.6 1995/03/03 00:22:18 janssen Exp $
*/

#ifdef __alpha
typedef unsigned int UINT4;
#else
typedef unsigned long UINT4;
#endif

typedef struct {
    UINT4 state[5];                             /* state (ABCDE) */
    UINT4 count[2];        /* number of bits */
    unsigned char buffer[64];                   /* input buffer */
} SHS_CTX;

void SHSInit (SHS_CTX *context);

void SHSUpdate (SHS_CTX *context, unsigned char *buffer, unsigned int nbytes);
	
void SHSFinal (unsigned char hash[20], SHS_CTX *context);
