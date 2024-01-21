/* $Id: decode.c,v 3.0 1991/11/22 04:12:25 davison Trn $
 */
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "INTERN.h"
#include "decode.h"

void
decode_init()
{
    unship_init();
}

void
decode_end()
{
    if (decode_fp != Nullfp) {
	fclose(decode_fp);
	decode_fp = Nullfp;
	printf("\n%s INCOMPLETE -- removed.\n", decode_dest) FLUSH;
	unlink(decode_dest);
    }
}
