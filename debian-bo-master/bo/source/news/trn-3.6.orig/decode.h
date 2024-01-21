/* $Id: decode.h,v 3.0 1991/11/22 04:12:25 davison Trn $
 */
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

EXT FILE *decode_fp INIT(NULL);
EXT char decode_fname[MAXFILENAME];
EXT char decode_dest[MAXFILENAME];
EXT int decode_type;

void decode_init _((void));
void decode_end _((void));

void uud_start _((void));
int uudecode _((FILE*));

void unship_init _((void));
int unship _((FILE*));

#define UUDECODE 0
#define UNSHIP   1
