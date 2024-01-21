/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */

extern const char	base64_alpha[64];
extern const char	inv_base64_alpha[256];

#define BASE64_ERROR		1u
#define BASE64_TRAILING_GARBAGE	2u

#define UUE_NO_BEGIN		1u
#define UUE_NO_END		2u
#define UUE_ERROR		4u

typedef struct {
    unsigned char	state;
    unsigned char	err;
} UueContext;

typedef struct {
    unsigned long	data;
    unsigned int	n;
    unsigned char	end;
    unsigned char	err;
    unsigned char	trailing_garbage;
} B64Context;

extern long	decode_qp(char*, char*, long, int*, int);
extern long	decode_base64(B64Context*, char*, char*, long);
extern int	base64_status(B64Context*);
extern long	decode_uue(UueContext*, char*, char*, long);
extern int	uue_status(UueContext*);
