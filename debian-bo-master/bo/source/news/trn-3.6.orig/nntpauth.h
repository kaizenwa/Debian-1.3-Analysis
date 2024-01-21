/* $Id: nntpclient.h,v 3.0 1992/12/14 00:14:55 davison Trn $
*/ 
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#if defined(USE_NNTP) && defined(USE_GENAUTH)

char	nntp_auth _((char*));

EXT int cookiefd INIT(-1);

#endif
