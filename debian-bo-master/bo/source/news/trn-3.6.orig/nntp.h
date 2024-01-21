/* $Id: nntp.h,v 3.0 1992/12/14 00:14:55 davison Trn $
*/ 
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#ifdef USE_NNTP

#define FB_BACKGROUND	0
#define FB_OUTPUT	1
#define FB_SILENT	2

int	nntp_list _((char*,char*,int));
bool	nntp_group _((char*,NG_NUM));
bool	nntp_stat _((ART_NUM));
ART_NUM	nntp_stat_id _((char*));
bool	nntp_header _((ART_NUM));
FILE	*nntp_body _((ART_NUM));
long	nntp_artsize _((void));
int	nntp_finishbody _((int));
void	nntp_seekart _((long));
long	nntp_tellart _((void));
char	*nntp_readart _((char*,int));
time_t	nntp_time _((void));
bool	nntp_newgroups _((time_t));
bool	nntp_listgroup _((void));
char	*nntp_artname _((void));
char	nntp_handle_timeout _((bool_int));
void	nntp_cleanup _((void));

#ifdef USE_XTHREAD
long	nntp_readcheck _((void));
long	nntp_read _((char*,long));
#endif

#include "nntpclient.h"

#endif /* USE_NNTP */
