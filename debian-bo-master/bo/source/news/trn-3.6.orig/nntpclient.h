/* $Id: nntpclient.h,v 3.0 1992/12/14 00:14:55 davison Trn $
*/ 
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#ifdef USE_NNTP

int	server_init _((char*));

int	nntp_connect _((bool_int));
void	nntp_command _((char*));
char	nntp_check _((bool_int));
int	nntp_gets _((char*, int));
void	nntp_close _((bool_int));

#define NNTP_LIST_END(s)  ((s)[0]=='.' && ((s)[1]=='\0' || (s)[1]=='\r'))

#define nntp_get_a_line(buf,len,realloc) get_a_line(buf,len,realloc,ser_rd_fp)

/* RFC 977 defines these, so don't change them */

#define	NNTP_CLASS_INF  	'1'
#define NNTP_CLASS_OK   	'2'
#define	NNTP_CLASS_CONT 	'3'
#define	NNTP_CLASS_ERR  	'4'
#define	NNTP_CLASS_FATAL	'5'

#define	NNTP_POSTOK_VAL 	200	/* Hello -- you can post */
#define	NNTP_NOPOSTOK_VAL	201	/* Hello -- you can't post */
#define NNTP_LIST_FOLLOWS_VAL	215	/* There's a list a-comin' next */

#define NNTP_GOODBYE_VAL	400	/* Have to hang up for some reason */
#define	NNTP_NOSUCHGROUP_VAL	411	/* No such newsgroup */

#define	NNTP_AUTH_NEEDED_VAL 	480	/* Authorization Failed */
#define	NNTP_AUTH_REJECT_VAL	482	/* Authorization data rejected */

#define	NNTP_BAD_COMMAND_VAL	500	/* Command not recognized */
#define	NNTP_SYNTAX_VAL		501	/* Command syntax error */
#define	NNTP_ACCESS_VAL 	502	/* Access to server denied */
#define	NNTP_TMPERR_VAL  	503	/* Program fault, command not performed */
#define	NNTP_AUTH_BAD_VAL 	580	/* Authorization Failed */

#define	NNTP_STRLEN	512

EXT FILE *ser_rd_fp INIT(NULL);
EXT FILE *ser_wr_fp INIT(NULL);
EXT char ser_line[NNTP_STRLEN];

#if defined(NNTP_HANDLE_TIMEOUT) || defined(USE_GENAUTH)
EXT char last_command[NNTP_STRLEN];
#endif

EXT bool CompliantServer INIT(FALSE);

#endif /* USE_NNTP */
