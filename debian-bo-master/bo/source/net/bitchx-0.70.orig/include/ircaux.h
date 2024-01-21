/*
 * ircaux.h: header file for ircaux.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: ircaux.h,v 1.6 1994/07/02 02:38:10 mrg Exp $
 */

#ifndef _IRCAUX_H_
#define _IRCAUX_H_

#include "irc.h"
#include "irc_std.h"
#include <stdio.h>

typedef int comp_len_func _((char *, char *, int));
typedef int comp_func _((char *, char *));

extern	int	match _((char *, char *));
extern	char	*check_nickname _((char *));
extern	char	*next_arg _((char *, char **));
extern	char	*new_next_arg _((char *, char **));
extern	char	*new_new_next_arg _((char *, char **, char *));
extern	char	*last_arg _((char **));
extern	char	*expand_twiddle _((char *));
extern	char	*upper _((char *));
extern	char	*lower _((char *));
extern	char	*sindex _((char *, char *));
extern	char	*rsindex _((char *, char *, char *));
extern	char	*rfgets _((char *, int, FILE *));
extern	char	*path_search _((char *, char *));
extern	char	*double_quote _((char *, char *));



extern	char *	malloc_strcpy _((char **, char *));
extern	char *	malloc_strcat _((char **, char *));
extern	char *	m_s3cat_s _((char **, char *, char *));
extern	char *	m_s3cat _((char **, char *, char *));
extern	char *	m_3cat _((char **, char *, char *));
extern	char *	m_strdup _((const char *));

extern	void	wait_new_free _((char **));
extern	char	*malloc_sprintf _((char **, char *, ...));
extern	char	*m_sprintf _((char *, ...));
extern	FILE	*zcat _((char *));
extern	int	is_number _((char *));
extern	char *	my_ctime _((time_t));
extern	int	my_stricmp _((char *, char *));
extern	int	my_strnicmp _((char *, char *, int));
extern	int	scanstr _((char *, char *));
extern	void	really_free _((int));
extern	char *	chop _((char *, int));
extern	char *	strmcpy _((char *, char *, int));
extern	char *	strmcat _((char *, char *, int));
extern	char *	strmcat_ue _((char *, char *, int));
extern	char *	m_strcat_ues _((char **, char *, int));
extern	char	*stristr _((char *, char *));
extern	char	*rstristr _((char *, char *));
extern	FILE	*uzfopen _((char **, char *));
extern  int	end_strcmp _((const char *, const char *, int));
extern	void	ircpanic _((char *, ...));
extern	int	vt100_decode _((char));
extern 	char	*strextend _((char *, char, int));

extern	int	empty _((const char *));
extern	char	*safe_new_next_arg _((char *, char **));

#ifdef ALLOC_DEBUG
extern		void	alloc_cmd 	_((char *, char *, char *));
#endif

extern	char	*n_malloc _((size_t, char *, int));
extern	char	*n_realloc _((char *, size_t, char *, int));

extern	char	*n_free _((char **, char *, int));

#define new_malloc(x) n_malloc((x),__FILE__,__LINE__)
#define new_realloc(x,y) n_realloc((x),(y),__FILE__,__LINE__)
#define new_free(x) n_free((x),__FILE__,__LINE__)

extern		int	fw_strcmp 	_((comp_len_func *, char *, char *));
extern		int	lw_strcmp 	_((comp_func *, char *, char *));
extern		int	open_to 	_((char *, int, int));
extern		struct timeval get_time _((struct timeval *));
extern		double 	time_diff 	_((struct timeval, struct timeval));
extern		char*	plural 		_((int));
extern		int	time_to_next_minute _((void));
extern 		char *	remove_trailing_spaces _((char *));
extern		char *	ltoa 		_((long));
extern		char *	strformat 	_((char *, char *, int, char));
extern		char *	chop_word 	_((char *));
extern  	int	splitw 		_((char *, char ***));
extern  	char *	unsplitw 	_((char **, int));
extern		char *  m_e3cat 	_((char **, char *, char *));
extern		char *  m_2dup 		_((const char *, const char *));
extern		int	check_val	_((char *));
extern		char *	on_off		_((int));
extern		char *	m_opendup	_((const char *, ...));
extern  char *  remove_brackets _((char *, char *, int *));

/* From words.c */
#define SOS -32767
#define EOS 32767
extern	char	*search _((char *, char **, char *, int));
extern	char	*move_to_abs_word _((char *, char **, int));
extern	char	*move_word_rel _((char *, char **, int));
extern	char	*extract _((char *, int, int));
extern	char	*extract2 _((char *, int, int));

/* Used for connect_by_number */
#define SERVICE_SERVER 0
#define SERVICE_CLIENT 1
#define PROTOCOL_TCP 0
#define PROTOCOL_UDP 1

/* Used from network.c */
extern int connect_by_number _((char *, unsigned short *, int, int, int));
extern struct hostent *resolv _((const char *));
extern struct hostent *lookup_host _((const char *));
extern struct hostent *lookup_ip _((const char *));
extern char *host_to_ip _((const char *));
extern char *ip_to_host _((const char *));
extern char *one_to_another _((const char *));
extern char *strfill _((char, int));
extern long my_atol _((char *));

#ifdef NON_BLOCKING_CONNECTS
extern int	set_blocking _((int));
extern int	set_non_blocking _((int));
#endif


#define my_isspace(x) \
	((x) == 9 || (x) == 10 || (x) == 11 || (x) == 12 || (x) == 13 || (x) == 32)
  
/* Sheer raving paranoia */
/*
static __inline int strncmp(s1, s2, n)
        register const char *s1, *s2;
        register size_t n;
{

        if (n == 0)
                return (0);
        do {
                if (*s1 != *s2++)
                        return (*(unsigned char *)s1 - *(unsigned char *)--s2);
                if (*s1++ == 0)
                        break;
        } while (--n != 0);
        return (0);
}
*/

#define my_isdigit(x) \
(*x >= '0' && *x <= '9') || \
((*x == '-'  || *x == '+') && (x[1] >= '0' && x[1] <= '9'))

#endif /* _IRCAUX_H_ */
