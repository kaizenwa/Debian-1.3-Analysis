/* elmlib.h */

#undef P_
#ifdef __STDC__
#define P_(x) x
#else
#define P_(x) ()
#endif

/* lib/strmcpy.c */

extern char *strmcpy P_((char *, char *));

/* safeopen.c */

extern FILE *safeopen P_((char *));
extern FILE *safeopen_rdwr P_((char *));

/* istrcmp.c */

extern int istrcmp P_((char *, char *));

/* add_site.c */

extern int add_site P_((char *, char *, char *, int));

/* addrmchusr.c */

extern int addr_matches_user P_((char *, char *));

/* atonum.c */

extern int atonum P_((char *));

/* mk_aliases.c */

extern int get_alias P_((FILE *,int));
extern int get_line P_((FILE *,char *,int, int));
extern void de_escape P_((char *));
extern int add_to_hash_table P_((char *,int32));
extern void add_to_table     P_((FILE *,char *, char *, char *, 
				 char *, char *));
extern int check_alias       P_((char *));
extern int check_address     P_((char *));
extern void put_alias        P_((FILE *));
extern int do_newalias       P_((char *, char *,int, int));

/* aliasdb.c */

/* fetch_alias */
extern char *next_addr_in_list P_((char **));

/* mk_lockname */
     
extern char * mk_lockname P_((char *));

/* can_access.c */
/* I don't understand this routine! access -system call uses
 * real uid / gid anyway!!!!!!!!!!!!!!
 *
 *                                - K E H <hurtta@ozone.FMI.FI>
 */

extern int can_access P_((char *,int));

/* can_open.c */

extern int can_open P_((char *, char *));

/* chloc.c */

/* Is this really needed? This does same than strchr or index, except
 *  that return type is different.
 *
 *                          - K E H <hurtta@ozone.FMI.FI>
 */

/* Argument (second) can't be char because there is both prototype and
 * non-prototype declaration.
 */
extern int chloc  P_((char *,int));
extern int qchloc P_((char *, int));

/* date_util.c */

extern int cvt_dayname_to_daynum     P_((char *,int *));
extern int cvt_monthname_to_monthnum P_((char *,int *));
extern int cvt_yearstr_to_yearnum    P_((char *,int *));
extern int cvt_mmddyy_to_dayofyear   P_((int, int, int, int*));
extern int cvt_timezone_to_offset    P_((char *str,int *, int));
extern int cvt_numtz_to_mins         P_((char *));
extern int cvt_timestr_to_hhmmss     P_((char *, int *, int *, int *));
extern long make_gmttime             P_((int, int, int, int, int, int));

/* dispaddr.c */

extern int DisplayAddress  P_((struct header_rec *,char *,int));
extern void get_real_name P_((char *, char *, int));

/* errno.c */

extern char *error_description P_((int));

/* expnad.c */

extern int expand P_((char *, int));
extern char *expand_define P_((char *));

/* figadrssee.c */

extern void figure_out_addressee P_((char *, char *, int));

/* gcos_name.c */

extern char * gcos_name          P_((char *, char *));

/* get_tz.c */

struct tm;  /* We need here only incomplete defination */

extern int get_tz_mins   P_((void));
extern char *get_tz_name P_((struct tm *));

/* getaddrfrm.c */

extern void get_address_from P_((char *, char *));

/* getarpdate.c */

extern char * get_arpa_date P_((void));

/* getfullnam.c */

extern char * get_full_name P_((char *));

/* getword.c */

extern int get_word P_((char *,int,char *,int));

/* getword.c */

extern char * header_cmp P_((char *, char *, char *));

/* in_list.c */

/* in_list should use quoted -variants (strtokq, ...)
 *                            - K E H <hurtta@ozone.FMI.FI>
 */
extern int in_list   P_((char *, char *));
extern int globmatch P_(( char *, char *));

/* in_string.c */

/* Is that needed? This is almost same as strstr (except return type).
 *                                     - K E H <hurtta@ozone.FMI.FI> 
 */
extern int in_string P_((char *, char *));

/* istrcmp.c */

/* This is same than strcasecmp.   - K E H <hurtta@ozone.FMI.FI>  */
extern int istrcmp  P_((char *, char *));

/* ldstate.c */

/* load_folder_state_file */

/* len_next.c */

extern int len_next_part P_((char *));

/* mail_gets.c */

extern int mail_gets P_((char *,int,FILE *));

/* move_left.c */

extern void move_left P_((char *,int));

/* okay_address.c */

extern int okay_address P_((char *, char *));

/* opt_utils.c */

#ifndef GETHOSTNAME
extern int gethostname P_((char *,int));
#endif

extern int gethostdomain P_((char *,int));

#ifndef HAS_CUSERID
extern char *cuserid P_((char *));
#ifndef L_cuserid
#define L_cuserid 9
#endif
#endif

#ifndef STRTOK
extern char *strtok  P_((char *, char *));
#endif

#ifndef STRPBRK
extern char *strpbrk     P_((char *, char *));
#endif

#ifndef STRSPN
extern int strspn        P_((char *, char *));
#endif

#ifndef STRCSPN
extern int strcspn       P_((char *, char *));
#endif

#ifndef TEMPNAM
extern char *tempnam     P_((char *, char *));
#endif

#ifndef GETOPT
extern int getopt P_((int,char	**, char *));
#endif

#ifndef RENAME
int rename P_((const char *, const char *fname));
#endif

/* parsarpdat.c */

/* parse_arpa_date */

/* parsarpwho.c */

/* This should go away.   - K E H <hurtta@ozone.FMI.FI> */
extern void parse_arpa_who P_((char *, char *,int));

/* posixsig.c */

#ifdef POSIX_SIGNALS
extern SIGHAND_TYPE (*posig_signal  P_((int, 
					SIGHAND_TYPE (*fun)P_((int))
					))
		     )P_((int));
#endif

/* putenv.c */
#ifndef PUTENV
extern int putenv   P_((char *));
#endif

/* realfrom.c */

/* real_from */

/* qstrings.c */

extern char *qstrpbrk   P_((char *, char *));
extern int qstrspn      P_((char *, char *));
extern int qstrcspn     P_((char *, char *));

/* remfirstwd.c */

void remove_first_word     P_((char *));
void remove_header_keyword P_((char *));

/* reverse.c */

extern void reverse P_((char *));

/* safemalloc.c */

extern void dflt_safe_malloc_fail_handler P_((char *proc,unsigned));
extern void (*safe_malloc_fail_handler)   P_((char *proc,unsigned));

extern malloc_t safe_malloc  P_((unsigned));
extern malloc_t safe_realloc P_((malloc_t,unsigned));
extern char *safe_strdup     P_((char *));

/* shiftlower.c */

extern char *shift_lower     P_((char *));

/* strfcpy */

extern char *strfcpy P_((char *, const char *, int));
extern char *strfcat P_((char *, const char *, int));

/* strincmp.c */

/* Is this needed? This is same than strncasecmp 
 *        - K E H <hurtta@ozone.FMI.FI>
 */

extern int strincmp P_((char *, char *, int));

/* striparens.c */

extern char *strip_parens P_((char *));
extern char *get_parens   P_((char *s));

/* strstr.c */

/* Why there is both in_string and strstr????
 *     - K E H <hurtta@ozone.FMI.FI>
 */

#ifndef STRSTR
extern char *strstr P_((char *,char *)); 
#endif

/* strtokq.c */

/* Why there is ' -- it is not quote character in mail.
 *    - K E H <hurtta@ozone.FMI.FI>
 */
extern char *strtokq P_((char *, char *,int));

/* tail_of.c */

extern int tail_of   P_((char *, char *, char *, int));

/* validname.c */

/* This is quite bogus   - K E H <hurtta@ozone.FMI.FI> */
int valid_name       P_((char *));


/* iprintf.c */

struct charset_info;

typedef uint16 ELMCHAR;     /* Character in Elm's 
			     * internal (16-bit) character set 
			     */

extern const ELMCHAR ELMCHAR_space;
extern const  ELMCHAR ELMCHAR_zero;
extern const  ELMCHAR ELMCHAR_sign_minus;
extern const  ELMCHAR ELMCHAR_sign_plus;
extern const  ELMCHAR ELMCHAR_bad;
extern const  ELMCHAR ELMCHAR_letA;
extern const  ELMCHAR ELMCHAR_leta;
extern const  ELMCHAR ELMCHAR_hyphen;
extern const  ELMCHAR ELMCHAR_soft_hyphen;
extern const  ELMCHAR ELMCHAR_NB_hyphen;
extern const  ELMCHAR ELMCHAR_NB_space;   
extern const  ELMCHAR ELMCHAR_cr;
extern const  ELMCHAR ELMCHAR_lf;
extern const  ELMCHAR ELMCHAR_nl;
extern const  ELMCHAR ELMCHAR_bs;
extern const  ELMCHAR ELMCHAR_display_base;
extern const  ELMCHAR ELMCHAR_tab;

extern void init_ELMCHAR P_((void));   /* Must be called before use of
					* functions in iprintf.c
					*/

extern void init_ELMCHAR        P_((void));
extern int is_ELMCHAR           P_((int));
extern int ELMCHAR_compat       P_((int, int));
extern int ELMCHAR_is_printable P_((int));

#define STRING_magic_static   0xFC02
#define STRING_magic_alloced  0xFC03
#define STRING_magic_null     0xFC04

typedef struct elmchar_string {
  unsigned short      magic;
  int                 len;
  ELMCHAR           * str;
} String;

extern const String NULL_String;

#define STRING_INIT(STR) STR = NULL_String

#define STRING_ARRAY(STR) \
{ int i; \
  for (i = 0; i < sizeof STR / sizeof STR[0]; i++) { \
     STRING_INIT(STR[i]); \
  } \
}

#define STRING_FREE(STR) switch (STR.magic) { \
default:   fprintf(stderr,\
           "\r\nSTRING_FREE: bad magic (file %s, line %d) %d\r\n", \
           __FILE__,__LINE__,STR.magic); abort(); break; \
case STRING_magic_alloced: \
  free(STR.str); STRING_INIT(STR); \
case STRING_magic_static: \
case STRING_magic_null: \
  break; \
}

#define STRING_ARRAY_FREE(STR) \
{ int i; \
  for (i = 0; i < sizeof STR / sizeof STR[0]; i++) { \
     STRING_FREE(STR[i]); \
  } \
}

extern void STRING__append_char P_((String *str, int c1));
extern void STRING__append_string P_((String *str, String *a));
extern void STRING__cut P_((String *str, int len));

#define STRING_APPEND_CHAR(str,c)   STRING__append_char(&(str),(int)c)
#define STRING_APPEND_STRING(str,a) STRING__append_string(&(str),&(a))
#define STRING_CUT(str,len)         STRING__append_char(&(str),len)

enum switch_system { switch_none, switch_ISO2022 };
			
extern const struct charset_state {
  enum switch_system system;

  int arg0;
  int arg1;
  int arg2;
  int arg3;

  unsigned char pr0;
  unsigned char pr1;

  int lookahead;
}  NULL_charset_state;

typedef struct charset_state charset_state_t;

extern int set_state  P_((struct charset_info *info,
			  charset_state_t *state,
			  enum switch_system system));

typedef int set_state_hook  P_((struct charset_info *info,
				charset_state_t *state,
				enum switch_system system));

/* returns how many characters was digested from inbuf
 * 0 == incomplete sequence, supply more
 * inbuf == NULL, reset state
 */
typedef int char_to_ELMCHAR  P_((struct charset_info *info,
				   charset_state_t *state,
				   String *outbuf,
				   const char *inbuf, int len));

/* returns how many characters was digested from inbuf
 * start = from where to start digesting in inbuf
 * inbuf == NULL: end of string, go to default state
 * reslen  number of charracters written
 */
typedef int ELMCHAR_to_char  P_((struct charset_info *info,
				 charset_state_t *state,
				 char *output, int size, int *reslen,
				 String *inbuf, int start));

#define CHARSET_INFO_magic   0xFC01

typedef struct charset_info {
  unsigned short     magic;
  const char *       name;  /* Name for messages */
  int                ref;   /* charset reference number */
  char_to_ELMCHAR  * from_charset;
  ELMCHAR_to_char  * to_charset;
  set_state_hook   * init_charset;
} * charset_t;

extern const charset_t   CODE_charset;
extern struct charset_info display_charset_s;
extern enum display_charset_options { display_cs_raw, 
				      display_cs_fallback,
				      display_cs_ascii } 
display_charset_option;

typedef void printfunc P_((void *ref, String *str));

typedef struct nl_info {
  nl_catd    catd;
  charset_t  nl_charset ;
} * nl_info_t;

extern int iprintf P_((void *ref, printfunc *func,
                       nl_info_t cati, int set_num,
                       const char *format, ...));

extern int viprintf P_((void *ref, printfunc *func,
                        nl_info_t cati, int set_num,
                        const char *format, va_list args));
   
extern int vsiprintf P_((char *s, int size, charset_t cset,
			nl_info_t cati, int set_num,
			const char *format, va_list args));

extern int siprintf P_((char *s, int size, charset_t cset,
                        nl_info_t cati, int set_num,
                        const char *format, ...));
   
extern int vSprintf P_((String *s,
                        nl_info_t cati, int set_num,
                        const char *format, va_list args));

extern int Sprintf P_((String *s,
			nl_info_t cati, int set_num,
			const char *format, ...));
   

extern int sf_printf P_((char *s, int size, const char *format, ...));

/*   format
 *         %sa.bX
 *
 *      s:    0         fill character is '0'
 *            +         leading '+' (or '-')
 *            -         left justify
 *            space     leading ' '
 *            #         alternate format
 *            !         Read charset_t
 *      a:    *         read int argument for width
 *            num       width
 *      b:    *         read int argument for precision or max width
 *            num       precision or max width
 *      X:    h         'short' (flag)
 *            l         'long'  (flag)
 *
 *            d         int
 *            i         int
 *            o         unsigned int
 *            u         unsigned int
 *            x         unsigned int
 *            X         unsigned int
 *
 *            f         double    (not iplmented)
 *            e         double    (not impmented)
 *            E         double    (not impmented)
 *            g         double    (not impmented)
 *            G         double    (not impmented)
 * 
 *            c         int       (unsigned char)
 *            C         wchar_t   (not impmented)
 *
 *            s         char *
 *            S         wchar_t *  (not impmented)
 *            p         void *     (not impmented)
 *
 *            n         int *      [no output]
 *            %                     %%
 *
 *            &S        String *
 *            &C        unsigned int        (ELMCHAR)
 *
 *            &!        charset_t  [set default charset for %s]
 *
 *
 *   in 'format' there can't be positional 'p$' specifiers
 *
 *   for messages via catgets (set_num):
 *       %p$sa.bX
 *	 Only p$ is used -- all other values are tka from original
 *                            format
 */
