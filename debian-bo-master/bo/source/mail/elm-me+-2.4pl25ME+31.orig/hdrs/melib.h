/* melib.h */

#undef P_
#ifdef __STDC__
#define P_(x) x
#else
#define P_(x) ()
#endif

/** Definitions for state operations **/

struct in_state;
struct out_state;

/* can't be 'char' as argumnet because problems of default
 * promotion rules of arguments -- therefore 'int' is used
 * in functios: state_filter, state_putc
 *
 * Notice also that we assumen here that char agument (as int)
 * is 'unsigned char'
 *
 *  - K E H
 */

typedef int state_filter P_((int, struct out_state *));

/* state magics */
#define   STATE_in_file      0xFA01
#define   STATE_in_string    0xFA02

#define   STATE_out_file     0xFB01
#define   STATE_out_string   0xFB02

typedef struct {
  FILE *fpin;
} in_state_file;

typedef struct {
  char *inbuf;
  char *inreadp;
} in_state_string;

typedef struct in_state {
  unsigned short magic;

  union {
    in_state_file    file;
    in_state_string  string;
  } u;
} in_state_t;

typedef struct {
  FILE *fpout;
} out_state_file;

typedef struct {
  char *outbuf;
  char *outwritep;
  int outbufsize;
} out_state_string;


typedef struct out_state {
  unsigned short magic;

  unsigned int displaying : 1;
  char *prefix;

  state_filter * filter;

  union {
    out_state_file    file;
    out_state_string  string;
  } u;
} out_state_t;

#define state_add_prefix(x) if((x)->prefix)state_puts((x)->prefix,x)

extern void in_state_clear   P_((in_state_t *, int));
extern void in_state_destroy P_((in_state_t *));

extern void set_in_state_buffer P_((char *,in_state_t *));   /* STATE_in_string */
extern void set_in_state_file   P_((FILE *,in_state_t *));   /* STATE_in_file */

extern int  in_state_seekable P_((in_state_t *)); 
extern int  in_state_fseek P_((in_state_t *, long)); /* STATE_in_file */
extern long in_state_ftell P_((in_state_t *));       /* STATE_in_file */
extern FILE * in_state_FILE P_((in_state_t *)); /* STATE_in_file */

extern char *state_gets P_((char *, int, in_state_t *));
extern int   state_getl P_((char *, int, in_state_t *));
extern int   state_getc P_((in_state_t *s));
extern int   state_ungetc P_((int,in_state_t *));

extern void out_state_clear   P_((out_state_t *, int));
extern void out_state_destroy P_((out_state_t *));

/* STATE_out_string */
extern void set_out_state_buffer P_((char *,int,out_state_t *));
extern void set_out_state_file   P_((FILE *,out_state_t *));  /* STATE_out_file */

extern int state_put  P_((char *, int, out_state_t *));
extern int state_puts P_((char *, out_state_t *));
extern int state_putc P_((int,  out_state_t *));

/* STATE_out_string */
extern void out_state_ref_buffer P_((out_state_t *, char **, int*));

extern int NULL_filter P_((int, struct out_state *));

#ifdef USE_PGP
/* pgp_decode.c */
extern void pgp_void_passphrase         P_((void));
extern int pgp_goodPassphrase           P_((void));
extern int pgp_decrypt_init		P_((FILE **, FILE **, int));
#ifdef MIME
extern void pgp_decode  		P_((mime_t *, 
					    in_state_t *, out_state_t *));
#endif
#endif

/* parse_util.c */

typedef struct header_info {
  char * header;
  int flag;
} * header_ptr;

#define HEADER_magic        0xFC00

typedef struct header_list {
  unsigned short magic;
  header_ptr header_name;
  struct header_list * next_this_header;
  struct header_list * next_header;
  struct header_list * next_other_header;
  char * body;
} * header_list_ptr;

#define   RHL_MARK_FOLDING      1
#define   RHL_CHECK_HEADER      2

extern void rfc822_reap_comments        P_((char *, char *, int));
extern int read_header_line             P_((FILE *,char *,int,int));
extern int state_read_hdr_line          P_((in_state_t *,char *, int, int));

typedef int header_filter    P_((header_list_ptr, int));
typedef void header_converter P_((header_list_ptr, int, char *, int));

extern int NULL_header_filter    P_((header_list_ptr, int));
extern void NULL_header_converter P_((header_list_ptr, int, char *, int));

extern void state_write_headers P_((out_state_t *,header_list_ptr,
				    header_filter *, header_converter *,
				    int));

extern header_list_ptr state_read_headers P_((in_state_t *, int));
extern header_list_ptr file_read_headers  P_((FILE *, int));
extern void delete_headers                P_((header_list_ptr));
extern header_list_ptr locate_header      P_((header_list_ptr,header_ptr));

extern header_ptr find_header           P_((char *,int));
extern int classify_header              P_((char *));

#define locate_header_by_name(h,n)      locate_header(h,find_header(n,0))

extern long skip_envelope P_((struct header_rec *hdr, FILE *fp));



#ifdef MIME

/* mime.c */

extern void mime_panic          P_((char *,int,char *, char *));
extern int check_encoding       P_((char *));
extern int charset_ok           P_((char *));
/* mime_decode.c */

extern void base64_decode	P_((in_state_t *, out_state_t *, int, int));
extern void quoted_printable_decode  P_((in_state_t *, out_state_t *,
					 int, int));
extern void null_decode		P_((mime_t *, in_state_t *, out_state_t *));
extern void mime_decode		P_((mime_t *, in_state_t *, out_state_t *));
extern void rfc1522_decode P_((char *, int));
extern void rfc1522_decode_structured   P_((int, char *, int));
extern int is_rfc1522 P_((char *));

extern int set_filter	P_((mime_t *, out_state_t *)); 
extern FILE *arrange_decoded P_((mime_t *,in_state_t  *,
			      out_state_t *,in_state_t *));

typedef void CT_decoder	        P_((mime_t *, in_state_t *, out_state_t *));
typedef CT_decoder *CT_decoder_t;
extern CT_decoder_t select_CT_decoder	P_((mime_t *));

/* mime_parse.c */

extern mime_t *mime_t_alloc             P_((void));
extern void mime_t_copy                 P_((mime_t *, mime_t *));
extern void mime_t_zero                 P_((mime_t *));
extern void mime_destroy		P_((mime_t *));
extern void mime_get_disposition        P_((char *, mime_t *));
extern void mime_get_content		P_((char *, mime_t *));
extern int class_charset                P_((char *));
extern int mime_get_charset             P_((char *, char *, int));
extern char * mime_parse_content_opts	P_((char *));
extern int mime_get_param               P_((char *,char *,char *,int));
extern void mime_warnings		P_((struct header_rec *));
extern mime_t *rfc822_parse		P_((FILE *, int));
extern mime_t * multipart_parse         P_((FILE *, int, char *, int));
extern void mime_get_boundary           P_((char *, char *, int));

extern void attach_parse		P_((struct header_rec *,FILE *));
extern mime_t *mime_read_header         P_((FILE *, int));
extern mime_t *parse_mime_headers       P_((header_list_ptr,long,long,int));

#endif

/* Used  by bgp_decode.c */

extern int optionally_enter P_((char *, int, int, int, int));
