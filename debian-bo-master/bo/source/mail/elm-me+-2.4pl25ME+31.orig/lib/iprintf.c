/* iprintf.c
 *     
 */

#include "defs.h"
#include "elmlib.h"

const struct charset_state NULL_charset_state = {switch_none,0, 0, 0, 0, 0, 0};
const String NULL_String = { STRING_magic_null, 0, NULL };

/* Notice: Thece values are nothing to do with charset
 *         string constants!
 */

const ELMCHAR  ELMCHAR_space  = 0x0020   /* ;*;SPACE;;; */                  ;
const ELMCHAR  ELMCHAR_zero   = 0x0030   /* ;*;DIGIT ZERO;;; */             ;
const ELMCHAR  ELMCHAR_letA   = 0x0041   /* ;*;LATIN CAPITAL LETTER A;;; */ ;
const ELMCHAR  ELMCHAR_leta   = 0x0061   /* ;*;LATIN SMALL LETTER A;;; */   ;
const ELMCHAR  ELMCHAR_sign_minus = 0x2212 /* ;*;MINUS SIGN;;;; */          ;
const ELMCHAR  ELMCHAR_sign_plus  = 0x002B /* ;*;PLUS SIGN;;;   */          ;
const ELMCHAR  ELMCHAR_bad    = 0xFFFD     /* ;*;REPLACEMENT CHARACTER;;; */ ;

const ELMCHAR  ELMCHAR_hyphen    = 0x2010  /* ;*;HYPHEN;;; */               ;
const ELMCHAR  ELMCHAR_soft_hyphen = 0x00AD /* ;*;SOFT HYPHEN;;; */         ;

const ELMCHAR  ELMCHAR_NB_hyphen   = 0x2011 /* ;*;NON-BREAKING HYPHEN;;;; */ ;
const ELMCHAR  ELMCHAR_NB_space    = 0x00A0
/* ;*;NO-BREAK SPACE;;NON-BREAKING SPACE; <no-break> [0020] <no-break> ;    */;
const  ELMCHAR ELMCHAR_cr          = 0x000D /* ASCII cr */ ;
const  ELMCHAR ELMCHAR_lf          = 0x000A /* ASCII lf */ ;
const  ELMCHAR ELMCHAR_nl          = 0x2028 /* ;*;LINE SEPARATOR;;; */ ;
const  ELMCHAR ELMCHAR_bs          = 0x0008 /* Ascii bs */ ;
const  ELMCHAR ELMCHAR_tab         = 0x0009 /* Ascii tab */ ;
const  ELMCHAR ELMCHAR_display_base  = 0xF000 /* Directly to terminal base */ ;

static struct {
  ELMCHAR base;
  ELMCHAR x1;
} compat_table[] = {
  /* 002D;*;HYPHEN-MINUS;;; */
  { 0x2212, 0x002D },
  { 0x2010, 0x002D },

  /* 2011;*;NON-BREAKING HYPHEN;;; <no-break> [2010] <no-break> */
  { 0x2010, 0x2011 },
  /* 00A0;*;NO-BREAK SPACE;;NON-BREAKING SPACE; <no-break> [0020] <no-break> */
  { 0x0020, 0x0020 },

  /* 2028;*;LINE SEPARATOR;;; */
  { 0x2028, 0x000D },
  { 0x2028, 0x000A }
};

#define EF_in_compat 1
#define EF_ambiquous 2
#define EF_printable 4
#define ELMCHAR_count 0x10000

/* Hmm. (2 + 1 + 1 + 4) * 64 kB = 512 kB (+ dynamically alloced) */
static struct {
  ELMCHAR base;
  unsigned char flags;
  unsigned char fbnum;
  union {
    ELMCHAR c;
    ELMCHAR *arr;
  } fallback;
} property_table[ELMCHAR_count]= { { 0, 0 , 0, 0} };

#define ABORT() do { fprintf(stderr,\
 "\r\nAborting (line %d,file %s)\r\n",__LINE__,__FILE__); abort(); } while(1)

static void add_fallback P_((int,int));
static void add_fallback(idx,c)
     int idx,c;
{
  if (idx < 0 && idx >= ELMCHAR_count)
    ABORT();
  if (c < 0 && c >= ELMCHAR_count)
    abort();
  if (property_table[idx].fbnum == 0) {
    property_table[idx].fallback.c = c;
    property_table[idx].fbnum = 1;
  } else if (property_table[idx].fbnum == 1) {
    ELMCHAR c1 = property_table[idx].fallback.c;
    property_table[idx].fallback.arr = safe_malloc(2 * sizeof (ELMCHAR));
    property_table[idx].fallback.arr[0] = c1;
    property_table[idx].fallback.arr[1] = c;
    property_table[idx].fbnum = 1;
  } else if (property_table[idx].fbnum < 255) {
    property_table[idx].fallback.arr = 
      safe_realloc(property_table[idx].fallback.arr,
		   ( 1 + property_table[idx].fbnum) * sizeof (ELMCHAR));
    property_table[idx].fallback.arr[property_table[idx].fbnum] = c;
    property_table[idx].fbnum ++;
  }    
}

static int get_fallback P_((int idx,ELMCHAR **arr));

static int get_fallback(idx, arr)
     int idx;
     ELMCHAR **arr;
{
  if (idx < 0 && idx >= ELMCHAR_count)
    ABORT();
  if (property_table[idx].fbnum == 0) {
    *arr = NULL;
    return 0;
  } else if (property_table[idx].fbnum == 1) {
    *arr = &property_table[idx].fallback.c;
    return 1;
  } else {
    *arr = property_table[idx].fallback.arr;
    return property_table[idx].fbnum;
  }
}

static struct {
  char c;
  ELMCHAR c1;
} fallback_table[] = {
  { '\r', 0x000D },
  { '\n', 0x000A },
  { '\t', 0x0009 },
  /* This table defines mapping between charset used in string
   * constants and ELMCHAR. This includes only characters, which
   * are likely to be existed in all sets.
   */  
  { ' ', 0x0020        /* ;*;SPACE;;; */            },
  { '!', 0x0021        /* ;*;EXCLAMATION MARK;;; */ },
  { '"', 0x0022        /* ;*;QUOTATION MARK;;; */   },
  { '%', 0x0025        /* ;*;PERCENT SIGN;;; */     },
  { '&', 0x0026        /* ;*;AMPERSAND;;; */        },
  { '(', 0x0028        /* ;*;LEFT PARENTHESIS;;OPENING PARENTHESIS; */ },
  { ')', 0x0029        /* ;*;RIGHT PARENTHESIS;;CLOSING PARENTHESIS; */ },
  { '*', 0x002A        /* ;*;ASTERISK;;; */  },
  { '+', 0x002B        /* ;*;PLUS SIGN;;; */ },
  { ',', 0x002C        /* ;*;COMMA;;;  */ },
  { '-', 0x002D        /* ;*;HYPHEN-MINUS;;; */ },
  { '.', 0x002E        /* ;*;FULL STOP;;PERIOD; */ },
  { '/', 0x002F        /* ;*;SOLIDUS;;SLASH; */ },
  { '0', 0x0030        /* ;*;DIGIT ZERO;;; */ },
  { '1', 0x0031        /* ;*;DIGIT ONE;;; */ },
  { '2', 0x0032        /* ;*;DIGIT TWO;;; */ },
  { '3', 0x0033        /* ;*;DIGIT THREE;;; */ },
  { '4', 0x0034        /* ;*;DIGIT FOUR;;; */ },
  { '5', 0x0035        /* ;*;DIGIT FIVE;;; */ },
  { '6', 0x0036        /* ;*;DIGIT SIX;;; */ },
  { '7', 0x0037        /* ;*;DIGIT SEVEN;;; */ },
  { '8', 0x0038        /* ;*;DIGIT EIGHT;;; */ },
  { '9', 0x0039        /* ;*;DIGIT NINE;;; */ },
  { ':', 0x003A        /* ;*;COLON;;; */ },
  { ';', 0x003B        /* ;*;SEMICOLON;;; */ },
  { '<', 0x003C        /* ;*;LESS-THAN SIGN;;; */ },
  { '=', 0x003D        /* ;*;EQUALS SIGN;;; */ },
  { '>', 0x003E        /* ;*;GREATER-THAN SIGN;;; */ },
  { '?', 0x003F        /* ;*;QUESTION MARK;;; */ },
  { 'A', 0x0041        /* ;*;LATIN CAPITAL LETTER A;;; */ },
  { 'B', 0x0042        /* ;*;LATIN CAPITAL LETTER B;;; */ },
  { 'C', 0x0043        /* ;*;LATIN CAPITAL LETTER C;;; */ },
  { 'D', 0x0044        /* ;*;LATIN CAPITAL LETTER D;;; */ },
  { 'E', 0x0045        /* ;*;LATIN CAPITAL LETTER E;;; */ },
  { 'F', 0x0046        /* ;*;LATIN CAPITAL LETTER F;;; */ },
  { 'G', 0x0047        /* ;*;LATIN CAPITAL LETTER G;;; */ },
  { 'H', 0x0048        /* ;*;LATIN CAPITAL LETTER H;;; */ },
  { 'I', 0x0049        /* ;*;LATIN CAPITAL LETTER I;;; */ },
  { 'J', 0x004A        /* ;*;LATIN CAPITAL LETTER J;;; */ },
  { 'K', 0x004B        /* ;*;LATIN CAPITAL LETTER K;;; */ },
  { 'L', 0x004C        /* ;*;LATIN CAPITAL LETTER L;;; */ },
  { 'M', 0x004D        /* ;*;LATIN CAPITAL LETTER M;;; */ },
  { 'N', 0x004E        /* ;*;LATIN CAPITAL LETTER N;;; */ },
  { 'O', 0x004F        /* ;*;LATIN CAPITAL LETTER O;;; */ },
  { 'P', 0x0050        /* ;*;LATIN CAPITAL LETTER P;;; */ },
  { 'Q', 0x0051        /* ;*;LATIN CAPITAL LETTER Q;;; */ },
  { 'R', 0x0052        /* ;*;LATIN CAPITAL LETTER R;;; */ },
  { 'S', 0x0053        /* ;*;LATIN CAPITAL LETTER S;;; */ },
  { 'T', 0x0054        /* ;*;LATIN CAPITAL LETTER T;;; */ },
  { 'U', 0x0055        /* ;*;LATIN CAPITAL LETTER U;;; */ },
  { 'V', 0x0056        /* ;*;LATIN CAPITAL LETTER V;;; */ },
  { 'W', 0x0057        /* ;*;LATIN CAPITAL LETTER W;;; */ },
  { 'X', 0x0058        /* ;*;LATIN CAPITAL LETTER X;;; */ },
  { 'Y', 0x0059        /* ;*;LATIN CAPITAL LETTER Y;;; */ },
  { 'Z', 0x005A        /* ;*;LATIN CAPITAL LETTER Z;;; */ },

  { 'a', 0x0061        /* ;*;LATIN SMALL LETTER A;;; */ },
  { 'b', 0x0062        /* ;*;LATIN SMALL LETTER B;;; */ },
  { 'c', 0x0063        /* ;*;LATIN SMALL LETTER C;;; */ },
  { 'd', 0x0064        /* ;*;LATIN SMALL LETTER D;;; */ },
  { 'e', 0x0065        /* ;*;LATIN SMALL LETTER E;;; */ },
  { 'f', 0x0066        /* ;*;LATIN SMALL LETTER F;;; */ },
  { 'g', 0x0067        /* ;*;LATIN SMALL LETTER G;;; */ },
  { 'h', 0x0068        /* ;*;LATIN SMALL LETTER H;;; */ },
  { 'i', 0x0069        /* ;*;LATIN SMALL LETTER I;;; */ },
  { 'j', 0x006A        /* ;*;LATIN SMALL LETTER J;;; */ },
  { 'k', 0x006B        /* ;*;LATIN SMALL LETTER K;;; */ },
  { 'l', 0x006C        /* ;*;LATIN SMALL LETTER L;;; */ },
  { 'm', 0x006D        /* ;*;LATIN SMALL LETTER M;;; */ },
  { 'n', 0x006E        /* ;*;LATIN SMALL LETTER N;;; */ },
  { 'o', 0x006F        /* ;*;LATIN SMALL LETTER O;;; */ },
  { 'p', 0x0070        /* ;*;LATIN SMALL LETTER P;;; */ },
  { 'q', 0x0071        /* ;*;LATIN SMALL LETTER Q;;; */ },
  { 'r', 0x0072        /* ;*;LATIN SMALL LETTER R;;; */ },
  { 's', 0x0073        /* ;*;LATIN SMALL LETTER S;;; */ },
  { 't', 0x0074        /* ;*;LATIN SMALL LETTER T;;; */ },
  { 'u', 0x0075        /* ;*;LATIN SMALL LETTER U;;; */ },
  { 'v', 0x0076        /* ;*;LATIN SMALL LETTER V;;; */ },
  { 'w', 0x0077        /* ;*;LATIN SMALL LETTER W;;; */ },
  { 'x', 0x0078        /* ;*;LATIN SMALL LETTER X;;; */ },
  { 'y', 0x0079        /* ;*;LATIN SMALL LETTER Y;;; */ },
  { 'z', 0x007A        /* ;*;LATIN SMALL LETTER Z;;; */ }

};

static char to_fallback_cs[ELMCHAR_count]= { 0 };
static ELMCHAR from_fallback_cs[0x100] = { 0 };
static int initialized = 0;

void init_ELMCHAR() {
  int i;

  for (i = 0; i < ELMCHAR_count; i++) {
    property_table[i].flags  = 0;
    property_table[i].base   = 0;
    property_table[i].fbnum  = 0;   
    to_fallback_cs[i]        = '\0';
  }

  for (i = 0; i < sizeof compat_table / sizeof compat_table[0]; i++) {
    int j;
    property_table[compat_table[i].base].flags |= EF_in_compat;
    property_table[compat_table[i].x1].flags   |= EF_in_compat;
    if (property_table[compat_table[i].x1].base) 
      property_table[compat_table[i].x1].flags   |= EF_ambiquous;
    else
      property_table[compat_table[i].x1].base  = compat_table[i].base;

    add_fallback(compat_table[i].base,compat_table[i].x1);
  }

  for (i = 0; i < 0x100; i++)
    from_fallback_cs[i] = ELMCHAR_bad;

  for (i = 0; i < sizeof fallback_table / sizeof fallback_table[0]; i++) {
    to_fallback_cs[fallback_table[i].c1] =  fallback_table[i].c;
    from_fallback_cs[fallback_table[i].c]  =  fallback_table[i].c1;
  }

  /* 0020	007E	BASIC LATIN */
  for (i = 0x0020; i <= 0x007E; i++)
    property_table[i].flags |= EF_printable;

  /* 0100	017F	LATIN EXTENDED-A */
  for (i = 0x0100; i <= 0x017F; i++)
    property_table[i].flags |= EF_printable;

  initialized = 1;
}


#define REQ(f) { if (!initialized) { fpritnf(stderr,\
"\r\n%s: init_ELMCHAR() not called!\r\n",f); abort(); } }

int is_ELMCHAR(c) 
     int c; 
{ 
  REQ("is_ELMCHAR"); 
  return c >= 0 && c < ELMCHAR_count; 
}

int ELMCHAR_is_printable (c1)
     int c1;
{
  REQ("ELMCHAR_is_printable"); 

  if (!is_ELMCHAR(c1)) {
    fprintf(stderr,
	    "\r\nELMCHAR_is_printable(0x%04X,0x%04X): Bad argument\r\n");
    abort();
    return 0;
  }

  if (c1 >= ELMCHAR_display_base && c1 < ELMCHAR_display_base + 256) {
    /* Directly to display */
    int ch = c1 - ELMCHAR_display_base;

    if(
#ifdef ASCII_CTYPE
    !isascii(ch) || !isprint(ch)
#else
    !isprint(ch)
#endif
    ) return 0;
    return 1;
  }
  
  if (property_table[c1].flags | EF_printable)
    return 1;

  return 0;
}

int ELMCHAR_compat (c1,c2) 
     int c1,c2;
{
  int i;

  REQ("ELMCHAR_compat"); 

  if (!is_ELMCHAR(c1) || !is_ELMCHAR(c2)) {
    fprintf(stderr,"\r\nELMCHAR_compat(0x%04X,0x%04X): Bad arguments\r\n");
    abort();
    return 0;
  }

  if (c1 == c2)
    return 1;

  if (!c1 || !c2)
    return 0;

  if (!(property_table[c1].flags & EF_in_compat) ||
      !(property_table[c2].flags & EF_in_compat))
    return 0;

  if (c1 == property_table[c2].base ||
      c2 == property_table[c1].base)
    return 1;

  if ((property_table[c1].flags & EF_ambiquous) ||
      (property_table[c2].flags & EF_ambiquous)) {
    int i;

    for (i = 0; i < sizeof compat_table / sizeof compat_table[0]; i++) {

      if (c1 == compat_table[i].base &&
	  c2 == compat_table[i].x1)
	return 1;
      if (c2 == compat_table[i].base &&
	  c1 == compat_table[i].x1)
	return 1;
    }
  }

  return 0;
}

void STRING__append_char (str,c1)
     String *str;
     int c1;
{  
  ELMCHAR c = c1;
  REQ("STRING__append_char"); 
  switch (str->magic) { 
    ELMCHAR *tmp;
    int i;
  default:   fprintf(stderr,
		     "\r\nSTRING__append_char: bad magic %d\r\n", 
		     str->magic); abort(); break; 
  case STRING_magic_alloced: 
    str->str = safe_realloc(str->str,(str->len+1)*sizeof (ELMCHAR));
    str->str[str->len]=c;
    str->len++;
    break;
  case STRING_magic_static: 
    tmp = safe_malloc((str->len+1)*sizeof (ELMCHAR));
    for (i = 0; i < str->len; i++)
      tmp[i] = str->str[i];
    tmp[str->len]=c;
    str->str=tmp;
    str->len++;
    str->magic = STRING_magic_alloced;
    break;
  case STRING_magic_null: 
    str->str = safe_malloc(sizeof (ELMCHAR));
    str->str[0] = c;
    str->len = 1;
    str->magic = STRING_magic_alloced;
    break;
  }
}

void STRING__append_string (str,a) 
     String *str; 
     String *a;
{
  ELMCHAR * l;
  int len;

  REQ("STRING__append_string"); 
  switch (a->magic) { 
    ELMCHAR *tmp;
    int i;
  default:   fprintf(stderr,
		     "\r\nSTRING__append_string: bad magic %d\r\n", 
		     str->magic); abort(); break; 
  case STRING_magic_alloced: 
  case STRING_magic_static: 
    l   = a -> str;
    len = a -> len;
    break;
  case STRING_magic_null: 
    l = NULL;
    len = 0;
    break;
  }

  switch (str->magic) { 
    ELMCHAR *tmp;
    int i;
  default:   fprintf(stderr,
		     "\r\nSTRING__append_string: bad magic %d\r\n", 
		     str->magic); abort(); break; 
  case STRING_magic_alloced: 
    str->str = safe_realloc(str->str,(str->len+len)*sizeof (ELMCHAR));
    for (i = 0; i < len; i++)
      str->str[str->len + i] = l[i];
    str->len += len;
    break;
  case STRING_magic_static: 
    tmp = safe_malloc((str->len+len)*sizeof (ELMCHAR));
    for (i = 0; i < str->len; i++)
      tmp[i] = str->str[i];
    for (i = 0; i < len; i++)
      str->str[str->len + i] = l[i];
    str->str=tmp;
    str->len += len;
    str->magic = STRING_magic_alloced;
    break;
  case STRING_magic_null: 
    if (0 == len)
      break;
    str->str = safe_malloc(len * sizeof (ELMCHAR));
    for (i = 0; i < len; i++)
      str->str[i] = l[i];
    str->len = len;
    str->magic = STRING_magic_alloced;
    break;
  }
}

void STRING__cut (str,len)
     String *str; 
     int len;
{
  REQ("STRING__cut"); 
  if (len < 0) {
    fprintf(stderr,
	    "\r\nSTRING__cut: bad len %d\r\n", 
	    len); abort(); 
  }

  switch (str->magic) { 
    ELMCHAR *tmp;
    int i;
  default:   fprintf(stderr,
		     "\r\nSTRING__cut: bad magic %d\r\n", 
		     str->magic); abort(); break; 
  case STRING_magic_alloced: 
  case STRING_magic_static: 
    if (len < str->len)
      str->len = len;
    break;
  case STRING_magic_null: 
    break;
  }
}

int set_state  (struct charset_info *info,
		charset_state_t *state,
		enum switch_system system) {
  int ret = info->init_charset(info,state,system);

  if (!ret)
    return ret;

  state -> system = system;
  return ret;
}


static int  from_fallback_charset P_((struct charset_info *info,
				      charset_state_t *state,
				      String *outbuf,
				      const char *inbuf, int len));

static int  to_fallback_charset P_((struct charset_info *info,
				    charset_state_t *state,
				    char *output, int size, int *reslen,
				    String *inbuf, int start));

static int fallback_state_hook  P_((struct charset_info *info,
				    charset_state_t *state,
				    enum switch_system system));

static int  from_display_charset P_((struct charset_info *info,
				   charset_state_t *state,
				   String *outbuf,
				   const char *inbuf, int len));

static int  to_display_charset P_((struct charset_info *info,
				   charset_state_t *state,
				   char *output, int size, int *reslen,
				   String *inbuf, int start));

static int display_state_hook  P_((struct charset_info *info,
				    charset_state_t *state,
				    enum switch_system system));


static struct charset_info fallback_charset_s = {
  CHARSET_INFO_magic,
  "Fallback charset",
  0,
  from_fallback_charset,
  to_fallback_charset,
  fallback_state_hook
};

struct charset_info display_charset_s = {
  CHARSET_INFO_magic,
  "Display charset",
  0,
  from_display_charset,
  to_display_charset,
  display_state_hook
};

/* For some ANSI C compilers 'char *' and 'unsigned char *' are incompatible
 * types -- we don't use casting macro here because it disables type checking
 * altogether:
 */
#if __STDC__
static unsigned const char * uns_str(const char * str) { 
  return (unsigned const char *)str; 
}
#else
#define std_str(str)  (str)
#endif

static int  from_fallback_charset (info,state,outbuf,inbuf,len)
     struct charset_info *info;
     charset_state_t *state;
     String *outbuf;
     const char *inbuf;
     int len;
{
  unsigned const char * input = uns_str(inbuf);
  int i = 0;
  REQ("from_fallback_charset"); 
  if (inbuf) {
    for (i = 0; i < len; i++) {
      unsigned const char c = input[i];
      ELMCHAR res = from_fallback_cs[c];

      if (state->lookahead == '\r' && '\n' == c) {
	res = ELMCHAR_nl;
	state->lookahead = 0;
      } 

      if (state->lookahead == '\r') {
	STRING_APPEND_CHAR(*outbuf, ELMCHAR_cr);
	state->lookahead = 0;
      }

      if ('\r' == c) {
	state->lookahead = '\r';
	continue;
      }

      if (!res)
	res = ELMCHAR_bad;
      
      STRING_APPEND_CHAR(*outbuf, res);
    }
  } else {
    if (state->lookahead == '\r') {
      STRING_APPEND_CHAR(*outbuf, ELMCHAR_cr);
      state->lookahead = 0;
    }
  }
  return i;
}

static int  to_fallback_charset (info,state,output,size,reslen,
				 inbuf,start)
     struct charset_info *info;
     charset_state_t *state;
     char *output;
     int size; 
     int *reslen;
     String *inbuf;
     int start;
{
  int i = 0;
  int idx = 0;

  REQ("to_fallback_charset"); 

  if (inbuf) {
    for (i = start; 
	 i < inbuf -> len &&
	   idx < size;
	 i++) {
      ELMCHAR c = inbuf -> str[i];
      unsigned char res = to_fallback_cs[c];
  
      if (!res && c == ELMCHAR_bad)
	res = '?';
      
      if (!res) {
	ELMCHAR * arr;
	int count = get_fallback(c,&arr);
	int j;
	for (j = 0; j < count; j++) {
	  if (to_fallback_cs[arr[j]]) {
	    res = to_fallback_cs[arr[j]];
	    break;
	  }
	}
      }
      
      if (!res) res = '_';
      output[idx++] = res;
    }
  }
  *reslen = idx;
  return i;
}

enum display_charset_options display_charset_option = display_cs_raw;

static int fallback_state_hook  (info,state,system)
     struct charset_info *info;
     charset_state_t *state;
     enum switch_system system;
{
  return 0;
}

static int  from_display_charset (info,state,outbuf,inbuf,len)
     struct charset_info *info;
     charset_state_t *state;
     String *outbuf;
     const char *inbuf; 
     int len;
{
  unsigned const char * input = uns_str(inbuf);
  int i = 0;
  REQ("from_display_charset"); 
  if (inbuf) {
    for (i = 0; i < len; i++) {
      unsigned const char c = input[i];
      ELMCHAR res = 0;

      switch (display_charset_option) {
      case display_cs_raw:
	break;
      case display_cs_ascii:
	if ( c < 128)
	  res = c;
	break;
      case display_cs_fallback:
	res = from_fallback_cs[c];
	break;
      }

      /* Handle \r and \n always as in fallback charset */
      if (state->lookahead == '\r' && '\n' == c) {
	res = ELMCHAR_nl;
	state->lookahead = 0;
      } 
      
      if (state->lookahead == '\r') {
	STRING_APPEND_CHAR(*outbuf, ELMCHAR_cr);
	state->lookahead = 0;
      }
      if ('\r' == c) {
	state->lookahead = '\r';
	continue;
      }
      if ('\n' == c)
	res = ELMCHAR_lf;
      
      if (!res)
	res = ELMCHAR_display_base + c;

      STRING_APPEND_CHAR(*outbuf, res);      
    }
  } else {
    if (state->lookahead == '\r') {
      STRING_APPEND_CHAR(*outbuf, ELMCHAR_cr);
      state->lookahead = 0;
    }
  }
}

static int  to_display_charset (info,state,outbuf,size,reslen,inbuf,start)
     struct charset_info *info;
     charset_state_t *state;
     char *outbuf;
     int size;
     int *reslen;
     String *inbuf; 
     int start;
{
  int i = 0;
  int idx = 0;
  if (inbuf) {
    for (i = start; 
	 i < inbuf -> len &&
	   idx < size;
	 i++) {
      ELMCHAR c = inbuf -> str[i];
      unsigned char res = 0;

      switch (display_charset_option) {
      case display_cs_raw:
	if ( c >= ELMCHAR_display_base && c < ELMCHAR_display_base + 256)
	  res = c - ELMCHAR_display_base;
	break;
      case display_cs_ascii:
	if ( c < 128)
	  res = c;
	break;
      case display_cs_fallback:
	res = to_fallback_cs[c];
	break;
      }
      
      if (!res && c == ELMCHAR_bad)
	res = '?';
      
      if (!res) {
	ELMCHAR * arr;
	int count = get_fallback(c,&arr);
	int j;
	for (j = 0; j < count; j++) {
	  ELMCHAR c1 = arr[j];

	  switch (display_charset_option) {
	  case display_cs_raw:
	    if ( c1 >= ELMCHAR_display_base && c1 < ELMCHAR_display_base + 256)
	      res = c1 - ELMCHAR_display_base;
	    break;
	  case display_cs_ascii:
	    if ( c1 < 128)
	      res = c1;
	    break;
	  case display_cs_fallback:
	    res = to_fallback_cs[c1];
	    break;
	  }
	  if (res)
	    break;	
	}
      }      
      if (!res) res = '_';
      outbuf[idx++] = res;
    }
  }
  *reslen = idx;
  return i;
}

static int display_state_hook  (info,state,system)
     struct charset_info *info;
     charset_state_t *state;
     enum switch_system system;
{
  return 0;
}


/* Following specifies what is charset used in character
 *       string constans!
 */

const charset_t   CODE_charset = &fallback_charset_s;

int viprintf P_((void *ref, printfunc *func,
		 nl_info_t cati, int set_num,
		 const char *format, va_list args));

int iprintf P_((void *ref, printfunc *func,
		 nl_info_t cati, int set_num,
		 const char *format, ...));

int iprintf (
#if ANSI_C
	     void *ref, printfunc *func,
	     nl_info_t cati, int set_num,
	     const char *format, ...
#else
	     ref, func, catd, set_num,
	     format, va_alist
#endif
	     )
#if !ANSI_C
     void *ref;
     printfunc *func;     
     nl_cat_t cati;
     int set_num;
     const char *format;
     va_dcl
#endif
{
  int ret;
  va_list vl;
  REQ("iprintf"); 

  Va_start(vl, format);           /* defined in defs.h */
  ret = viprintf(ref,func,cati,set_num,format,vl);
  va_end(vl);
  return ret;
}

int viprintf (ref,func,cati,set_num,format,args)
     void *ref;
     printfunc *func;
     nl_info_t cati;
     int set_num;
     const char *format;
     va_list args;
{
  String * string_vector = NULL;
  int argc = 0, i;
  int bad = 0;
  int count = 0;

  charset_t def_charset = CODE_charset;
  const char *ptr;

  REQ("viprintf"); 

  for (ptr = format; *ptr; ptr++) {
    charset_t string_charset = def_charset;

    int num = 0;
    ELMCHAR fill = ELMCHAR_space;
    ELMCHAR sign = 0;
    int leftjust = 0;
    int alternate = 0;
    const char * oldptr;
    int width     = 0;
    int precision = 0;
    enum { short_f, long_f, norm_f } flag = norm_f;
    String value;

    STRING_INIT(value);

    if (*ptr != '%')
      continue;

    if (*(ptr+1) == '%') {
      ptr++;
      continue;
    }

    do {  
      oldptr = ptr;

      if (*ptr == '0') {
	fill = ELMCHAR_zero;
	ptr++;
      } else if (*ptr == ' ') {
	fill = ELMCHAR_space;
	ptr++;
      } else if (*ptr == '+') {
	sign = ELMCHAR_sign_plus;
	ptr++;
      } else if (*ptr == '-') {
	leftjust++;
	ptr++;
      } else if (*ptr == '#') {
	alternate++;
	ptr++;
      } else if (*ptr == '!') {
	string_charset = va_arg(args,charset_t);
	ptr++; 
     }
    } while(oldptr != ptr);

    num = 0;
    if (*ptr == '*') {
      num = va_arg(args,int);
      ptr++;
    } else while (*ptr >= '0' && *ptr <= '9') {
      num = 10 * num + *ptr - '0';
      ptr++;
    } 
    if (num < 0) {
      leftjust++;
      num = -num;
    }
    width = num;
    if (*ptr == '.') {
      num = 0;
      ptr++;

      if (*ptr == '*') {
	num = va_arg(args,int);
	ptr++;
      } else while (*ptr >= '0' && *ptr <= '9') {
	num = 10 * num + *ptr - '0';
	ptr++;
      } 
      if (num < 0)
	num = 0;
      precision = num;
    }

    if (*ptr == 'h') {
      flag = short_f;
      ptr++;
    } else if (*ptr == 'l') {
      flag = long_f;
      ptr++;
    }

    switch (*ptr) {
      long i_val;
      unsigned long u_val;
      double f_val;
#ifdef _WCHAR_T
      wchar_t w_val;
      wchar_t * ws_val;
#endif
      char * s_val;
      void * p_val;
      String * S_val;
      ELMCHAR e_val;
    case '$':
    case '\0':
    default:
      bad = 1;
      goto bail_out;

    case 'd':
    case 'i':
    case 'c':
      if (flag == long_f) i_val = va_arg(args, long int);
      else                i_val = va_arg(args, int);

      switch (*ptr) {
	unsigned char buf[2];
	charset_state_t st;
	int ret,cnt;
	unsigned long base;
      case 'c':
	st = NULL_charset_state;
	buf[0] = i_val;
	buf[1] = '\0';

	ret = string_charset->from_charset(string_charset,
				     &st,
				     &value,
				     buf,1);

	if (ret != 1) {
	  STRING_APPEND_CHR(value,ELMCHAR_bad);
	  bad = 1;
	} else {
	  String tmp;
	  STRING_INIT(tmp);

	  string_charset->from_charset(string_charset,
				       &st,
				       &tmp,
				       NULL,0);
	  STRING_APPEND_STRING(value,tmp);

	  STRING_FREE(tmp);
	}
	break;
      case 'd':
      case 'i':
	if (i_val < 0) {
	  i_val = - i_val;
	  sign = ELMCHAR_sign_minus;
	}
	
	for (base = 1,cnt = 1; 
	     (i_val / base >= 10 ||
	      cnt < precision) &&
	       base * 10 > base;
	     base *= 10, cnt++);

	while (cnt > 0) {
	  unsigned long div = i_val / base;
	  ELMCHAR c = ELMCHAR_zero + div;

	  i_val -= div * base;
	  cnt--;
	  base /= 10;
	  STRING_APPEND_CHR(value,c);
	}
	if (0 != i_val)
	  STRING_APPEND_CHR(value,ELMCHAR_bad);
	bad = 1;
      }
      break;
    case 'o':
    case 'u':
    case 'x':
    case 'X':
      if (flag == long_f) u_val = va_arg(args, unsigned long int);
      else                u_val = va_arg(args, unsigned int);

      { 
	unsigned long base, base1;
	int upc = 0,cnt;
	
	switch (*ptr) {
	case 'u': base1 = 10; break;
	case 'x': base1 = 16; break;
	case 'X': base1 = 16; upc = 1; break;
	case 'o': base1 = 8; break;
	}

	for (base = 1,cnt = 1; 
	     (u_val / base >= base1 ||
	      cnt < precision) &&
	       base * base1 > base;
	     base *= base1, cnt++);

	while (cnt > 0) {
	  unsigned long div = u_val / base;
	  ELMCHAR c;
	  /* Ordering of Unicode assumed in here */
	  if (div < 0)   c = ELMCHAR_zero + div;
	  else if (upc)  c = ELMCHAR_letA + div - 10;
	  else           c = ELMCHAR_leta + div - 10;

	  u_val -= div * base1;
	  cnt--;
	  base /= base1;
	  STRING_APPEND_CHR(value,c);
	}
	if (0 != u_val)
	  STRING_APPEND_CHR(value,ELMCHAR_bad);
	bad = 1;
      }
      break;
    case 'f':
    case 'e':
    case 'E':
    case 'g':
      f_val = va_arg(args, double);

      break;
#ifdef _WCHAR_T
    case 'C':
      w_val = va_arg(args, wchar_t);

      break;
#endif
    case 's':
      s_val = va_arg(args, char *);
      if (s_val) {
	charset_state_t st;
	int ret,cnt, len = strlen(s_val);	

	String tmp;
	STRING_INIT(tmp);
	st = NULL_charset_state;

	for (cnt = 0; cnt < len; cnt += ret) {
	  ret = string_charset->from_charset(string_charset,
					     &st,
					     &tmp,
					     s_val+cnt,len-cnt);
	
	  STRING_APPEND_STRING(value,tmp);
	
	  STRING_FREE(tmp);

	  if (ret < 1)
	    break;
	}

	if (cnt < len) {
	  STRING_APPEND_CHR(value,ELMCHAR_bad);
	  bad = 1;
	} else {
 	  string_charset->from_charset(string_charset,
				       &st,
				       &tmp,
				       NULL,0);
	  STRING_APPEND_STRING(value,tmp);

	  STRING_FREE(tmp);

	  if (precision > 0)
	    STRING_CUT(value,precision);
	}
      } else {
	STRING_APPEND_CHR(value,ELMCHAR_bad);
	bad = 1;
      }
      break;
#ifdef _WCHAR_T
    case 'S':
      ws_val = va_arg(args, wchar_t *);

      break;
#endif
    case 'p':
      p_val = va_arg(args, void *);

      break;
    case '&':
      ptr++;
      switch (*ptr) {
      case '\0':
      default:
	bad = 1;
	goto bail_out;

      case 'S':
	S_val = va_arg(args, String *);

	if (NULL == S_val) {
	  STRING_APPEND_CHR(value,ELMCHAR_bad);
	  bad = 1;
	} else {
	  STRING_APPEND_STRING(value,*S_val);

	  if (precision > 0)
	    STRING_CUT(value,precision);
	}
	break;
      case 'C':
	e_val = va_arg(args, unsigned int);

	STRING_APPEND_CHR(value,e_val);

	break;
      }
      break;
    }

    string_vector = safe_realloc(string_vector,(argc +1) * sizeof (String));
    STRING_INIT(string_vector[argc]);
    STRING_APPEND_STRING(string_vector[argc],value);
    STRING_FREE(value);
    argc++;
  }

  {
    charset_t format_charset = CODE_charset;
    const char * nls_text = cati ? 
      catgets(cati->catd,set_num,format) :
      format ;
    const char *ptr;

    String value;
    
    int lastpos = 0, realpos;

    if (nls_text != format)
      format_charset = cati -> nl_charset;

    STRING_INIT(value);

    for (ptr = nls_text; *ptr; ptr++) {
      int pos = 0;
      const char *oldptr;
      
      /* special meaning in format */
      if (*ptr == '\n'
	  || *ptr == '\r' && *(ptr+1) == '\n') {  
	if (*ptr == '\r') ptr++;
	ptr++;

	STRING_APPEND_CHAR(value,ELMCHAR_nl);
	func(ref,&value);
	count += value.len;
	STRING_FREE(value);
	continue;	
      }

      if (*ptr != '%' || *(ptr+1) == '%') {
	const char * mark = ptr;
	charset_state_t st = NULL_charset_state;
	int cnt = 0, ret, len;
	String tmp;
	STRING_INIT(tmp);

	if (*(ptr+1) == '%') {
	  ptr++;
	  mark = ptr;
	}
	while (*(ptr+1) != '%' && *(ptr+1) != '\0' &&
	       *(ptr+1) != '\n' && *(ptr+1) != '\r')
	  ptr++;
	len = ptr - mark + 1;

	for (cnt = 0; cnt < len; cnt += ret) {
	  ret = format_charset->from_charset(format_charset,
					     &st,
					     &tmp,
					     mark+cnt,len-cnt);
	  STRING_APPEND_STRING(value,tmp);
	  
	  STRING_FREE(tmp);
	  
	  if (ret < 1)
	    break;
	}

	if (cnt < len) {
	  STRING_APPEND_CHR(value,ELMCHAR_bad);
	  bad = 1;
	} else {
 	  format_charset->from_charset(format_charset,
				       &st,
				       &tmp,
				       NULL,0);
	  STRING_APPEND_STRING(value,tmp);
	  
	  STRING_FREE(tmp);
	}

	func(ref,&value);
	count += value.len;
	STRING_FREE(value);
	continue;
      }

      do {  
	oldptr = ptr;
	
	if ('0' <= *ptr ||
	    *ptr <= '9') {
	  pos = 10 * pos + *ptr - '0';
	  ptr++;
	}
	
      } while(oldptr != ptr);

      if ('$' == *ptr) {
	ptr++;
      } else
	pos = lastpos + 1;

      if (pos < 0 || pos >= argc) {
	STRING_APPEND_CHR(value,ELMCHAR_bad);
	bad = 1;
      } else {
	STRING_APPEND_STRING(value,string_vector[pos]);
	lastpos = pos;
      }

      while (*ptr == '.' ||
	     *ptr >= '0' && *ptr <= '9' ||
	     *ptr == 'l' || *ptr == 'h' ||
	     *ptr == '&' || *ptr == '*' ||
	     *ptr == '!' || *ptr == '-' ||
	     *ptr == '+' || *ptr == ' ') 
	ptr++;

      if (! (*ptr >= 'a' && *ptr <= 'z') &&
	  ! (*ptr >= 'A' && *ptr <= 'Z')) {
	bad = 1;
	STRING_APPEND_CHR(value,ELMCHAR_bad);
      }

      func(ref,&value);
      count += value.len;
      STRING_FREE(value);
    }
  }

  bail_out:
    if (string_vector) {
      for (i = 0; i < argc; i++)
	STRING_FREE(string_vector[i]);
      free(string_vector);
    }

    if (bad)
      return -1;
    return count;
}

int vsiprintf P_((char *s, int size, charset_t cset,
		  nl_info_t cati, int set_num,
		  const char *format, va_list args));

int siprintf P_((char *s, int size, charset_t cset,
		 nl_info_t cati, int set_num,
		 const char *format, ...));


int siprintf (
#if ANSI_C
	      char *s, int size, charset_t cset,
	      nl_info_t cati, int set_num,
	      const char *format, ...
#else
	      s, size, cset, cati, set_num,
	      format, va_alist
#endif
	      )
#if !ANSI_C
	      char *s;
	      int size;
	      charset_t cset;
	      nl_info_t cati; 
	      int set_num;
	      const char *format;
	      va_dcl
#endif
{
  int ret;
  va_list vl;

  REQ("siprintf"); 
  Va_start(vl, format);           /* defined in defs.h */
  ret = vsiprintf(s,size,cset,cati,set_num,format,vl);
  va_end(vl);
  return ret;
}

int sf_printf P_((char *s, int size, const char *format, ...));

int sf_printf (
#if ANSI_C
	       char *s, int size, const char *format, ...
#else
	       s, size, format, va_alist
#endif
	       )
#if !ANSI_C
     char *s;
     int size; 
     const char *format;
     va_dcl
#endif
{
  int ret;
  va_list vl;

  REQ("sf_printf"); 
  Va_start(vl, format);           /* defined in defs.h */
  ret = vsiprintf(s,size,CODE_charset,NULL,-1,format,vl);
  va_end(vl);
  return ret;
}

#define MAGIC_siprintf        0xFC05

struct siprintf_refer {
  int magic;
  char *s;
  int size;
  int pos;
  charset_t cset;
  charset_state_t st;
};

static void siprintfunc P_((void *ref, String *str));

static void siprintfunc (ref, str) 
     void *ref;
     String *str;
{
  struct siprintf_refer *r = (struct siprintf_refer *)ref;
  int ret;
  int reslen;
  int count;

  if (r->magic != MAGIC_siprintf) {
    fprintf(stderr,
	    "\r\nsiprintfunc: bad magic (file %s, line %d) %d\r\n", 
	    __FILE__,__LINE__,r->magic); 
    abort();
    return;
  }

  if (r->pos < 0) {
    fprintf(stderr,
	    "\r\nsiprintfunc: bad pos (file %s, line %d) %d\r\n", 
	    __FILE__,__LINE__,r->pos); 
    abort();
    return;
  }

  if (r->pos >= r->size -1)
    return;
  
  
  for (count = 0; count < str->len; count += ret) {
    reslen = 0;
    ret = r -> cset -> to_charset(r -> cset, &(r -> st),
				  r -> s    + r -> pos,
				  r -> size - r -> pos -1,
				  &reslen, 
				  str,count);
    r -> pos += reslen;
    r -> s [r -> pos] = '\0';
    if (ret <= 0)
      break;
  }
}

int vsiprintf (s,size,cset,cati,set_num,format,args)
     char *s; 
     int size;
     charset_t cset;
     nl_info_t cati; 
     int set_num;
     const char *format;
     va_list args;
{
  int ret;

  struct siprintf_refer r;
  r.magic = MAGIC_siprintf;
  r.s     = s;
  r.size  = size;
  r.pos   = 0;
  r.cset  = cset;
  r.st    = NULL_charset_state;

  REQ("vsiprintf"); 

  ret = viprintf(&r,siprintfunc,cati,set_num,format,args);

  if (r.pos < size-1) {
    int ret2;
    int reslen = 0;

    ret2 = cset->to_charset(cset,&r.st,s + r.pos, size - r.pos -1,
			    &reslen, NULL, 0);
    r.pos += reslen;
    s[r.pos] = '\0';
    if (ret2 < 0)
      return ret2;
  }
  if (ret < 0)
    return ret;

  return r.pos;
}


int vSprintf P_((String *s,
		 nl_info_t cati, int set_num,
		 const char *format, va_list args));

int Sprintf P_((String *s,
		nl_info_t cati, int set_num,
		const char *format, ...));
   

int Sprintf (
#if ANSI_C
	     String *s, nl_info_t cati, int set_num,
	     const char *format, ...
#else
	     s,cati,set_num,format, va_alist
#endif
	     )
#if !ANSI_C
	     String *s;
	     nl_info_t cati;
	     int set_num;
	     const char *format;
	     va_dcl
#endif
{
  int ret;
  va_list vl;

  REQ("Sprintf"); 
  Va_start(vl, format);           /* defined in defs.h */
  ret = vSprintf(s,cati,set_num,format,vl);
  va_end(vl);
  return ret;
}

#define MAGIC_Sprintf        0xFC06

struct Sprintf_refer {
  int magic;
  String *s;
};

static void Sprintfunc P_((void *ref, String *str));

static void Sprintfunc (ref,str)
     void *ref; 
     String *str;
{
  struct Sprintf_refer * r = (struct Sprintf_refer *)ref;

  if (r -> magic != MAGIC_Sprintf) {
    fprintf(stderr,
	    "\r\nSprintfunc: bad magic (file %s, line %d) %d\r\n", 
	    __FILE__,__LINE__,r->magic); 
    abort();
    return;
  } 
  STRING_APPEND_STRING(* (r->s),*str);
}

int vSprintf (s,cati,set_num,format,args)
     String *s;
     nl_info_t cati;
     int set_num;
     const char *format;
     va_list args;
{
  int ret;

  struct Sprintf_refer r;
  r.magic = MAGIC_Sprintf;
  r.s     = s;

  REQ("vSprintf"); 
  ret = viprintf(&r,Sprintfunc,cati,set_num,format,args);

  if (ret < 0)
    return ret;
  return s->len;
}
