/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -acCgopt -k1,2,5,$ keyword.gperf  */
/* `strncmp' is used for comparison.  */
#include <string.h>

/* This file defines `enum token'.  */
#include "token.h"
struct locale_keyword { char *name; enum token token_id; };

#define TOTAL_KEYWORDS 68
#define MIN_WORD_LENGTH 3
#define MAX_WORD_LENGTH 17
#define MIN_HASH_VALUE 3
#define MAX_HASH_VALUE 145
/* maximum key range = 143, duplicates = 0 */

#ifdef __GNUC__
inline
#endif
static unsigned int
hash (register const char *str, register int len)
{
  static const unsigned char asso_values[] =
    {
     146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
     146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
     146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
     146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
     146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
     146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
     146, 146, 146, 146, 146, 146, 146,   0,   0,   0,
       0,   0, 146,   0, 146, 146,   0, 146,   0,  15,
     146, 146,   0,   0,   0,   5, 146, 146, 146,   0,
     146, 146, 146, 146, 146,   5, 146,  30,   0,  45,
      30,   5,  55,  10,  45,   0, 146,  60,  15,  15,
      10,   0,   0, 146,  15,  45,   0,  65, 146,  15,
      15,  30, 146, 146, 146, 146, 146, 146,
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 5:
        hval += asso_values[str[4]];
      case 4:
      case 3:
      case 2:
        hval += asso_values[str[1]];
      case 1:
        hval += asso_values[str[0]];
    }
  return hval + asso_values[str[len - 1]];
}

#ifdef __GNUC__
inline
#endif
const struct locale_keyword *
in_word_set (register const char *str, register int len)
{
  static const struct locale_keyword wordlist[] =
    {
      {"",}, {"",}, {"",}, 
      {"END",                TOK_END},
      {"",}, {"",}, 
      {"IGNORE",             TOK_IGNORE},
      {"LC_TIME",            _NL_NUM_LC_TIME},
      {"LC_CTYPE",           _NL_NUM_LC_CTYPE},
      {"",}, 
      {"t_fmt",              T_FMT},
      {"LC_MESSAGES",        _NL_NUM_LC_MESSAGES},
      {"",}, {"",}, 
      {"UNDEFINED",          TOK_UNDEFINED},
      {"LC_NUMERIC",         _NL_NUM_LC_NUMERIC},
      {"",}, {"",}, 
      {"position",           TOK_POSITION},
      {"",}, 
      {"print",              TOK_PRINT},
      {"",}, 
      {"toupper",            TOK_TOUPPER},
      {"positive_sign",      POSITIVE_SIGN},
      {"p_sep_by_space",     P_SEP_BY_SPACE},
      {"LC_COLLATE",         _NL_NUM_LC_COLLATE},
      {"LC_MONETARY",        _NL_NUM_LC_MONETARY},
      {"",}, 
      {"mon",                MON_1},
      {"",}, 
      {"t_fmt_ampm",         T_FMT_AMPM},
      {"noexpr",             NOEXPR},
      {"mon_thousands_sep",  MON_THOUSANDS_SEP},
      {"",}, 
      {"n_sep_by_space",     N_SEP_BY_SPACE},
      {"digit",              TOK_DIGIT},
      {"p_sign_posn",        P_SIGN_POSN},
      {"tolower",            TOK_TOLOWER},
      {"negative_sign",      NEGATIVE_SIGN},
      {"",}, 
      {"d_fmt",              D_FMT},
      {"order_start",        TOK_ORDER_START},
      {"",}, 
      {"grouping",           GROUPING},
      {"",}, 
      {"nostr",              NOSTR},
      {"n_sign_posn",        N_SIGN_POSN},
      {"mon_grouping",       MON_GROUPING},
      {"",}, {"",}, 
      {"lower",              TOK_LOWER},
      {"xdigit",             TOK_XDIGIT},
      {"",}, 
      {"era",                ERA},
      {"",}, 
      {"abmon",              ABMON_1},
      {"yesstr",             YESSTR},
      {"",}, {"",}, 
      {"era_d_fmt",          ERA_D_FMT},
      {"space",              TOK_SPACE},
      {"",}, 
      {"mon_decimal_point",  MON_DECIMAL_POINT},
      {"decimal_point",      DECIMAL_POINT},
      {"",}, {"",}, {"",}, {"",}, 
      {"p_cs_precedes",      P_CS_PRECEDES},
      {"order_end",          TOK_ORDER_END},
      {"punct",              TOK_PUNCT},
      {"",}, 
      {"yesexpr",            YESEXPR},
      {"era_year",           ERA_YEAR},
      {"",}, {"",}, 
      {"escape_char",        TOK_ESCAPE_CHAR},
      {"comment_char",       TOK_COMMENT_CHAR},
      {"n_cs_precedes",      N_CS_PRECEDES},
      {"copy",               TOK_COPY},
      {"am_pm",              AM_STR},
      {"",}, {"",}, 
      {"backward",           TOK_BACKWARD},
      {"",}, 
      {"int_curr_symbol",    INT_CURR_SYMBOL},
      {"",}, {"",}, {"",}, 
      {"from",               TOK_FROM},
      {"cntrl",              TOK_CNTRL},
      {"",}, 
      {"collating_element",  TOK_COLLATING_ELEMENT},
      {"day",                DAY_1},
      {"",}, 
      {"abday",              ABDAY_1},
      {"",}, 
      {"d_t_fmt",            D_T_FMT},
      {"",}, {"",}, 
      {"upper",              TOK_UPPER},
      {"",}, {"",}, 
      {"thousands_sep",      THOUSANDS_SEP},
      {"",}, {"",}, 
      {"collating_symbol",   TOK_COLLATING_SYMBOL},
      {"",}, {"",}, {"",}, 
      {"alpha",              TOK_ALPHA},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"graph",              TOK_GRAPH},
      {"",}, 
      {"forward",            TOK_FORWARD},
      {"",}, {"",}, 
      {"int_frac_digits",    INT_FRAC_DIGITS},
      {"",}, {"",}, {"",}, {"",}, 
      {"alt_digits",         ALT_DIGITS},
      {"frac_digits",        FRAC_DIGITS},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"blank",              TOK_BLANK},
      {"",}, {"",}, {"",}, {"",}, 
      {"currency_symbol",    CURRENCY_SYMBOL},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*s == *str && !strncmp (str + 1, s + 1, len - 1))
            return &wordlist[key];
        }
    }
  return 0;
}
