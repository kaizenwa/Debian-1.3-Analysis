/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -a -C -E -g -H is_reserved_hash -k 1,3,$ -N is_reserved_word -p -t keywords.gperf  */
struct resword { const char *name; int token; };
/* maximum key range = 63, duplicates = 0 */

#ifdef __GNUC__
inline
#endif
static unsigned int
is_reserved_hash (register const char *str, register int len)
{
  static const unsigned char asso_values[] =
    {
     66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
     66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
     66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
     66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
     66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
     66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
     66, 66, 66, 66, 66, 66, 66, 66, 66,  5,
     66, 66, 66, 66, 66, 66,  0, 66, 66, 66,
      0, 66, 66, 66, 66, 66, 66, 66, 66, 66,
     66, 66, 66, 66, 66,  0, 66,  0, 66, 30,
     25,  0, 10, 15, 66, 20, 66, 66, 25,  0,
     15, 20,  5, 66, 10,  0,  0, 25,  5, 66,
     66, 66, 66, 66, 66, 66, 66, 66,
    };
  return len + asso_values[str[2]] + asso_values[str[0]] + asso_values[str[len - 1]];
}

#ifdef __GNUC__
inline
#endif
const struct resword *
is_reserved_word (register const char *str, register int len)
{
  enum
    {
      TOTAL_KEYWORDS = 35,
      MIN_WORD_LENGTH = 3,
      MAX_WORD_LENGTH = 13,
      MIN_HASH_VALUE = 3,
      MAX_HASH_VALUE = 65,
    };

  static const struct resword wordlist[] =
    {
      {"",}, {"",}, {"",}, 
      {"asm",  ASM_KEYW},
      {"",}, 
      {"__asm",  ASM_KEYW},
      {"",}, 
      {"__asm__",  ASM_KEYW},
      {"",}, 
      {"attribute",  ATTRIBUTE_KEYW},
      {"__signed__",  SIGNED_KEYW},
      {"__attribute",  ATTRIBUTE_KEYW},
      {"",}, 
      {"__attribute__",  ATTRIBUTE_KEYW},
      {"",}, 
      {"__volatile",  VOLATILE_KEYW},
      {"struct",  STRUCT_KEYW},
      {"__volatile__",  VOLATILE_KEYW},
      {"EXPORT_SYMBOL",  EXPORT_SYMBOL_KEYW},
      {"",}, {"",}, 
      {"extern",  EXTERN_KEYW},
      {"typedef",  TYPEDEF_KEYW},
      {"int",  INT_KEYW},
      {"auto",  AUTO_KEYW},
      {"short",  SHORT_KEYW},
      {"",}, {"",}, 
      {"__inline",  INLINE_KEYW},
      {"enum",  ENUM_KEYW},
      {"__inline__",  INLINE_KEYW},
      {"",}, {"",}, 
      {"__signed",  SIGNED_KEYW},
      {"",}, 
      {"float",  FLOAT_KEYW},
      {"static",  STATIC_KEYW},
      {"__const",  CONST_KEYW},
      {"volatile",  VOLATILE_KEYW},
      {"__const__",  CONST_KEYW},
      {"",}, {"",}, {"",}, 
      {"register",  REGISTER_KEYW},
      {"char",  CHAR_KEYW},
      {"",}, 
      {"signed",  SIGNED_KEYW},
      {"",}, {"",}, {"",}, 
      {"const",  CONST_KEYW},
      {"inline",  INLINE_KEYW},
      {"",}, {"",}, 
      {"void",  VOID_KEYW},
      {"",}, 
      {"double",  DOUBLE_KEYW},
      {"",}, 
      {"unsigned",  UNSIGNED_KEYW},
      {"long",  LONG_KEYW},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"union",  UNION_KEYW},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = is_reserved_hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
