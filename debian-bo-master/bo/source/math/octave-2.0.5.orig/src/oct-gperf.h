/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -a -C -E -H octave_kw_hash -g -N octave_kw_lookup -p -t octave.gperf  */

enum octave_kw_id
{
  all_va_args_kw,
  break_kw,
  case_kw,
  catch_kw,
  continue_kw,
  else_kw,
  elseif_kw,
  end_kw,
  end_try_catch_kw,
  end_unwind_protect_kw,
  endfor_kw,
  endfunction_kw,
  endif_kw,
  endswitch_kw,
  endwhile_kw,
  for_kw,
  function_kw,
  global_kw,
  gplot_kw,
  gsplot_kw,
  if_kw,
  otherwise_kw,
  replot_kw,
  return_kw,
  switch_kw,
  try_kw,
  unwind_protect_kw,
  unwind_protect_cleanup_kw,
  while_kw
};

struct octave_kw { char *name; int tok; octave_kw_id kw_id; };
/* maximum key range = 48, duplicates = 0 */

#ifdef __GNUC__
inline
#endif
static unsigned int
octave_kw_hash (register const char *str, register int len)
{
  static const unsigned char asso_values[] =
    {
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     50, 50, 50, 50, 50, 50, 50, 20,  5, 15,
      0,  0,  0, 15, 15,  0, 50,  0,  5, 50,
     28,  0,  0, 50, 10,  0, 25, 10, 50, 10,
     50,  5, 50, 50, 50, 50, 50, 50,
    };
  return len + asso_values[str[len - 1]] + asso_values[str[0]];
}

#ifdef __GNUC__
inline
#endif
const struct octave_kw *
octave_kw_lookup (register const char *str, register int len)
{
  enum
    {
      TOTAL_KEYWORDS = 29,
      MIN_WORD_LENGTH = 2,
      MAX_WORD_LENGTH = 22,
      MIN_HASH_VALUE = 2,
      MAX_HASH_VALUE = 49,
    };

  static const struct octave_kw wordlist[] =
    {
      {"",}, {"",}, 
      {"if",  IF, if_kw},
      {"end",  END, end_kw},
      {"else",  ELSE, else_kw},
      {"endif",  END, endif_kw},
      {"elseif",  ELSEIF, elseif_kw},
      {"",}, 
      {"endwhile",  END, endwhile_kw},
      {"otherwise",  OTHERWISE, otherwise_kw},
      {"break",  BREAK, break_kw},
      {"",}, {"",}, 
      {"for",  FOR, for_kw},
      {"",}, 
      {"while",  WHILE, while_kw},
      {"endfor",  END, endfor_kw},
      {"",}, {"",}, 
      {"case",  CASE, case_kw},
      {"",}, 
      {"switch",  SWITCH, switch_kw},
      {"",}, 
      {"continue",  CONTINUE, continue_kw},
      {"endswitch",  END, endswitch_kw},
      {"",}, 
      {"global",  GLOBAL, global_kw},
      {"",}, 
      {"end_try_catch",  END, end_try_catch_kw},
      {"",}, {"",}, 
      {"all_va_args",  ALL_VA_ARGS, all_va_args_kw},
      {"unwind_protect_cleanup",  CLEANUP, unwind_protect_cleanup_kw},
      {"try",  TRY, try_kw},
      {"",}, 
      {"catch",  CATCH, catch_kw},
      {"function",  FCN, function_kw},
      {"",}, {"",}, 
      {"endfunction",  END, endfunction_kw},
      {"",}, 
      {"replot",  PLOT, replot_kw},
      {"",}, 
      {"end_unwind_protect",  END, end_unwind_protect_kw},
      {"return",  FUNC_RET, return_kw},
      {"gplot",  PLOT, gplot_kw},
      {"gsplot",  PLOT, gsplot_kw},
      {"",}, {"",}, 
      {"unwind_protect",  UNWIND, unwind_protect_kw},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = octave_kw_hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
