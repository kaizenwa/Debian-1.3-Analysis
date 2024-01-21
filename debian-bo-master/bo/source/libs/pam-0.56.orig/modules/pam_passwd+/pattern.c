/*
 * interface for System V pattern matchers
 */

#include "passwd.h"
#include <regex.h>

/*
 * free_pattern - release the storage for the compiled pattern
 */

void free_pattern (struct _options *opt)
{
    if (opt->regpattern != (regex_t *) 0)
        regfree (opt->regpattern);
    opt->regpattern = (regex_t *) 0;
}

/*
 * smatch -- set up the pattern to be matched; bomb on error
 */
int smatch (struct _options *opt, char *pat)
{
    if (opt->regpattern)
        regfree (opt->regpattern);

    opt->regpattern = (regex_t *) malloc(sizeof(regex_t));
    if (opt->regpattern == (regex_t *) 0)
      {
	paterr (opt, "no memory");
	return 1;
      }
    memset (opt->regpattern, 0, sizeof (regex_t));

    if (regcomp(opt->regpattern, (char *) pat, 0))
      {
	free (opt->regpattern);
	opt->regpattern = (regex_t *) 0;
	paterr (opt, "Invalid pattern");
	return 1;
      }

    opt->pattern = pat;
    return(0);
}

/*
 * match -- compare a string to the compiled pattern
 */

int match (struct _options *opt, char *str)
{
  regmatch_t rm;
  return (regexec (opt->regpattern, (char *) str, 1, &rm, 0) == 0);
}
