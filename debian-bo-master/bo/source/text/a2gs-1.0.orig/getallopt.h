/*
 * Define the long names for input options.
 */

struct OptionType
{
	char Short, *Long;
	int Fields;
};

#ifdef internal_to_getallopt 

#define IGNORED '-'
#define IfIncreasePArg(i) \
	if (++(i) == *argc){error--;}else if ((i) < *argc)
#else
	extern int getallopt(
	  int *argc, char *argv [], char *Options,
	  int sizeofOptions, struct OptionType Option[]);
#endif

