#include <string.h>
#define internal_to_getallopt
#include "getallopt.h"

/*
 *  This evaluates argv and puts all options into the Options string.
 *  The array argv is reordered to only contain values for input arguments.
 */

int getallopt(
  int *argc, char *argv [], char *Options,
  int sizeofOptions, struct OptionType Option[]){

	int i,NewArgc,error=0,m=0;
	char c[3]="--";
	char *Argument;

	for (i=1,NewArgc=0;i < *argc;i++) if (argv[i] != 0)
	  if (*(Argument=argv[i]) == '-'){int j=0;
		while (Argument[j] != '\0'){
			if (m >= sizeofOptions){
				error--; /* Options string is too short */
				i=*argc;
				break;
			};
			if (!strncmp(&Argument[j],"--",2)){int k=0,l;
				for (j += 2;Option[k].Long[0] != '\0';k++)
				  if (!strncmp(&Argument[j],Option[k].Long,
				  l=strlen(Option[k].Long))){int n=0;
					for (j += l;n < Option[k].Fields;n++)
					  IfIncreasePArg(i) argv[++NewArgc]=argv[i];
					if (Option[k].Short != IGNORED)
					  Options[m++]=Option[k].Short;
					break;
				};
				if (Option[k].Long[0] == '\0'){
					error--; /* This option is unknown */
					break;
				};
			}else{int k=0;
				for (;Option[k].Long[0] != '\0';k++)
				  if (Argument[j] == Option[k].Short){int n=0;
					for (;n < Option[k].Fields;n++)
					  IfIncreasePArg(i) argv[++NewArgc]=argv[i];
					if (Option[k].Short != IGNORED){
						Options[m++]=Argument[j++];
					}else j++;
					break;
				};
				if (Option[k].Long[0] == '\0'){
					c[1]=Argument[j];
					error--; /* This option is unknown */
					break;
				};
			};
		};
	}else IfIncreasePArg(NewArgc) argv[NewArgc]=argv[i];

	Options[m++]='\0';
	*argc=++NewArgc;
	return error;
};

