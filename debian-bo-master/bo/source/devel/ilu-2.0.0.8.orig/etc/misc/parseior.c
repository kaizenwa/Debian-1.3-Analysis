#include <stdio.h>
#include <string.h>
#include <iluxport.h>

extern ilu_boolean _ilu_IIOP_ParseIOR (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *,
				       ilu_cardinal *, ilu_Error *);

static ilu_string encode (ilu_string key)
{
  ilu_Error err;
  int i;
  ilu_string copy;
  char *p;

  copy = ilu_StrdupE(key, &err);
  ILU_HANDLED(err);
  for (p = copy;  *p != 0;  p++)
    if ((*p < ((char) 0x20)) || (*p > ((char) 0x7E)))
      *p = '.';
  return copy;
}

int main (int ac, char **av)
{
  ilu_string ih = ILU_NIL, sid = ILU_NIL, mstid = ILU_NIL, cinfo = ILU_NIL;
  ilu_cardinal cinfolen = 0;
  ilu_Error err;

  if (ac < 2 || ((strncmp(av[1], "IOR:", 4) != 0) && (strncmp(av[1], "IOR:", 4) != 0)))
    {
      fprintf (stderr, "Usage:  %s STRINGIFIED-IOR\n", av[0]);
      return 1;
    }
  else
    {
      ilu_SetDebugLevelViaString("iiop");
      if (!_ilu_IIOP_ParseIOR (av[1], &ih, &sid, &mstid, &cinfo, &cinfolen, &err))
	{
	  printf ("Error parsing IOR:  %s\n", ILU_ERR_NAME(err));
	  ILU_HANDLED(err);
	  return 1;
	}
      else
	{
	  if (strcmp(ih, "$") == 0)
	    {
	      ilu_string objkey = encode(sid);
	      printf ("object <%s> (type <%s>)\n    at %*.*s\n", objkey,
		      (mstid == ILU_NIL) ? "--unknown--" : mstid,
		      cinfolen, cinfolen, cinfo);
	    }
	  else
	    printf ("object <%s/%s> (type <%s>)\n    at %*.*s (an ILU orb)\n", sid, ih,
		    (mstid == ILU_NIL) ? "--unknown--" : mstid,
		    cinfolen, cinfolen, cinfo);
	  return 0;
	}
    }
}
