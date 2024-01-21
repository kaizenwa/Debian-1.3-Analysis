// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <stdio.h>
#include <string.h>

#include "defines.h"
#include "structs.h"

void
read_rc (FILE * rc_fp, char *filename, struct options_info *options)
{
  char *str_ptr;
  int name_len;
  char rc_line[MAX_RC_LEN];

  if ((str_ptr = strrchr (filename, '/')) == NULL)
    str_ptr = filename;
  else
    str_ptr++;

  if ((name_len = strlen (str_ptr)) > 0)
    {
      fseek (rc_fp, 0, SEEK_SET);

      while (fgets (rc_line, MAX_RC_LEN, rc_fp) != NULL)
	{
	  if (!strncmp (str_ptr, rc_line, name_len))
	    {
#ifndef USE_X
	      //fprintf (stderr, "rc: %s\n", rc_line);
#endif

	      if (strstr (rc_line, "nobpm") != NULL)
		options->bpm_tempos = 0;
	      if (strstr (rc_line, "ntsc") != NULL)
		options->ntsc = 1;
	      if (strstr (rc_line, "nospeed0") != NULL)
		options->tolerant = 1;
	      if (strstr (rc_line, "extend") != NULL)
		options->extend_oct = 1;
	      if (strstr (rc_line, "50hz") != NULL)
		options->use_50hz = 1;
	    }
	}
    }
}
