/* Main program of GNU Rez, a Mac resource compiler.
   Copyright 1994, 1995 Free Software Foundation.

This file is part of GNU Rez.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "grez.h"

enum mac_file_format {
  native,
  applesingle,
  appledouble,
  macbinary,
  binhex
};

#ifndef c2p
#define c2p(STR,PBUF) \
  strcpy(((char *) PBUF) + 1, STR);  \
  PBUF[0] = strlen(STR);
#endif

#ifndef p2c
#define p2c(PSTR,BUF)  \
  strncpy(BUF, ((char *) (PSTR) + 1), PSTR[0]);  \
  BUF[PSTR[0]] = '\0';
#endif

int num_input_files = 0;

char *input_file_names[100];

char *output_file_name = "Grez.out";

OSType output_file_type = 'APPL';

OSType output_file_creator = '????';

enum mac_file_format output_file_format = applesingle;

int merge_resources = 0;

int show_progress = 0;

int show_version = 0;

char *current_file_name;

FILE *current_input_file;

FILE *output_data_fork;

FILE *output_resource_fork;

#ifdef MPW /* native */

short resource_file_refnum = -1;

#endif

int debug;

int marker = 0xdeadbeef;

struct resource_map *original_map = NULL;

struct resource_map *current_map = NULL;

void open_resource_file PARAMS ((void));
void save_all_resources PARAMS ((void));
void close_resource_file PARAMS ((void));

void write_mac_file_header PARAMS ((FILE *));

void write_resource_fork PARAMS ((FILE *));

/* Build up a 4-char code from a string.  Note that the order is always
   bigendian.  */

int
get_four_char_code (str, rslt)
     char *str;
     long *rslt;
{
  int i;

  *rslt = 0;
  for (i = 0; i < 4; ++i)
    {
      if (str[i] == '\0')
        break;
      *rslt = *rslt * 256 + str[i];
    }
  /* (should return error code if string longer than 4 chars) */
  return 0;
}

main (argc, argv)
     int argc;
     char **argv;
{
  int i, err, saw_filename = 0;
  char buf[256];

#ifdef MPW
  /* If we're actually being hosted on a Mac, prefer to use actual
     resources.  */
  output_file_format = native;

  InitCursorCtl (nil);
#endif

  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  char *str = argv[i] + 1;

	  if (strcmp (str, "a") == 0 || strcmp (str, "append") == 0)
	    merge_resources = 1;

	  else if (strcmp (str, "align") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "2") == 0 || strcmp (str, "appledouble") == 0)
	    output_file_format = appledouble;

	  else if (strcmp (str, "1") == 0 || strcmp (str, "applesingle") == 0)
	    output_file_format = applesingle;

	  else if (strcmp (str, "c") == 0 || strcmp (str, "creator") == 0)
	    err = get_four_char_code (argv[++i], &output_file_creator);

	  else if (strcmp (str, "debug") == 0)
	    debug = 1;

	  else if (strcmp (str, "d") == 0 || strcmp (str, "define") == 0)
#ifdef CPPLIB
	    warning ("option not implemented yet, ignoring");
#else
	    warning ("option not implemented yet, ignoring");
#endif /* CPPLIB */

	  else if (strcmp (str, "h") == 0 || strcmp (str, "-help") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "i") == 0 || strcmp (str, "include") == 0)
#ifdef CPPLIB
	    warning ("option not implemented yet, ignoring");
#else
	    warning ("option not implemented yet, ignoring");
#endif /* CPPLIB */

	  else if (strcmp (str, "m") == 0 || strcmp (str, "modification") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "macbinary") == 0)
	    output_file_format = macbinary;

	  else if (strcmp (str, "noResolve") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "o") == 0)
	    output_file_name = argv[i++];

	  else if (strcmp (str, "ov") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "p") == 0)
	    show_progress = 1;

	  else if (strcmp (str, "rd") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "ro") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "script") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "s") == 0 || strcmp (str, "search") == 0)
	    warning ("option not implemented yet, ignoring");

	  else if (strcmp (str, "t") == 0 || strcmp (str, "type") == 0)
	    err = get_four_char_code (argv[++i], &output_file_type);

	  else if (strcmp (str, "u") == 0 || strcmp (str, "undef") == 0)
#ifdef CPPLIB
	    warning ("option not implemented yet, ignoring");
#else
	    warning ("option not implemented yet, ignoring");
#endif /* CPPLIB */

	  else if (strcmp (str, "v") == 0 || strcmp (str, "-version") == 0)
	    show_version = 1;

#ifdef YYDEBUG
	  else if (strcmp (str, "yydebug") == 0)
	    {
	      extern int yydebug;

	      yydebug = 1;
	    }
#endif

	  else
	    fprintf (stderr, "Unknown option \"%s\"\n", str - 1);
	}
      else
	/* Interpret as a file name to be processed.  */
	input_file_names[num_input_files++] = argv[i];
    }

  if (show_version)
    {
      printf ("GNU Rez version %s\n", version_string);
      exit (0);
    }

  /* Create/open the output file.  */

  current_map = create_new_resource_map ();

  switch (output_file_format)
    {
      case native:
	open_resource_file ();
	break;

      case appledouble:
	output_data_fork = fopen (output_file_name, "w");
	sprintf (buf, "%%%s", output_file_name);
	output_resource_fork = fopen (buf, "w");
	break;

      case applesingle:
      case macbinary:
	output_data_fork = fopen (output_file_name, "w");
	break;

      default:
	error ("unknown output format");
	break;
    }

  /* Collect and merge resources from input files.  */

  if (num_input_files > 0)
    {
      for (i = 0; i < num_input_files; ++i)
	{
	  current_file_name = input_file_names[i];
#ifdef BFD
	  if (0 /* is recognized as a bfd file */)
	    {
	      /* (should xform into code resources, create CODE 0 if necessary) */
	      convert_elf_to_code ();
	    }
	  else
#endif
	    {
	      char ch1;

	      current_input_file = fopen (current_file_name, "r");
	      if (current_input_file == NULL)
		{
		  warning ("could not open file");
		  continue;
		}
		ch1 = fgetc (current_input_file);
		ungetc (ch1, current_input_file);
		if (ch1 == 0x7f)
		  {
			/* Almost certainly an ELF file. */
			convert_elf_to_code ();
		  }
		init_lex ();
		yyparse ();
		fclose (current_input_file);
	    }
	}
    }
  else
    {
      /* Collect and merge resources from standard input.  */
      current_file_name = NULL;
      current_input_file = stdin;
      /* Assume that stdin can only be Rez language.  */
      init_lex ();
      yyparse ();
    }

  /* Write out any remaining unwritten resources.  */
  
  if (output_file_format == native)
    {
      save_all_resources ();
    }
  else if (output_resource_fork)
    {
      write_mac_file_header (output_resource_fork);
      write_resource_fork (output_resource_fork);
    }
  else if (output_data_fork)
    {
      write_mac_file_header (output_data_fork);
      write_resource_fork (output_data_fork);
    }

  /* Close the output file(s). */

  if (output_file_format == native)
    close_resource_file ();

  if (output_resource_fork)
    fclose (output_resource_fork);

  if (output_data_fork)
    fclose (output_data_fork);
}

void
open_resource_file ()
{
#ifdef MPW
  short vref;
  long dirid;
  OSErr err;
  Str255 filename;
  short initialvrefnum;
  int i, j, numtypes, numresources;
  ResType restype;
  Handle reshandle;

  /* Capture the current vrefnum. */
  GetVol (NULL, &initialvrefnum);
  c2p (output_file_name, filename);
  vref = 0;
  dirid = 0;
  err = HCreate (vref, dirid, filename, output_file_creator, output_file_type);
  if (err == dupFNErr)
    {
      /* (should change file type and creator) */
      err = noErr;
    }
  if (err == noErr)
    {
      /* Ensure that the resource fork exists. */
      HCreateResFile (vref, dirid, filename);
      resource_file_refnum = HOpenResFile(vref, dirid, filename, fsRdWrPerm);
      if (resource_file_refnum == -1)
	err = -1;
      if (ResError ())
	err = ResError ();
    }
  if (err < 0)
    {
      warning ("file error?");
    }
  original_map = create_new_resource_map ();

  numtypes = Count1Types();
  for (i = 1; i <= numtypes; ++i)
    {
      Get1IndType (&restype, i);
      numresources = Count1Resources (restype);
      for (j = 1; j <= numresources; ++j)
	{
	  reshandle = Get1IndResource (restype, j);
	  if (reshandle != nil)
	    {
	      short resid;
	      ResType thetype;
	      Str255 name;
	      struct resource *rsrc;

	      GetResInfo (reshandle, &resid, &thetype, &name);
	      rsrc = create_resource (original_map, restype, resid);
	      rsrc->handle = reshandle;
	    }
	}
    }
#else /* not MPW */
  error ("not running on a Mac");
#endif /* MPW */
}

void
save_all_resources ()
{
#ifdef MPW
  struct resource *rsrc;
  OSType rtype;
  Str255 rname;
  Handle rdata;

  if (resource_file_refnum != -1)
    {
      for (rsrc = original_map->resources; rsrc != NULL; rsrc = rsrc->next)
        {
	  if (rsrc->handle != nil)
	    {
	      if (!merge_resources)
	        {
		  RmveResource (rsrc->handle);
		  DisposeHandle (rsrc->handle);
		  rsrc->handle = nil;
		}
	    }
	}
      for (rsrc = current_map->resources; rsrc != NULL; rsrc = rsrc->next)
        {
	  rtype = rsrc->type;
	  c2p ("", rname);
	  if (rsrc->name)
	    c2p (rsrc->name, rname);
	  rdata = NewHandle (rsrc->size);
	  memcpy (*rdata, rsrc->data, rsrc->size);
	  AddResource (rdata, rtype, rsrc->id, rsrc->name);
	}
    }
#else
  error ("not running on a Mac");
#endif /* MPW */
}

void
close_resource_file ()
{
#ifdef MPW
  if (resource_file_refnum != -1)
    CloseResFile (resource_file_refnum);
#else
  error ("not running on a Mac");
#endif /* MPW */
}

/* Write an AppleSingle/Double format header to the given file.  */

void
write_mac_file_header (fp)
     FILE *fp;
{
  int header = 0x71600000;

  fwrite (((char *) &header), 1, 4, fp);
  fwrite (((char *) &output_file_type), 1, 4, fp);
  fwrite (((char *) &output_file_creator), 1, 4, fp);
  /* etc */
}

/* Write everything in the resource fork to the given file.  */

void
write_resource_fork (fp)
     FILE *fp;
{
  int i, zero = 0, minus1 = -1, count, tmp, typecount, len;
  struct resource_header resource_fork_header;
  struct resource *rsrc;
  struct resource_map *map;
  struct resource_type_list *typelist;

  resource_fork_header.map = current_map;  /* kludge */

  fseek (fp, 0, SEEK_SET);
  for (i = 0; i < 64; ++i)
    fwrite (((char *) &zero), 1, 4, fp);

  resource_fork_header.data_offset = ftell (fp);
  resource_fork_header.data_size = 0;

  for (rsrc = current_map->resources; rsrc != NULL; rsrc = rsrc->next)
    {
      rsrc->data_offset = ftell (fp) - resource_fork_header.data_offset;
      fwrite (((char *) &(rsrc->size)), 1, 4, fp);
      if (rsrc->data != NULL && rsrc->size > 0)
	fwrite (rsrc->data, 1, rsrc->size, fp);
      resource_fork_header.data_size += 4 + rsrc->size;
    }

  resource_fork_header.map_offset = ftell (fp);
  resource_fork_header.map_size = 0;

  for (map = resource_fork_header.map; map != NULL; map = map->next)
    {
      long type_offset_offset, name_offset_offset, typecount_offset;
      long map_offset;
      short type_offset, name_offset;

      /* Fill in the resource map with mostly zeros, but record various
	 offsets so we can fill them in later.  */
      map_offset = ftell (fp);      
      fwrite (((char *) &zero), 1, 4, fp);
      fwrite (((char *) &zero), 1, 4, fp);
      fwrite (((char *) &zero), 1, 4, fp);
      fwrite (((char *) &zero), 1, 4, fp);
      fwrite (((char *) &zero), 1, 4, fp);
      fwrite (((char *) &zero), 1, 2, fp);
      fwrite (((char *) &(map->fork_attrs)), 1, 2, fp);
      type_offset_offset = ftell (fp);
      fwrite (((char *) &marker), 1, 2, fp);
      name_offset_offset = ftell (fp);
      fwrite (((char *) &marker), 1, 2, fp);
      typecount_offset = ftell (fp);
      type_offset = typecount_offset - map_offset;
      fwrite (((char *) &marker), 1, 2, fp);

      typecount = 0;

      for (typelist = map->types; typelist != NULL; typelist = typelist->next)
	{
	  typelist->offset = ftell (fp);
	  fwrite (((char *) &(typelist->type)), 1, 4, fp);
	  fwrite (((char *) &marker), 1, 2, fp);
	  fwrite (((char *) &marker), 1, 2, fp);

	  typelist->resource_count = 0;

	  for (rsrc = typelist->resources; rsrc != NULL; rsrc = rsrc->next)
	    {
	      ++(typelist->resource_count);
	    }
	  
	  ++typecount;
	}

      for (typelist = map->types; typelist != NULL; typelist = typelist->next)
	{
	  typelist->reflist_offset = ftell (fp);

	  for (rsrc = typelist->resources; rsrc != NULL; rsrc = rsrc->next)
	    {
	      fwrite (((char *) &(rsrc->id)), 1, 2, fp);
	      fwrite (((char *) &minus1), 1, 2, fp);
	      fwrite (((char *) &(rsrc->attrs)), 1, 1, fp);
	      fwrite (((char *) &(rsrc->data_offset)) + 1, 1, 3, fp);
	      fwrite (((char *) &zero), 1, 4, fp);
	    }
	}

      /* Write all the resource names.  */

      name_offset = ftell (fp) - map_offset;

      for (typelist = map->types; typelist != NULL; typelist = typelist->next)
	{
	  for (rsrc = typelist->resources; rsrc != NULL; rsrc = rsrc->next)
	    {
	      if (rsrc->name)
		{
		  len = strlen (rsrc->name);

		  rsrc->name_offset = ftell (fp);
		  fwrite (((char *) &len), 1, 1, fp);
		  fwrite (rsrc->name, 1, len, fp);
		}
	    }
	}

      /* At this point the resource map is at its final size, and the
         remaining work is to go back and write various sizes and offsets.  */

      resource_fork_header.map_size = ftell (fp) - resource_fork_header.map_offset;

      for (typelist = map->types; typelist != NULL; typelist = typelist->next)
	{
	  fseek (fp, typelist->offset + 4, SEEK_SET);
	  count = typelist->resource_count - 1;
	  fwrite (((char *) &count), 1, 2, fp);
	  tmp = typelist->reflist_offset - typelist->offset;
	  fwrite (((char *) &tmp), 1, 2, fp);

	  for (rsrc = typelist->resources; rsrc != NULL; rsrc = rsrc->next)
	    {
	      if (rsrc->name)
	        {
		  fseek (fp, rsrc->ref_offset + 2, SEEK_SET);
		  tmp = rsrc->name_offset - name_offset;
		  fwrite (((char *) &tmp), 1, 2, fp);
		}
	    }
	}

      fseek (fp, type_offset_offset, SEEK_SET);
      fwrite (((char *) &type_offset), 1, 2, fp);
      fseek (fp, name_offset_offset, SEEK_SET);
      fwrite (((char *) &name_offset), 1, 2, fp);
      fseek (fp, typecount_offset, SEEK_SET);
      --typecount;
      fwrite (((char *) &typecount), 1, 2, fp);
      
    }

  /* Go back to the beginning and write final counts, etc. */
  fseek (fp, 0, SEEK_SET);
  fwrite (((char *) &(resource_fork_header.data_offset)), 1, 4, fp);
  fwrite (((char *) &(resource_fork_header.map_offset)), 1, 4, fp);
  fwrite (((char *) &(resource_fork_header.data_size)), 1, 4, fp);
  fwrite (((char *) &(resource_fork_header.map_size)), 1, 4, fp);
}

convert_coff_to_code ()
{
  /* (should copy sections into code resources) */
  /* (should create a CODE 0 if doesn't exist) */
  /* (should add entry points to CODE 0) */
}

convert_elf_to_code ()
{
  /* (should copy sections into code resources) */
  /* (should create a CODE 0 if doesn't exist) */
  /* (should add entry points to CODE 0) */
}

error (s)
     char *s;
{
  extern int lineno;

#ifdef MPW
  if (current_file_name)
    fprintf (stderr, "File \"%s\"; ", current_file_name);
  fprintf (stderr, "Line %d # ", lineno);
#else
  if (current_file_name)
    fprintf (stderr, "%s:%d: ", current_file_name, lineno);
  else
    fprintf (stderr, ":%d: ", lineno);
#endif /* MPW */
  fprintf (stderr, "Error: %s\n", s);
}

warning (s)
     char *s;
{
  extern int lineno;

#ifdef MPW
  if (current_file_name)
    fprintf (stderr, "File \"%s\"; ", current_file_name);
  fprintf (stderr, "Line %d # ", lineno);
#else
  if (current_file_name)
    fprintf (stderr, "%s:%d: ", current_file_name, lineno);
  else
    fprintf (stderr, ":%d: ", lineno);
#endif /* MPW */
  fprintf (stderr, "Warning: %s\n", s);
}

/* Resource handling. */

struct resource *current_resource;

struct rtype_node *current_type;

struct rtype_node *field_stack[100];

int field_stack_top;

struct resource_map *
create_new_resource_map ()
{
  struct resource_map *map;

  map = (struct resource_map *) xmalloc (sizeof (struct resource_map));
  memset (map, 0, sizeof (struct resource_map));
  return map;
}

struct resource_type_list *
find_type_list (map, type)
     struct resource_map *map;
     long type;
{
  struct resource_type_list *typelist;

  if (map == NULL)
    {
      warning ("no map?");
      return NULL;
    }

  for (typelist = map->types; typelist != NULL; typelist = typelist->next)
    {
      if (type == typelist->type)
	return typelist;
    }

  return NULL;
}

struct resource_type_list *
get_type_list (map, type)
     struct resource_map *map;
     long type;
{
  struct resource_type_list *typelist;

  if (map == NULL)
    {
      warning ("no map?");
      return NULL;
    }

  typelist = find_type_list (map, type);
  if (typelist != NULL)
    return typelist;

  typelist = (struct resource_type_list *) xmalloc (sizeof (struct resource_type_list));
  memset (typelist, 0, sizeof (struct resource_type_list));
  typelist->type = type;
  typelist->next = map->types;
  map->types = typelist;

  return typelist;
}

int
create_new_resource (stmt, rspec, usetype)
     char *stmt;
     struct rspec *rspec;
     int usetype;
{
  struct resource *rsrc;
  union {
    char chars[4];
    long type;
  } converter;
  int id = 0;

  if (debug)
    drs (rspec);

  if (rspec->id_given)
    id = rspec->id;
  else
    warning ("no id!");

  if (0 /* find_resource (rspec) */)
    error ("duplicate resource");

  if (usetype)
    {
      current_type = find_type_definition (rspec);
      if (current_type == NULL)
        error ("no type definition found");
    }

  memcpy (converter.chars, rspec->type, 4);

  rsrc = create_resource (current_map, rspec->Type, id);

  if (show_progress)
    {
      printf ("### ");
      if (stmt)
        printf ("%s ", stmt);
      printf ("'%c%c%c%c' (%d)",
	      converter.chars[0], converter.chars[1],
	      converter.chars[2], converter.chars[3],
	      rsrc->id);
      printf ("\n");
    }

  current_resource = rsrc;
  return 0;
}

struct resource *
create_resource (map, type, id)
     struct resource_map *map;
     long type;
     short id;
{
  struct resource *rsrc;
  struct resource_type_list *typelist;

  rsrc = (struct resource *) xmalloc (sizeof (struct resource));
  memset (rsrc, 0, sizeof (struct resource));

  rsrc->type = type;  
  rsrc->id = id;  

  rsrc->next = map->resources;
  map->resources = rsrc;

  typelist = get_type_list (map, rsrc->type);

  if (typelist)
    {
      rsrc->tnext = typelist->resources;
      typelist->resources = rsrc;
    }

  return rsrc;
}

int
append_data_to_resource (rsrc, ptr, len)
     struct resource *rsrc;
     char *ptr;
     int len;
{
  if (rsrc->data == NULL)
    {
      rsrc->data = xmalloc (128);
      memset(rsrc->data, 0, 128);
      rsrc->size = 0;
      rsrc->allocated = 128;
    }
  if (rsrc->allocated < rsrc->size + len)
    {
      int incr = len;
      char *newdata;

      if (len < rsrc->allocated / 4)
        incr = rsrc->allocated / 4;

      newdata = xrealloc (rsrc->data, rsrc->allocated + incr);
      if (newdata == NULL)
        {
	  error ("no space");
	  return -1;
	}

      rsrc->data = newdata;
      rsrc->allocated += incr;
    }
  memcpy (rsrc->data + rsrc->size, ptr, len);
  rsrc->size += len;

  return 0;
}

int
append_number_to_resource (rsrc, num, bits)
     struct resource *rsrc;
     int num, bits;
{
  int bytes = 4;
  union {
    char chars[4];
    int num;
  } converter;

  converter.num = num;

  if (bits % 8 == 0)
    bytes = bits / 8;
  else
    warning ("bitstrings not implemented yet");

  return append_data_to_resource (rsrc, converter.chars + (4 - bytes), bytes);
}

int
append_string_to_resource (rsrc, str, subcode, length_expr)
     struct resource *rsrc;
     char *str;
     int subcode;
     struct expr *length_expr;
{
  char buf[1000];

  switch (subcode)
    {
    case 0:
      return append_data_to_resource (rsrc, str, strlen (str));
    case 1:
      c2p (str, buf);
      return append_data_to_resource (rsrc, buf, strlen (str) + 1);
    case 2:
      /* (should fix this) */
      c2p (str, buf);
      return append_data_to_resource (rsrc, buf, strlen (str) + 2);
    case 3:
      return append_data_to_resource (rsrc, str, strlen (str) + 1);
    }
}

#define tohex(c) ((c) <= '9' ? (c) - '0' : \
		  ((c) <= 'F' ? (c) - 'A' + 10 : (c) - 'a' + 10))

int
append_hex_string_to_resource (rsrc, str, subcode, length_expr)
     struct resource *rsrc;
     char *str;
     int subcode;
     struct expr *length_expr;
{
  int len, datalen, i, digit1 = -1, digit2;
  char *buf;

  len = strlen (str);

  buf = xmalloc (len);
  memset (buf, 0, len);
  datalen = 0;

  for (i = 0; i < len; ++i)
    {
      /* Skip over any embedded spaces or tabs.  */
      if (str[i] == ' ' || str[i] == '\t')
        continue;
      if (digit1 < 0)
        digit1 = tohex(str[i]);
      else
	{
	  digit2 = tohex(str[i]);
	  buf[datalen++] = digit1 * 16 + digit2;
	  digit1 = -1;
	}
    }

  if (digit1 >= 0)
    warning ("hex string has odd length, ignoring extra char");

  append_data_to_resource (rsrc, buf, datalen);

  free (buf);

  return 0;
}

void
close_current_resource ()
{
  if (show_progress)
    {
      printf ("  Size is %d\n", current_resource->size);
    }
  current_type = NULL; /* (but need to pad out possible remaining fields?) */
  current_resource = NULL;
}

int
read_resource_from_file (rspec, filename)
     struct rspec *rspec;
     char *filename;
{
  char c;
  FILE *fp;

  fp = fopen (filename, "r");
  if (fp != NULL)
    {
      create_new_resource ("Read", rspec, 0);
      while ((c = getc (fp)) != EOF)
	append_data_to_resource (current_resource, &c, 1);
      close_current_resource ();
      fclose (fp);
    }
  else
    error ("could not open file for reading");

  return 0;
}
