/* -*- mode: C; mode: fold; -*- */
/*  Copyright (c) 1997 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 * 
 * Most of the code in this file came from Bjoern Frantzen, bjoff@bgnett.no
 */

/* The basic assumption of the character mapping code is that the native
 * character codes have a one-to-one mapping to isolatin codes.
 * However, this is unlikely to be true and there will most likely be
 * characters in the native charcter set that have no isolatin
 * equivalents.  One reason for this is that the isolatin character
 * set does not use codes in the 128-160 because this range is
 * reserved for 8 bit escape sequences.  This would not be a problem
 * if each character set defined 2 arrays that defined maps to and
 * from isolatin.  However, the current implementation uses only one
 * array that contains the mapping from the native character set to
 * the isolatin set.  From this single well-defined map, an inverse
 * map is constructed and the problem described above emerges. See
 * the chmap_setup_map function for the solution currently adopted.
 * 
 * Perhaps the best thing would be to define two arrays.
 */

#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <string.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef VMS
# include "vms.h"
#endif

#include <slang.h>
#include "jdmacros.h"

#include "misc.h"
#include "util.h"
#include "chmap.h"
#include "group.h"
#include "art.h"

#if SLRN_HAS_CHARACTER_MAP
char *Slrn_Charset;

static char File_Error [] = "File error:";
static char Not_Posted [] = "--- message not posted.";

# include "ibm850.h"
static unsigned char *ChMap_To_Iso_Map;
static unsigned char *ChMap_From_Iso_Map;

static void chmap_map_string (char *str, unsigned char *map)
{
   unsigned char ch;
   
   if (map == NULL)
     return;
   
   while (0 != (ch = *str))
     {
	if (ch & 0x80)
	  ch = map [ch & 0x7F];
	
	*str++ = ch;
     }
}

static void chmap_map_string_from_iso (char *str)
{
   chmap_map_string (str, ChMap_From_Iso_Map);
}

static void chmap_map_string_to_iso (char *str)
{
   chmap_map_string (str, ChMap_To_Iso_Map);
}

#endif

/* We fix the rest of the header later from hide_art_headers() */
void slrn_chmap_fix_headers (void)
{
#if SLRN_HAS_CHARACTER_MAP
   Slrn_Header_Type *h = Slrn_First_Header;
   
   while (h != NULL) 
     {
	if ((h->flags & HEADER_CHMAP_PROCESSED) == 0)
	  {
	     chmap_map_string_from_iso (h->subject);
	     chmap_map_string_from_iso (h->from);
	     h->flags |= HEADER_CHMAP_PROCESSED;
	  }
        h = h->real_next;
    }
#endif
}

void slrn_chmap_fix_body (void)
{
#if SLRN_HAS_CHARACTER_MAP
    Slrn_Article_Line_Type *ptr = Slrn_Article_Lines;
   
    while (ptr != NULL) 
     {
        chmap_map_string_from_iso (ptr->buf);
        ptr = ptr->next;
    }
#endif
}

#if SLRN_HAS_CHARACTER_MAP

static int chmap_copy_file (char *infile, char *outfile)
{
   FILE *in, *out;
   int ch;
   int ret;
   
   if (NULL == (in = fopen (infile, "r")))
     {
	slrn_error ("%s %s %s", File_Error, infile, Not_Posted);
	return -1;
     }
   
   if (NULL == (out = fopen (outfile, "w")))
     {
	fclose (in);
        slrn_error ("%s %s %s", File_Error, outfile, Not_Posted);
	return -1;
     }

   ret = 0;
   while (EOF != (ch = getc (in)))
     {
	if (EOF == putc (ch, out))
	  {
	     slrn_error ("Write Error. %s", Not_Posted);
	     break;
	  }
     }
   
   fclose (in);
   if (-1 == slrn_fclose (out))
     ret = -1;
   
   return ret;
}
#endif

int slrn_chmap_fix_file (char *file)
{
#if SLRN_HAS_CHARACTER_MAP
   FILE *fp, *tmpfp;
   char buf [4096];
   char tmp_file [SLRN_MAX_PATH_LEN];
   char *name;
   unsigned int len;
   int ret;
   
   name = slrn_basename (file);
   len = (unsigned int) (name - file);
   if (len != 0)
     strncpy (tmp_file, file, len);
   else
     {
	tmp_file [0] = '.';
	len++;
     }
   tmp_file [len] = 0;
   
   if (NULL == (fp = fopen (file, "r")))
     {
        slrn_error ("%s %s %s", File_Error, file, Not_Posted);
        return -1;
     }
   
   if (NULL == (tmpfp = slrn_open_tmpfile_in_dir (tmp_file, tmp_file, "w")))
     {
	slrn_error ("%s %s %s", File_Error, tmp_file, Not_Posted);
	fclose (fp);
	return -1;
     }
   
   ret = 0;
   while (NULL != fgets (buf, sizeof (buf), fp))
     {
	chmap_map_string_to_iso (buf);
	if (EOF == fputs (buf, tmpfp))
	  {
	     slrn_error ("Write Error. Disk Full? %s", Not_Posted);
	     ret = -1;
	     break;
	  }
     }
   
   slrn_fclose (fp);
   if (-1 == slrn_fclose (tmpfp))
     ret = -1;
   
   if (ret == -1)
     {
	(void) slrn_delete_file (tmp_file);
	return -1;
     }
   
   (void) slrn_delete_file (file);
   if (-1 == rename (tmp_file, file))
     {
	ret = chmap_copy_file (tmp_file, file); 
	slrn_delete_file (tmp_file);
     }
   return ret;
#else
   (void) file;
   return 0;
#endif
}


#if SLRN_HAS_CHARACTER_MAP
static unsigned char From_Iso_Map_Buf [128];
static void chmap_setup_map (unsigned char *to_iso_map)
{
   unsigned int i;
   unsigned char ch;
   
   for (i = 0; i < 128; i++) From_Iso_Map_Buf [i] = (unsigned char) (i + 128);
   for (i = 0; i < 128; i++)
     {
	unsigned int j = i + 128;
	
	ch = to_iso_map [i];
	
	/* Try to avoid a bad inverse in case where map is not 1-1 and onto. */
	if (j != (unsigned int)ch)
	  From_Iso_Map_Buf [ch & 0x7F] = (unsigned char) j;
     }
   
   ChMap_To_Iso_Map = to_iso_map;
   ChMap_From_Iso_Map = From_Iso_Map_Buf;
}
#endif

int slrn_set_charset (char *name)
{
#if SLRN_HAS_CHARACTER_MAP
   
   if (name == NULL)
     {
# ifdef __os2__
	name = "ibm850";
# else
	name = "isolatin";
# endif
     }
   
   ChMap_To_Iso_Map = ChMap_From_Iso_Map = NULL;
   SLsmg_Display_Eight_Bit = 160;
   
   if (0 == strcmp (name, "ibm850"))
     {
	SLsmg_Display_Eight_Bit = 128; /* all 8 bit chars are displayable */
	chmap_setup_map (Cp850_To_Iso_Map);
	return 0;
     }
   if (0 == strcmp (name, "isolatin"))
     return 0;
   
   slrn_error ("Supported character sets: ibm850, isolatin");
   return -1;

#else
   (void) name;
   return -1;
#endif
}
