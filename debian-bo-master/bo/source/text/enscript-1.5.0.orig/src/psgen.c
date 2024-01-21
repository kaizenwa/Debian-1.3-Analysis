/* 
 * Convert ASCII to PostScript.
 * Copyright (c) 1995 Markku Rossi.
 *
 * Author: Markku Rossi <mtr@iki.fi>
 */

/*
 * This file is part of GNU enscript.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "gsint.h"

/* 
 * Types and definitions.
 */

/* Values for token flags. */

/* EPSF. */
#define F_EPSF_CENTER			0x01
#define F_EPSF_RIGHT			0x02
#define M_EPSF_JUSTIFICATION		0x03

#define F_EPSF_NO_CPOINT_UPDATE_X	0x04
#define F_EPSF_NO_CPOINT_UPDATE_Y	0x08

#define F_EPSF_ABSOLUTE_X		0x10
#define F_EPSF_ABSOLUTE_Y		0x20

#define F_EPSF_SCALE_X			0x40
#define F_EPSF_SCALE_Y			0x80


/* Predicate to check if we are at the correct slice. */
#define CORRECT_SLICE() (slicing == 0 || current_slice == slice)

/* Predicates for the current body font. */

/* Is character <ch> printable. */
#define ISPRINT(ch) (font_ctype[(unsigned char) (ch)] != ' ')

/* Does character <ch> exist in current body font? */
#define EXISTS(ch) (font_ctype[(unsigned char) (ch)] == '*')


#define RESOURCE_LINE_WIDTH 75

/* Token types. */
typedef enum
{
  tNONE,
  tEOF,
  tSTRING,
  tFORMFEED,
  tNEWLINE,
  tCARRIAGE_RETURN,
  tWRAPPED_NEWLINE,
  tEPSF,
  tSETFILENAME,
  tSETPAGENUMBER,
  tNEWPAGE,
  tFONT,
  tCOLOR,
  tPS
} TokenType;

/* Special escape tokens. */
typedef enum
{
  ESC_COMMENT,
  ESC_EPSF,
  ESC_FONT,
  ESC_COLOR,
  ESC_NEWPAGE,
  ESC_SETFILENAME,
  ESC_SETPAGENUMBER,
  ESC_SHADE,
  ESC_PS
} SpecialEscape;

/* Token structure. */
struct gs_token_st
{
  TokenType type;
  unsigned int flags;
  double new_x;			/* Current point x after this token. */
  double new_y;			/* Current point y after this token. */
  int new_col;			/* Line column after this token. */

  union
    {
      int i;
      char *str;
      struct
	{
	  double x;		/* x-offset */
	  double y;		/* y-offset */
	  double w;		/* width */
	  double h;		/* height */
	  double xscale;
	  double yscale;
	  int llx, lly, urx, ury; /* Bounding box. */
	  char filename[512];
	  char *skipbuf;
	  unsigned int skipbuf_len;
	  unsigned int skipbuf_pos;
	  FILE *fp;		/* File from which eps image is read. */
	  int pipe;		/* Is <fp> opened to pipe?  */
	} epsf;
      Color color;
      struct
	{
	  char name[512];
	  FontPoint size;
	} font;
      char filename[512];
    } u;
};

typedef struct gs_token_st Token;


/*
 * Prototypes for static functions.
 */

static void get_next_token ___P ((InputStream *is, double linepos,
				  unsigned int col, double linew,
				  Token *token));

static void dump_ps_page_header ___P ((char *fname));

static void dump_ps_page_trailer ();

static void dump_empty_page ();

/*
 * Recognize a EPS file described by <token>.  Returns 1 if file was a
 * valid EPS file or 0 otherwise.  File is accepted if it starts with
 * the PostScript magic `%!' and it has a valid `%%BoundingBox' DSC
 * comment.
 */
static int recognize_eps_file ___P ((Token *token));

/*
 * Insert EPS file described by <token> to the output stream.
 */
static void paste_epsf ___P ((Token *token));

/*
 * Check if InputStream <is> contains a file which can be passed
 * through without any modifications.  Returns 1 if file was passed or
 * 0 otherwise.
 */
static int do_pass_through ___P ((char *fname, InputStream *is));

/*
 * Read one float dimension from InputStream <is>.  If <units> is
 * true, number can be followed by an optional unit specifier.  If
 * <horizontal> is true, dimension is horizontal, otherwise it is
 * vertical (this is used to find out how big `line' units are).
 */
static double read_float ___P ((InputStream *is, int units, int horizontal));

/*
 * Print linenumber <linenum> to the beginning of the current line.
 * Current line start is specified by point (x, y).
 */
static void print_line_number ___P ((double x, double y, double space,
				     double margin, unsigned int linenum));

/* Send PostScript to the output file. */
static void output ___P ((char *fmt, ...));

/* Divert output to tmp file so the total page count can be counted. */
static void divert ();

/* Paste diverted data to the output and patch the total page counts. */
static void undivert ();


/*
 * Global variables.
 */

unsigned int current_pagenum;	/* The number of the current page. */
unsigned int total_pages_in_file;
unsigned int input_filenum = 0;
unsigned int current_file_linenum;
char fname[1024];		/* The name of the current input file. */


/*
 * Static variables
 */

/* Have we dumped PS header? */
static int ps_header_dumped = 0;

/* Divert file. */
static FILE *divertfp = NULL;

/* Current output() file. */
static FILE *cofp = NULL;

/* To print or not to print, that's a question. */
static int do_print = 1;

/* Is ^@font{}-defined font active? */
static int user_fontp = 0;

/* The user ^@font{}-defined font. */
static char user_font_name[256];
static FontPoint user_font_pt;

/* Is ^@color{}-defined color active? */
static int user_colorp = 0;

/* The user ^@color{}-defined color. */
static Color user_color;

/* The last linenumber printed by print_line_number(). */
static unsigned int print_line_number_last;

/*
 * Global functions.
 */

void
dump_ps_header ()
{
  char *cp, *cp2;
  int i, j, got;

  /* Dump PS header only once. */
  if (ps_header_dumped)
    return;
  ps_header_dumped = 1;

  /* 
   * Header.
   */

  output ("%s\n", output_first_line);
  output ("%%%%BoundingBox: %d %d %d %d\n", media->llx, media->lly,
	  media->urx, media->ury);
  output ("%%%%Title: %s\n", title);
  output ("%%%%For: %s\n", passwd->pw_gecos);
  output ("%%%%Creator: %s\n", version_string);
  output ("%%%%CreationDate: %s\n", date_string);
  output ("%%%%Orientation: %s\n",
	  ((nup > 1) && nup_landscape)
	  || ((nup == 1) && landscape) ? "Landscape" : "Portrait");
  output ("%%%%Pages: (atend)\n");
  output ("%%%%DocumentMedia: %s %d %d 0 () ()\n",
	  media->name, media->w, media->h);
  output ("%%%%DocumentNeededResources: (atend)\n");

  if (count_key_value_set (pagedevice) > 0)
    output ("%%%%LanguageLevel: 2\n");

  output ("%%%%EndComments\n");


  /*
   * Procedure Definitions.
   */

  output ("%%%%BeginProlog\n");

  /* Prolog. */
  output ("%%%%BeginResource: procset Enscript-Prolog %s\n",
	  ps_version_string);
  if (!paste_file ("enscript", ".pro"))
    fatal (_("couldn't find prolog \"%s\": %s\n"), "enscript.pro",
	   strerror (errno));
  output ("%%%%EndResource\n");

  /* Encoding vector. */
  output ("%%%%BeginResource: procset Enscript-Encoding-%s %s\n",
	  encoding_name, ps_version_string);
  if (!paste_file (encoding_name, ".enc"))
    fatal (_("couldn't find encoding file \"%s.enc\": %s\n"), encoding_name,
	   strerror (errno));
  output ("%%%%EndResource\n");

  output ("%%%%EndProlog\n");


  /* 
   * Document Setup.
   */

  output ("%%%%BeginSetup\n");

  /* Download fonts. */
  for (got = strhash_get_first (download_fonts, &cp, &j, (void **) &cp2); got;
       got = strhash_get_next (download_fonts, &cp, &j, (void **) &cp2))
    download_font (cp);

  /* For each required font, emit %%IncludeResouce comment. */
  for (got = strhash_get_first (res_fonts, &cp, &j, (void **) &cp2); got;
       got = strhash_get_next (res_fonts, &cp, &j, (void **) &cp2))
    output ("%%%%IncludeResource: font %s\n", cp);

  /* User supplied header? */
  if (page_header)
    {
      output ("/user_header_p true def\n");
      output ("/user_header_str (%s) def\n", page_header);
    }
  else
    output ("/user_header_p false def\n");

  output ("/HFpt_w %g def\n", HFpt.w);
  output ("/HFpt_h %g def\n", HFpt.h);


  /* Select our fonts. */

  /* Header font HF. */
  output ("/%s /HF-gs-font MF\n", HFname);
  output ("/HF /HF-gs-font findfont [HFpt_w 0 0 HFpt_h 0 0] makefont def\n");

  /* Our default typing font F. */
  output ("/%s /F-gs-font MF\n", Fname);
  output ("/F-gs-font findfont [%g 0 0 %g 0 0] makefont setfont\n",
	  Fpt.w, Fpt.h);

  /* Underlay. */
  if (underlay != NULL)
    {
      output ("/ul_str (%s) def\n", underlay);
      output ("/ul_w_ptsize %g def\n", ul_ptsize.w);
      output ("/ul_h_ptsize %g def\n", ul_ptsize.h);
      output ("/ul_gray %g def\n", ul_gray);
      output ("/ul_x %g def\n", ul_x);
      output ("/ul_y %g def\n", ul_y);
      output ("/ul_angle %g def\n", ul_angle);
      output ("/ul_style %d def\n", ul_style);
      output ("/%s /F-ul-font MF\n", ul_font);
      output ("/ul_font /F-ul-font findfont \
[ul_w_ptsize 0 0 ul_h_ptsize 0 0] makefont def\n");
    }

  /* Number of copies. */
  output ("/#copies %d def\n", num_copies);

  /* Page prefeed. */
  if (page_prefeed)
    output ("true page_prefeed\n");

  /* Statusdict definitions. */
  if (count_key_value_set (statusdict) > 0)
    {
      output ("%% Statustdict definitions:\nstatusdict begin\n  ");
      i = 2;
      for (got = strhash_get_first (statusdict, &cp, &j, (void **) &cp2); got;
	   got = strhash_get_next (statusdict, &cp, &j, (void **) &cp2))
	{
	  j = strlen (cp) + 1 + strlen (cp2) + 1;
	  if (i + j > RESOURCE_LINE_WIDTH)
	    {
	      output ("\n  ");
	      i = 2;
	    }
	  output ("%s %s ", cp2, cp);
	  i += j;
	}
      output ("\nend\n");
    }

  /* Page device definitions. */
  if (count_key_value_set (pagedevice) > 0)
    {
      output ("%% Pagedevice definitions:\n");
      output ("gs_languagelevel 1 gt {\n  <<\n    ");
      i = 4;
      for (got = strhash_get_first (pagedevice, &cp, &j, (void **) &cp2); got;
	   got = strhash_get_next (pagedevice, &cp, &j, (void **) &cp2))
	{
	  j = strlen (cp2) + 1 + strlen (cp) + 2;
	  if (i + j > RESOURCE_LINE_WIDTH)
	    {
	      output ("\n    ");
	      i = 4;
	    }
	  output ("/%s %s ", cp, cp2);
	  i += j;
	}
      output ("\n  >> setpagedevice\n} if\n");
    }

  /*
   * Dump header procset.  Header must come after all font inclusions
   * and enscript's dynamic state definition.
   */
  if (header != HDR_NONE)
    {
      char *hdr;
      if (header == HDR_SIMPLE)
	hdr = "simple";
      else
	hdr = fancy_header_name;
      
      output ("%%%%BeginResource: procset Enscript-Header-%s %s\n",
	      hdr, ps_version_string);
      if (!paste_file (hdr, ".hdr"))
	fatal (_("couldn't find header definition file \"%s.hdr\": %s\n"), hdr,
	       strerror (errno));
      output ("%%%%EndResource\n");
    }

  /*
   * Count output width and height here; we can't do it earlier because
   * header might have just allocated some extra space.
   */
  d_output_w = d_page_w;
  d_output_h = d_page_h - d_header_h - d_footer_h;

  /* Dump our current dynamic state. */
  output ("/d_page_w %d def\n", d_page_w);
  output ("/d_page_h %d def\n", d_page_h);

  output ("/d_header_x %d def\n", 0);
  output ("/d_header_y %d def\n", d_output_h + d_footer_h);
  output ("/d_header_w %d def\n", d_header_w);
  output ("/d_header_h %d def\n", d_header_h);

  output ("/d_footer_x %d def\n", 0);
  output ("/d_footer_y %d def\n", 0);
  output ("/d_footer_w %d def\n", d_header_w);
  output ("/d_footer_h %d def\n", d_footer_h);

  output ("/d_output_w %d def\n", d_output_w);
  output ("/d_output_h %d def\n", d_output_h);
  output ("/cols %d def\n", num_columns);

  output ("%%%%EndSetup\n");
}


void
dump_ps_trailer ()
{
  int i, j, got;
  char *cp;
  void *value;
  unsigned int nup_subpage;

  if (!ps_header_dumped)
    /* No header, let's be consistent and forget trailer also. */
    return;

  /* The possible pending N-up showpage. */
  nup_subpage = (total_pages - 1) % nup;
  if (nup > 1 && nup_subpage + 1 != nup)
    {
      /* N-up showpage missing. */
      output ("_R\n");
      output ("S\n");
    }

  /* Trailer. */

  output ("%%%%Trailer\n");

  if (page_prefeed)
    output ("false page_prefeed\n");

  output ("%%%%Pages: %d\n", total_pages);

  /* Document needed resources. */

  /* fonts. */
  output ("%%%%DocumentNeededResources: font ");
  i = 32;			/* length of the previous string. */
  for (got = strhash_get_first (res_fonts, &cp, &j, &value); got;
       got = strhash_get_next (res_fonts, &cp, &j, &value))
    {
      if (i + strlen (cp) + 1 > RESOURCE_LINE_WIDTH)
	{
	  output ("\n%%%%+ font ");
	  i = 9;		/* length of the previous string. */
	}
      output ("%s ", cp);
      i += strlen (cp) + 1;
    }
  output ("\n");

  output ("%%%%EOF\n");
}


void
process_file (char *fname_arg, InputStream *is)
{
  int col;
  double x, y;
  double lx, ly;
  double linewidth;		/* Line width in points. */
  double lineend;
  int done = 0;
  int page_clear = 1;
  unsigned int line_column;
  unsigned int current_linenum;
  double linenumber_space = 0;
  double linenumber_margin = 0;
  Token token;
  int reuse_last_token = 0;
  unsigned int current_slice = 1;
  int last_wrapped_line = -1;
  int last_spaced_file_linenum = -1;

  /* Save filename. */
  strcpy (fname, fname_arg);

  /* Init page number and line counters. */
  current_pagenum = 0;
  total_pages_in_file = 0;
  current_file_linenum = 1;

  /* We got a new input file. */
  input_filenum++;

  /* We haven't printed any line numbers yet. */
  print_line_number_last = (unsigned int) -1;

  if (pass_through && do_pass_through (fname, is))
    return;

  /* We have work to do, let's give header a chance to dump itself. */
  dump_ps_header ();

  /*
   * Align files to the file_align boundary, this is handy for two-side
   * printing.
   */
  while ((total_pages % file_align) != 0)
    {
      dump_empty_page ();
      total_pages++;
    }

  message (1, _("processing file \"%s\"...\n"), fname);

  linewidth = d_output_w / num_columns - 2 * d_output_x_margin
    - line_indent;

  divert ();

  while (!done)
    {
      /* Start a new page. */
      page_clear = 1;

      for (col = 0; !done && col < num_columns; col++)
	{
	  /* Move to the beginning of the column <col>. */
	  lx = x = col * d_output_w / (float) num_columns + d_output_x_margin
	    + line_indent;
	  lineend = lx + linewidth;

	  ly = y = d_footer_h + d_output_h - d_output_y_margin - LINESKIP;
	  current_linenum = 0;
	  line_column = 0;

	  while (1)
	    {
	      if (line_numbers && line_column == 0
		  && (current_file_linenum != last_spaced_file_linenum))
		{
		  /* 
		   * Count the space needed by linenumbers.  This should
		   * be enought for 99999 lines.
		   */

		  linenumber_space = CHAR_WIDTH ('0') * 5 + 1.0;
		  linenumber_margin = CHAR_WIDTH (':') + CHAR_WIDTH ('m');

		  /* Forward x by the amount needed by our line numbers. */
		  x += linenumber_space + linenumber_margin;
		  last_spaced_file_linenum = current_file_linenum;
		}

	      /* Get token. */
	      if (!reuse_last_token)
		get_next_token (is, x, line_column, lineend, &token);
	      reuse_last_token = 0;

	      /*
	       * Page header printing is delayed to this point because
	       * we want to handle files ending with a newline character
	       * with care.  If the last newline would cause a pagebreak,
	       * otherwise we would print page header to the non-existent
	       * next page and that would be ugly ;)
	       */
	      
	      if (token.type == tEOF)
		{
		  done = 1;
		  goto end_of_page;
		}

	      /*
	       * Now we know that we are going to make marks to this page
	       * => print page header.
	       */

	      if (page_clear)
		{
		  PageRange *pr;

		  current_pagenum++;
		  total_pages_in_file++;

		  /* Check page ranges. */
		  if (page_ranges == NULL)
		    do_print = 1;
		  else
		    {
		      do_print = 0;
		      for (pr = page_ranges; pr; pr = pr->next)
			{
			  if (pr->odd || pr->even)
			    {
			      if ((pr->odd && (current_pagenum % 2) == 1)
				  || (pr->even && (current_pagenum % 2) == 0))
				{
				  do_print = 1;
				  break;
				}
			    }
			  else
			    {
			      if (pr->start <= current_pagenum
				  && current_pagenum <= pr->end)
				{
				  do_print = 1;
				  break;
				}
			    }
			}
		    }

		  if (do_print)
		    total_pages++;

		  dump_ps_page_header (fname);
		  page_clear = 0;
		}

	      /* Print line numbers if needed. */
	      if (line_numbers && line_column == 0 && token.type != tFORMFEED)
		print_line_number (lx, y, linenumber_space, linenumber_margin,
				   current_file_linenum);

	      /* Print line highlight. */
	      if (line_column == 0 && line_highlight_gray < 1.0)
		output ("%g %g %g %g %g line_highlight\n",
			lx, (y - baselineskip
			     + (font_bbox_lly * Fpt.h / UNITS_PER_POINT)),
			linewidth, Fpt.h + baselineskip, line_highlight_gray);

	      /* Check rest of tokens. */
	      switch (token.type)
		{
		case tFORMFEED:
		  switch (formfeed_type)
		    {
		    case FORMFEED_COLUMN:
		      goto end_of_column;
		      break;

		    case FORMFEED_PAGE:
		      goto end_of_page;
		      break;
		    }
		  break;

		case tSTRING:
		  if (CORRECT_SLICE ())
		    {
		      output ("%g %g M\n", x, y);
		      output ("(%s) s\n", token.u.str);
		    }
		  x = token.new_x;
		  line_column = token.new_col;
		  break;

		case tCARRIAGE_RETURN:
		  /* Just reset the x-coordinate. */
		  x = col * d_output_w / (float) num_columns
		    + d_output_x_margin + line_indent;
		  line_column = 0;
		  break;
		  
		case tNEWLINE:
		case tWRAPPED_NEWLINE:
		  if (token.type == tNEWLINE)
		    {
		      current_file_linenum++;
		      current_slice = 1;
		      y -= LINESKIP;
		    }
		  else
		    {
		      current_slice++;
		      if (!slicing)
			/*
			 * For wrapped newlines, decrement y only if
			 * we are not slicing the input.
			 */
			y -= LINESKIP;

		      /* Count the wrapped lines here. */
		      if (!slicing || current_slice > slice)
			if (current_file_linenum != last_wrapped_line)
			  {
			    num_truncated_lines++;
			    last_wrapped_line = current_file_linenum;
			  }
		    }

		  current_linenum++;
		  if (current_linenum >= lines_per_page
		      || y < d_footer_h + d_output_y_margin)
		    goto end_of_column;

		  x = col * d_output_w / (float) num_columns
		    + d_output_x_margin + line_indent;
		  line_column = 0;
		  break;

		case tEPSF:
		  /* Count current point movement. */

		  if (token.flags & F_EPSF_ABSOLUTE_Y)
		    token.new_y = ly;
		  else
		    token.new_y = y;
		  token.new_y += token.u.epsf.y - token.u.epsf.h;

		  if (token.flags & F_EPSF_ABSOLUTE_X)
		    token.new_x = lx;
		  else
		    token.new_x = x;
		  token.new_x += token.u.epsf.x;

		  /* Check flags. */
		  
		  /* Justification flags overwrite <x_ofs>. */
		  if (token.flags & F_EPSF_CENTER)
		    token.new_x = lx + (linewidth - token.u.epsf.w) / 2;
		  if (token.flags & F_EPSF_RIGHT)
		    token.new_x = lx + (linewidth - token.u.epsf.w);

		  /* Check if eps file does not fit to this column. */
		  if ((token.flags & F_EPSF_NO_CPOINT_UPDATE_Y) == 0
		      && token.new_y < d_footer_h + d_output_y_margin)
		    {
		      if (current_linenum == 0)
			{
			  /*
			   * At the beginning of the column, warn user
			   * and print image.
			   */
			  message (0, _("EPS file \"%s\" is too \
large for page\n"),
				   token.u.epsf.filename);
			}
		      else
			{
			  /* Must start a new column. */
			  reuse_last_token = 1;
			  goto end_of_column;
			}
		    }

		  /* Do paste. */
		  if (CORRECT_SLICE ())
		    paste_epsf (&token);

		  /* Update current point? */
		  if (!(token.flags & F_EPSF_NO_CPOINT_UPDATE_Y))
		    y = token.new_y;
		  if (!(token.flags & F_EPSF_NO_CPOINT_UPDATE_X))
		    x = token.new_x + token.u.epsf.w;
		  
		  if (y < d_footer_h + d_output_y_margin)
		    goto end_of_column;
		  break;

		case tFONT:
		  /* Select a new current font. */
		  message (2, "^@font=");
		  if (token.u.font.name[0] == '\0')
		    {
		      /* Select the default font. */
		      Fpt.w = default_Fpt.w;
		      Fpt.h = default_Fpt.h;
		      Fname = default_Fname;
		      output ("/F-gs-font findfont \
[%g 0 0 %g 0 0] makefont setfont\n",
			      Fpt.w, Fpt.h);
		      user_fontp = 0;
		    }
		  else
		    {
		      strhash_put (res_fonts, token.u.font.name,
				   strlen (token.u.font.name) + 1,
				   NULL, NULL);
		      output ("/%s /F-gs-user-font MF\n",
			      token.u.font.name);
		      output ("/F-gs-user-font findfont \
[%g 0 0 %g 0 0] makefont setfont\n",
			      token.u.font.size.w, token.u.font.size.h);

		      strcpy (user_font_name, token.u.font.name);
		      user_font_pt.w = token.u.font.size.w;
		      user_font_pt.h = token.u.font.size.h;
		      user_fontp = 1;
		      
		      Fpt.w = user_font_pt.w;
		      Fpt.h = user_font_pt.h;
		      Fname = user_font_name;
		    }
		  message (2, "%s %g/%gpt\n", Fname, Fpt.w, Fpt.h);
		  read_font_info ();
		  break;

		case tCOLOR:
		  /* Select a new color. */
		  message (2, "^@color{%f %f %f}\n",
			   token.u.color.r,
			   token.u.color.g,
			   token.u.color.b);
		  if (token.u.color.r == token.u.color.g
		      && token.u.color.g == token.u.color.b
		      && token.u.color.b == 0.0)
		    {
		      /* Select the default color (black). */
		      output ("0 setgray\n");
		      user_colorp = 0;
		    }
		  else
		    {
		      output ("%g %g %g setrgbcolor\n",
			      token.u.color.r,
			      token.u.color.g,
			      token.u.color.b);

		      user_color.r = token.u.color.r;
		      user_color.g = token.u.color.g;
		      user_color.b = token.u.color.b;
		      user_colorp = 1;
		    }
		  break;

		case tSETFILENAME:
		  strcpy (fname, token.u.filename);
		  break;

		case tSETPAGENUMBER:
		  current_pagenum = token.u.i - 1;
		  break;

		case tNEWPAGE:
		  if (current_linenum >= token.u.i)
		    goto end_of_page;
		  break;

		case tPS:
		  output ("%g %g M\n", x, y);
		  output ("%s\n", token.u.str);
		  xfree (token.u.str);
		  break;

		case tNONE:
		default:
		  fatal("process_file(): got illegal token %d", token.type);
		  break;
		}
	    }
	end_of_column:
	  ;			/* ULTRIX's cc needs this line. */
	}

    end_of_page:
      if (!page_clear)
	dump_ps_page_trailer ();
    }

  /*
   * Reset print flag to true so all the required document trailers
   * etc. get printed properly.
   */
  do_print = 1;
  
  undivert ();
  
  /* Table of contents? */
  if (toc)
    {
      char *cp;

      cp = format_user_string ("TOC", toc_fmt_string);
      fprintf (toc_fp, "%s\n", cp);
      xfree (cp);
    }
}


/*
 * Static functions.
 */

/* Help macros. */

/* Check if character <ch> fits to current line. */
#define FITS_ON_LINE(ch) ((linepos + CHAR_WIDTH (ch) < linew) || col == 0)

/* Is line buffer empty? */
#define BUFFER_EMPTY() (bufpos == 0)

/* Unconditionally append character <ch> to the line buffer. */
#define APPEND_CHAR(ch) 				\
  do {							\
    if (bufpos >= buflen)				\
      {							\
	buflen += 4096;					\
	buffer = xrealloc (buffer, buflen);		\
      }							\
    buffer[bufpos++] = ch;				\
  } while (0)

/* 
 * Copy character <ch> (it fits to this line) to output buffer and
 * update current point counters.
 */
#define EMIT(ch) 		\
  do {				\
    APPEND_CHAR (ch);		\
    linepos += CHAR_WIDTH (ch);	\
    col++;			\
  } while (0)


/* Read one special escape from input <fp>. */

static struct
{
  char *name;
  SpecialEscape escape;
} escapes[] =
  {
    {"comment",		ESC_COMMENT},
    {"epsf", 		ESC_EPSF},
    {"font", 		ESC_FONT},
    {"color",		ESC_COLOR},
    {"newpage",		ESC_NEWPAGE},
    {"ps",		ESC_PS},
    {"setfilename",	ESC_SETFILENAME},
    {"setpagenumber",	ESC_SETPAGENUMBER},
    {"shade",		ESC_SHADE},
    {NULL, 0},
  };
  

static void
read_special_escape (InputStream *is, Token *token)
{
  char escname[256];
  char buf[4096];
  int i, e;
  int ch;

  /* Get escape name. */
  for (i = 0; i < sizeof (escname) - 1 && (ch = is_getc (is)) != EOF; i++)
    {
      if (!isalnum (ch))
	{
	  is_ungetc (ch, is);
	  break;
	}
      else
	escname[i] = ch;
    }
  escname[i] = '\0';

  /* Lookup escape. */
  for (e = 0; escapes[e].name; e++)
    if (strcmp (escname, escapes[e].name) == 0)
      break;
  if (escapes[e].name == NULL)
    fatal (_("unknown special escape: %s"), escname);

  /*
   * The epsf escape takes optional arguments so it must be handled
   * differently.
   */
  if (escapes[e].escape == ESC_EPSF)
    {
      int i;
      int pw, ph;
      double scale;

      token->flags = 0;
      token->u.epsf.x = 0.0;
      token->u.epsf.y = 0.0;
      token->u.epsf.h = 0.0;
      token->u.epsf.pipe = 0;

      ch = is_getc (is);
      if (ch == '[')
	{
	  /* Read options. */
	  while ((ch = is_getc (is)) != EOF && ch != ']')
	    {
	      switch (ch)
		{
		case 'c':	/* center justification */
		  token->flags &= ~M_EPSF_JUSTIFICATION;
		  token->flags |= F_EPSF_CENTER;
		  break;

		case 'n':	/* no current point update */
		  /* Check the next character. */
		  ch = is_getc (is);
		  switch (ch)
		    {
		    case 'x':
		      token->flags |= F_EPSF_NO_CPOINT_UPDATE_X;
		      break;

		    case 'y':
		      token->flags |= F_EPSF_NO_CPOINT_UPDATE_Y;
		      break;

		    default:
		      is_ungetc (ch, is);
		      token->flags |= F_EPSF_NO_CPOINT_UPDATE_X;
		      token->flags |= F_EPSF_NO_CPOINT_UPDATE_Y;
		      break;
		    }
		  break;

		case 'r':	/* right justification */
		  token->flags &= ~M_EPSF_JUSTIFICATION;
		  token->flags |= F_EPSF_RIGHT;
		  break;


		case 's':	/* scale */
		  /* Check the next character. */
		  ch = is_getc (is);
		  switch (ch)
		    {
		    case 'x':
		      token->flags |= F_EPSF_SCALE_X;
		      token->u.epsf.xscale = read_float (is, 0, 1);
		      break;

		    case 'y':
		      token->flags |= F_EPSF_SCALE_Y;
		      token->u.epsf.yscale = read_float (is, 0, 0);
		      break;

		    default:
		      is_ungetc (ch, is);
		      token->flags |= F_EPSF_SCALE_X;
		      token->flags |= F_EPSF_SCALE_Y;
		      token->u.epsf.xscale = token->u.epsf.yscale
			= read_float (is, 0, 1);
		      break;
		    }
		  break;

		case 'x':	/* x-position */
		  token->u.epsf.x = read_float (is, 1, 1);

		  /* Check the next character. */
		  ch = is_getc (is);
		  switch (ch)
		    {
		    case 'a':
		      token->flags |= F_EPSF_ABSOLUTE_X;
		      break;

		    default:
		      is_ungetc (ch, is);
		      break;
		    }
		  break;

		case 'y':	/* y-position */
		  token->u.epsf.y = - read_float (is, 1, 0);

		  /* Check the next character. */
		  ch = is_getc (is);
		  switch (ch)
		    {
		    case 'a':
		      token->flags |= F_EPSF_ABSOLUTE_Y;
		      break;

		    default:
		      is_ungetc (ch, is);
		      break;
		    }
		  break;

		case 'h':	/* height */
		  token->u.epsf.h = read_float (is, 1, 0);
		  break;

		case ' ':
		case '\t':
		  break;

		default:
		  fatal (_("illegal option %c for ^@epsf escape"), ch);
		}
	    }	      
	  if (ch != ']')
	    fatal (_("malformed ^@epsf escape: no ']' after options"));

	  ch = is_getc (is);
	}
      if (ch == '{')
	{
	  /* Read filename. */
	  for (i = 0; (ch = is_getc (is)) != EOF && ch != '}'; i++)
	    {
	      token->u.epsf.filename[i] = ch;
	      if (i + 1 >= sizeof (token->u.epsf.filename))
		fatal (_("too long file name for ^@epsf escape:\n%.*s"),
		       i, token->u.epsf.filename);
	    }
	  if (ch == EOF)
	    fatal (_("unexpected EOF while scanning ^@epsf escape"));

	  token->u.epsf.filename[i] = '\0';
	  token->type = tEPSF;
	}
      else
	fatal (_("malformed ^@epsf escape: no '{' found"));

      /* 
       * Now we have a valid epsf-token in <token>.  Let's read BoundingBox
       * and do some calculations.
       */
      if (!recognize_eps_file (token))
	/* Recognize eps has already printed error message so we are done. */
	token->type = tNONE;
      else
	{
	  /* Some fixups for x and y dimensions. */
	  token->u.epsf.y += LINESKIP - 1;
	  if (token->u.epsf.h != 0.0)
	    token->u.epsf.h -= 1.0;

	  /* Count picture's width and height. */

	  pw = token->u.epsf.urx - token->u.epsf.llx;
	  ph = token->u.epsf.ury - token->u.epsf.lly;

	  /* The default scale. */
	  if (token->u.epsf.h == 0.0)
	    scale = 1.0;
	  else
	    scale = token->u.epsf.h / ph;

	  if ((token->flags & F_EPSF_SCALE_X) == 0)
	    token->u.epsf.xscale = scale;
	  if ((token->flags & F_EPSF_SCALE_Y) == 0)
	    token->u.epsf.yscale = scale;

	  pw *= token->u.epsf.xscale;
	  ph *= token->u.epsf.yscale;

	  token->u.epsf.w = pw;
	  token->u.epsf.h = ph;
	}
    }
  else if (escapes[e].escape == ESC_COMMENT)
    {
      /* Comment the rest of this line. */
      while ((ch = is_getc (is)) != EOF && ch != nl)
	;
      token->type = tNONE;
    }
  else 
    {
      char *cp;
      int parenlevel;

      /*
       * Handle the rest of the escapes.
       */

      /* Read argument. */
      ch = is_getc (is);
      if (ch != '{')
	fatal (_("malformed %s escape: no '{' found"), escapes[e].name);

      parenlevel = 0;
      for (i = 0;
	   (ch = is_getc (is)) != EOF && (parenlevel > 0 || ch != '}'); i++)
	{
	  if (ch == '{')
	    parenlevel++;
	  else if (ch == '}')
	    parenlevel--;

	  buf[i] = ch;
	  if (i + 1 >= sizeof (buf))
	    fatal (_("too long argument for %s escape:\n%.*s"),
		   escapes[i].name, i, buf);
	}
      buf[i] = '\0';

      /* And now handle the escape. */
      switch (escapes[e].escape)
	{
	case ESC_FONT:
	  strcpy (token->u.font.name, buf);

	  /* Check for the default font. */
	  if (strcmp (token->u.font.name, "default") == 0)
	    token->u.font.name[0] = '\0';
	  else
	    {
	      if (!parse_font_spec (token->u.font.name, &cp,
				    &token->u.font.size))
		fatal (_("malformed font spec for ^@font escape: %s"),
		       token->u.font.name);
	      
	      strcpy (token->u.font.name, cp);
	      xfree (cp);
	    }
	  token->type = tFONT;
	  break;

	case ESC_COLOR:
	  /* Check for the default color. */
	  if (strcmp (buf, "default") == 0)
	    {
	      token->u.color.r = 0;
	      token->u.color.g = 0;
	      token->u.color.b = 0;
	    }
	  else
	    {
	      int got;

	      got = sscanf (buf, "%g %g %g",
			    &token->u.color.r,
			    &token->u.color.g,
			    &token->u.color.b);
	      switch (got)
		{
		case 0:
		case 2:
		  fatal (_("malformed color spec for ^@color escape: %s"),
			 buf);
		  break;

		case 1:
		  token->u.color.g = token->u.color.b = token->u.color.r;
		  break;

		default:
		  /* Got all three components. */
		  break;
		}
	    }
	  token->type = tCOLOR;
	  break;

	case ESC_SHADE:
	  line_highlight_gray = atof (buf);
	  if (line_highlight_gray < 0.0 || line_highlight_gray > 1.0)
	    fatal (_("invalid value for ^@shade escape: %s"), buf);

	  token->type = tNONE;
	  break;

	case ESC_SETFILENAME:
	  strcpy (token->u.filename, buf);
	  token->type = tSETFILENAME;
	  break;

	case ESC_SETPAGENUMBER:
	  token->u.i = atoi (buf);
	  token->type = tSETPAGENUMBER;
	  break;

	case ESC_NEWPAGE:
	  if (i == 0)
	    token->u.i = 1;	/* The default is the first line. */
	  else
	    token->u.i = atoi (buf);
	  token->type = tNEWPAGE;
	  break;

	case ESC_PS:
	  token->u.str = xstrdup (buf);
	  token->type = tPS;
	  break;

	default:
	  /* NOTREACHED */
	  abort ();
	  break;
	}
    }
}


/* Get next token from input file <fp>. */
static void
get_next_token (InputStream *is, double linepos, unsigned int col,
		double linew, Token *token)
{
  static unsigned char *buffer = NULL; /* output buffer */
  static unsigned int buflen = 0; /* output buffer's length */
  unsigned int bufpos = 0;	/* current position in output buffer */
  int ch = 0;
  int done = 0;
  int i;
  static int pending_token = tNONE;

  if (pending_token != tNONE)
    {
      token->type = pending_token;
      pending_token = tNONE;
      return;
    }

#define DONE_DONE 1
#define DONE_WRAP 2

  while (!done)
    {
      ch = is_getc (is);
      switch (ch)
	{
	case EOF:
	  if (BUFFER_EMPTY ())
	    {
	      token->type = tEOF;
	      return;
	    }

	  done = DONE_DONE;
	  break;

	case '(':
	case ')':
	case '\\':
	  if (FITS_ON_LINE (ch))
	    {
	      APPEND_CHAR ('\\');
	      EMIT (ch);
	    }
	  else
	    {
	      is_ungetc (ch, is);
	      done = DONE_WRAP;
	    }
	  break;

	case '\r':
	case '\n':
	  /*
	   * One of these is the newline character and the other one
	   * is carriage return.
	   */
	  if (ch == nl)
	    {
	      /* The newline character. */
	      if (BUFFER_EMPTY ())
		{
		  token->type = tNEWLINE;
		  return;
		}
	      else
		{
		  is_ungetc (ch, is);
		  done = DONE_DONE;
		}
	    }
	  else
	    {
	      /* The carriage return character. */
	      if (BUFFER_EMPTY ())
		{
		  token->type = tCARRIAGE_RETURN;
		  return;
		}
	      else
		{
		  is_ungetc (ch, is);
		  done = DONE_DONE;
		}
	    }
	  break;

	case '\t':
	  if (font_is_fixed)
	    {
	      i = tabsize - (col % tabsize);
	      for (; i > 0; i--)
		{
		  if (FITS_ON_LINE (' '))
		    EMIT (' ');
		  else
		    {
		      done = DONE_WRAP;
		      break;
		    }
		}
	    }
	  else
	    {
	      /* Proportional font. */

	      double grid = tabsize * CHAR_WIDTH ('m');
	      col++;

	      /* Move linepos to the next multiple of <grid>. */
	      linepos = ((int) (linepos / grid) + 1) * grid;
	      if (linepos >= linew)
		done = DONE_WRAP;
	      else
		done = DONE_DONE;
	    }
	  break;

	case '\f':
	  if (BUFFER_EMPTY ())
	    {
	      if (interpret_formfeed)
		token->type = tFORMFEED;
	      else
		token->type = tNEWLINE;
	      return;
	    }
	  else
	    {
	      is_ungetc (ch, is);
	      done = DONE_DONE;
	    }
	  break;

	default:
	  /* Handle special escapes. */
	  if (special_escapes && ch == escape_char)
	    {
	      if (BUFFER_EMPTY ())
		{
		  /* Interpret special escapes. */
		  read_special_escape (is, token);
		  if (token->type != tNONE)
		    return;

		  /*
		   * Got tNONE special escape => read_special_escape()
		   * has already done what was needed.  Just read more.
		   */
		  break;
		}
	      else
		{
		  is_ungetc (ch, is);
		  done = DONE_DONE;
		  break;
		}
	    }

	  /* Handle backspace character. */
	  if (ch == bs)
	    {
	      if (BUFFER_EMPTY () || !EXISTS (buffer[bufpos - 1]))
		linepos -= CHAR_WIDTH ('m');
	      else
		linepos -= CHAR_WIDTH (buffer[bufpos - 1]);

	      done = DONE_DONE;
	      break;
	    }

	  /* Check normal characters. */
	  if (EXISTS (ch))
	    {
	      if (FITS_ON_LINE (ch))
		{
		  /*
		   * Print control characters (and optionally
		   * characters greater than 127) in the escaped form
		   * so PostScript interpreter will not hang on them.
		   */
		  if (ch < 040 || (clean_7bit && ch >= 0200))
		    {
		      char buf[10];

		      sprintf (buf, "\\%03o", ch);
		      for (i = 0; buf[i]; i++)
			APPEND_CHAR (buf[i]);
		      
		      /* Update current point counters manually. */
		      linepos += CHAR_WIDTH (ch);
		      col++;
		    }
		  else
		    EMIT (ch);
		}
	      else
		{
		  is_ungetc (ch, is);
		  done = DONE_WRAP;
		}
	    }
	  else if (ISPRINT (ch))
	    {
	      /* Printable, but do not exists in this font. */
	      if (FITS_ON_LINE ('?'))
		{
		  EMIT ('?');
		  if (missing_chars[ch]++ == 0)
		    num_missing_chars++;
		}
	      else
		{
		  is_ungetc (ch, is);
		  done = DONE_WRAP;
		}
	    }
	  else
	    {
	      char buf[20];
	      double len = 0.0;

	      /*
	       * Non-printable and does not exist in current font, print
	       * it in the format specified by non_printable_format.
	       */
	      
	      if (non_printable_chars[ch]++ == 0)
		num_non_printable_chars++;

	      switch (non_printable_format)
		{
		case NPF_SPACE:
		  strcpy (buf, " ");
		  break;

		case NPF_QUESTIONMARK:
		  strcpy (buf, "?");
		  break;

		case NPF_CARET:
		  if (ch < 0x20)
		    {
		      buf[0] = '^';
		      buf[1] = '@' + ch;
		      buf[2] = '\0';
		      break;
		    }
		  /* FALLTHROUGH */

		case NPF_OCTAL:
		  sprintf (buf, "\\%03o", ch);
		  break;
		}

	      /* Count length. */
	      for (i = 0; buf[i]; i++)
		len += CHAR_WIDTH (buf[i]);

	      if (linepos + len < linew || col == 0)
		{
		  /* Print it. */
		  for (i = 0; buf[i]; i++)
		    {
		      if (buf[i] == '\\')
			APPEND_CHAR ('\\'); /* Escape '\\' characters. */
		      EMIT (buf[i]);
		    }
		}
	      else
		{
		  is_ungetc (ch, is);
		  done = DONE_WRAP;
		}
	    }
	  break;
	}
    }

  /* Got a string. */

  /* Check for wrapped line. */
  if (done == DONE_WRAP)
    {
      /* This line is too long. */
      ch = nl;
      if (truncate_lines)
	{
	  /* Truncate this line. */
	  while ((ch = is_getc (is)) != EOF && ch != nl)
	    ;
	}
      if (ch == nl)
	{
	  if (truncate_lines)
	    pending_token = tNEWLINE;
	  else
	    pending_token = tWRAPPED_NEWLINE;
	}
      else
	pending_token = tEOF;
    }

  APPEND_CHAR ('\0');
  token->type = tSTRING;
  token->u.str = (char *) buffer;
  token->new_x = linepos;
  token->new_col = col;
}


static void
dump_ps_page_header (char *fname)
{
  char buf[512];
  char *ftail;
  int got, i;
  char *cp, *cp2;
  char *cstr = "%%";
  unsigned int nup_subpage;

  /* The N-up printing sub-page. */
  nup_subpage = (total_pages - 1) % nup;

  /* Create fdir and ftail. */
  ftail = strrchr (fname, '/');
  if (ftail == NULL)
    {
      buf[0] = '\0';
      ftail = fname;
    }
  else
    {
      ftail++;
      strncpy (buf, fname, ftail - fname);
      buf[ftail - fname] = '\0';
    }
  
  if (nup > 1)
    {
      /* N-up printing is active. */
      cstr = "%";

      if (nup_subpage == 0)
	{
	  /* This is a real page start. */

	  switch (page_label)
	    {
	    case LABEL_SHORT:
	      output ("%%%%Page: (%d-%d) %d\n", current_pagenum,
		      current_pagenum + nup - 1, total_pages / nup + 1);
	      break;

	    case LABEL_LONG:
	      output ("%%%%Page: (%s:%3d-%3d) %d\n", ftail, current_pagenum,
		      current_pagenum + nup - 1, total_pages / nup + 1);
	      break;
	    }

	  /* Page setup. */
	  output ("%%%%BeginPageSetup\n");
	  output ("_S\n");

#define PRINT_BOUNDING_BOXES 0

#if PRINT_BOUNDING_BOXES
	  output ("%d %d moveto %d %d lineto %d %d lineto %d %d lineto closepath stroke\n",
		  media->llx, media->lly, media->llx, media->ury,
		  media->urx, media->ury, media->urx, media->lly);
#endif

	  if (landscape)
	    {
	      if (nup_landscape)
		{
		  output ("90 rotate\n");
		  output ("%d %d translate\n", media->lly, -media->urx);
		}
	      else
		output ("%d %d translate\n", media->llx, media->lly);
	    }
	  else
	    {
	      if (nup_landscape)
		{
		  output ("90 rotate\n");
		  output ("%d %d translate\n", media->lly, -media->llx);
		}
	      else
		output ("%d %d translate\n", media->llx, media->ury);
	    }

	  /*
	   * Scale us down a bit so the page can be printed in its
	   * bouding box.
	   */
	  output ("%f dup scale\n",
		  (float) (media->urx - media->llx) / media->w);
	}
    }

  /* Page start comment. */
  switch (page_label)
    {
    case LABEL_SHORT:
      output ("%sPage: (%d) %d\n", cstr, current_pagenum, total_pages);
      break;

    case LABEL_LONG:
      output ("%sPage: (%s:%3d) %d\n", cstr, ftail, current_pagenum,
	      total_pages);
      break;
    }

  /* 
   * Page Setup.
   */

  output ("%sBeginPageSetup\n", cstr);

  output ("_S\n");

  if (nup > 1)
    {
      output ("%% N-up sub-page %d/%d\n", nup_subpage + 1, nup);
      if (landscape)
	output ("%d %d translate\n",
		(nup_subpage / nup_rows) * nup_width,
		(nup_subpage % nup_rows) * nup_height);
      else
	output ("%d %d translate\n",
		(nup_subpage % nup_columns) * nup_width,
		- ((nup_subpage / nup_columns + 1) * nup_height));
      output ("%g dup scale\n", nup_scale);
    }

#if PRINT_BOUNDING_BOXES
	  output ("%d %d moveto %d %d lineto %d %d lineto %d %d lineto closepath stroke\n",
		  media->llx, media->lly, media->llx, media->ury,
		  media->urx, media->ury, media->urx, media->lly);
#endif

  if (landscape)
    {
      output ("90 rotate\n");
      output ("%d %d translate\n", media->lly, -media->urx);
    }
  else
    output ("%d %d translate\n", media->llx, media->lly);

  output ("/pagenum %d def\n", current_pagenum);
  output ("/fname (%s) def\n", fname);
  output ("/fdir (%s) def\n", buf);
  output ("/ftail (%s) def\n", ftail);

  /* Do we have a pending ^@font{} font? */
  if (user_fontp)
    {
      output ("/%s /F-gs-user-font MF\n", Fname);
      output ("/F-gs-user-font findfont [%g 0 0 %g 0 0] makefont setfont\n",
	      Fpt.w, Fpt.h);
    }

  /* Dump user defined strings. */
  if (count_key_value_set (user_strings) > 0)
    {
      output ("%% User defined strings:\n");
      for (got = strhash_get_first (user_strings, &cp, &i, (void **) &cp2);
	   got;
	   got = strhash_get_next (user_strings, &cp, &i, (void **) &cp2))
	{
	  cp2 = format_user_string ("%Format", cp2);
	  output ("/%s (%s) def\n", cp, cp2);
	  xfree (cp2);
	}
    }

  output ("%%%%EndPageSetup\n");


  /* 
   * Mark standard page decorations.
   */

  /* Highlight bars. */
  if (highlight_bars)
    output ("%d %f %d %f highlight_bars\n", highlight_bars,
	    LINESKIP, d_output_y_margin, highlight_bar_gray);
  
  /* Underlay. */
  if (underlay != NULL)
    {
      if (ul_position_p || ul_angle_p)
	output ("user_underlay\n");
      else
	output ("underlay\n");
    }

  /* Column lines. */
  if (num_columns > 1 && (header == HDR_FANCY || borders))
    output ("column_lines\n");

  /* Borders around columns. */
  if (borders)
    output ("column_borders\n");

  /* Header. */
  switch (header)
    {
    case HDR_NONE:
      break;

    case HDR_SIMPLE:
    case HDR_FANCY:
      output ("do_header\n");
      break;
    }

  /* Do we have a pending ^@color{} color? */
  if (user_colorp)
    output ("%g %g %g setrgbcolor\n", user_color.r, user_color.g,
	    user_color.b);
}


static void
dump_ps_page_trailer ()
{
  unsigned int nup_subpage = (total_pages - 1) % nup;

  output ("_R\n");

  if (nup > 1)
    {
      if (nup_subpage + 1 == nup)
	{
	  /* Real end of page. */
	  output ("_R\n");
	  output ("S\n");
	}
    }
  else
    output ("S\n");
}


static void
dump_empty_page ()
{
  if (nup > 1)
    {
      unsigned int nup_subpage = (total_pages - 1) % nup;

      if (nup_subpage == 0)
	{
	  /* Real start of the page, must do it by the harder way. */
	  dump_ps_page_header (fname);
	}
      else
	output ("%%Page: (-) %d\n", total_pages);

      if (nup_subpage + 1 == nup)
	{
	  /* This is the last page on this sheet, dump us. */
	  output ("_R\n");
	  output ("S\n");
	}
    }
  else
    {
      output ("%%%%Page: (-) %d\n", total_pages);
      output ("S\n");
    }
}


static int
recognize_eps_file (Token *token)
{
  int i;
  char filename[512];
  char buf[4096];
  int line;
  int valid_epsf;
  float llx, lly, urx, ury;

  message (2, "^@epsf=\"%s\"\n", token->u.epsf.filename);

  i = strlen (token->u.epsf.filename);
  if (i > 0 && token->u.epsf.filename[i - 1] == '|')
    {
      /* Read EPS data from pipe. */
      token->u.epsf.pipe = 1;
      token->u.epsf.filename[i - 1] = '\0';
      token->u.epsf.fp = popen (token->u.epsf.filename, "r");
      if (token->u.epsf.fp == NULL)
	{
	  message (0, _("epsf: couldn't open pipe to command \"%s\": %s\n"),
		   token->u.epsf.filename, strerror (errno));
	  return 0;
	}
    }
  else
    {
      /* Read EPS data from file. */
      tilde_subst (token->u.epsf.filename, filename);

      token->u.epsf.fp = fopen (filename, "rb");
      if (token->u.epsf.fp == NULL)
	{
	  if (token->u.epsf.filename[0] != '/')
	    {
	      /* Name is not absolute, let's lookup path. */
	      FileLookupCtx ctx;

	      strcpy (ctx.name, token->u.epsf.filename);
	      strcpy (ctx.suffix, "");

	      if (pathwalk (libpath, file_lookup, &ctx))
		token->u.epsf.fp = fopen (ctx.fullname, "rb");
	    }
	  if (token->u.epsf.fp == NULL)
	    {
	      message (0, _("couldn't open EPS file \"%s\": %s\n"),
		       token->u.epsf.filename, strerror (errno));
	      return 0;
	    }
	}
    }

  /* Find BoundingBox DSC comment. */

  line = 0;
  valid_epsf = 0;
  token->u.epsf.skipbuf = NULL;
  token->u.epsf.skipbuf_len = 0;
  token->u.epsf.skipbuf_pos = 0;

  while (fgets (buf, sizeof (buf), token->u.epsf.fp))
    {
      line++;

      /* Append data to the skip buffer. */
      i = strlen (buf);
      if (i + token->u.epsf.skipbuf_pos >= token->u.epsf.skipbuf_len)
	{
	  token->u.epsf.skipbuf_len += 8192;
	  token->u.epsf.skipbuf = xrealloc (token->u.epsf.skipbuf,
					    token->u.epsf.skipbuf_len);
	}
      memcpy (token->u.epsf.skipbuf + token->u.epsf.skipbuf_pos, buf, i);
      token->u.epsf.skipbuf_pos += i;

      /* Check the "%!" magic cookie. */
      if (line == 1)
	{
	  if (buf[0] != '%' || buf[1] != '!')
	    {
	      message (0,
		       _("EPS file \"%s\" does not start with \"%%!\" magic\n"),
		       token->u.epsf.filename);
	      break;
	    }
	}

#define BB_DSC "%%BoundingBox:"

      if (strncmp (buf, BB_DSC, strlen (BB_DSC)) == 0)
	{
	  i = sscanf (buf + strlen (BB_DSC), "%f %f %f %f",
		      &llx, &lly, &urx, &ury);
	  if (i != 4)
	    {
	      /* (atend) ? */

	      /* Skip possible whitespace. */
	      for (i = strlen (BB_DSC);
		   buf[i] && (buf[i] == ' ' || buf[i] == '\t');
		   i++)
		;
#define BB_DSC_ATEND "(atend)"
	      if (strncmp (buf + i, BB_DSC_ATEND, strlen (BB_DSC_ATEND)) != 0)
		{
		  /* No, this BoundingBox comment is corrupted. */
		  message (0, _("EPS file \"%s\" contains malformed \
%%%%BoundingBox row:\n\"%.*s\"\n"),
			   token->u.epsf.filename, strlen (buf) - 1, buf);
		  break;
		}
	    }
	  else
	    {
	      /* It was a valid EPS file. */

	      /* We store bounding box in int format. */
	      token->u.epsf.llx = llx;
	      token->u.epsf.lly = lly;
	      token->u.epsf.urx = urx;
	      token->u.epsf.ury = ury;

	      valid_epsf = 1;
	      break;
	    }
	}
    }

  /* Check that we found the BoundingBox comment. */
  if (!valid_epsf)
    {
      message (0, _("EPS file \"%s\" is not a valid EPS file\n"),
	       token->u.epsf.filename);
      if (token->u.epsf.pipe)
	pclose (token->u.epsf.fp);
      else
	fclose (token->u.epsf.fp);
      xfree (token->u.epsf.skipbuf);
      return 0;
    }

  message (2, "BoundingBox: %d %d %d %d\n",
	   token->u.epsf.llx, token->u.epsf.lly,
	   token->u.epsf.urx, token->u.epsf.ury);

  return 1;
}


static void
paste_epsf (Token *token)
{
  char buf[4096];
  int i;
  
  /* EPSF import header. */
  output ("BeginEPSF\n");
  output ("%g %g translate\n", token->new_x, token->new_y);
  output ("%g %g scale\n", token->u.epsf.xscale, token->u.epsf.yscale);
  output ("%d %d translate\n", -token->u.epsf.llx,
	  -token->u.epsf.lly);
  output ("%d %d %d %d Box clip newpath\n",
	  token->u.epsf.llx - 1,
	  token->u.epsf.lly - 1,
	  token->u.epsf.urx - token->u.epsf.llx + 2,
	  token->u.epsf.ury - token->u.epsf.lly + 2);
  output ("%%%%BeginDocument: %s%s\n", token->u.epsf.filename,
	  token->u.epsf.pipe ? "|" : "");

  if (do_print)
    {
      /* Dump skip buffer. */
      fwrite (token->u.epsf.skipbuf, 1, token->u.epsf.skipbuf_pos, cofp);

      /* Dump file. */
      while ((i = fread (buf, 1, sizeof (buf), token->u.epsf.fp)) != 0)
	fwrite (buf, 1, i, cofp);
    }

  /* Add a newline to keep comments correct */
  output ("\n");

  /* EPSF import trailer. */
  output ("%%%%EndDocument\n");
  output ("EndEPSF\n");

  /* Cleanup. */
  if (token->u.epsf.pipe)
    pclose (token->u.epsf.fp);
  else
    fclose (token->u.epsf.fp);
  xfree (token->u.epsf.skipbuf);
}


static double
read_float (InputStream *is, int units, int horizontal)
{
  char buf[256];
  int i, ch;
  double val;

  for (i = 0; (i < sizeof (buf) - 1
	       && (ch = is_getc (is)) != EOF
	       && ISNUMBERDIGIT (ch)); 
       i++)
    buf[i] = ch;
  buf[i] = '\0';
  if (ch != EOF)
    is_ungetc (ch, is);

  val = atof (buf);

  if (units)
    {
      /* Get unit. */
      ch = is_getc (is);
      switch (ch)
	{
	case 'c':		/* centimeters */
	  val *= 72 / 2.54;
	  break;

	case 'p':		/* PostScript points */
	  break;

	case 'i':		/* inches */
	  val *= 72;
	  break;

	default:
	  is_ungetc (ch, is);
	  /* FALLTHROUGH */

	case 'l':		/* lines or characters */
	  if (horizontal)
	    val *= CHAR_WIDTH ('m');
	  else
	    val *= LINESKIP;
	  break;
	}
    }

  return val;
}


/* Magics used to recognize different pass-through files. */
static struct
{
  char *magic;
  unsigned int magiclen;
  char *name;
  int revert_delta;
} pass_through_magics[] =
  {
    {"%!", 	2, "PostScript", 	-2},
    {"\004%!",	3, "PostScript", 	-2},
    {"\033E",	2, "PCL",		-2},
    {"\033%",	2, "PCL",		-2},
    {NULL, 0, NULL, 0},
  };


static int
do_pass_through (char *fname, InputStream *is)
{
  int ch;
  unsigned long saved_pos = is->bufpos;
  int i, j;

  /*
   * Try to recognize pass-through files.
   */

  for (i = 0; pass_through_magics[i].magic; i++)
    {
      for (j = 0; j < pass_through_magics[i].magiclen; j++)
	{
	  ch = is_getc (is);
	  if (ch == EOF
	      || ch != (unsigned char) pass_through_magics[i].magic[j])
	    break;
	}

      if (j >= pass_through_magics[i].magiclen)
	/* The <i>th one matched. */
	break;
      
      /* Try the next one, but first, seek the input stream to its start. */
      is->bufpos = saved_pos;
    }

  /* Did we find any? */
  if (pass_through_magics[i].magic == NULL)
    /* No we didn't. */
    return 0;

  /* Yes, it really is a pass-through file.  Now do the pass through. */

  if (ps_header_dumped)
    {
      /* A pass-through file between normal ASCII files, obey DSC. */

      /* 
       * XXX I don't know how to handle PCL files... Let's hope none
       * mixes them with the normal ASCII files.
       */

      output ("%%%%Page: (%s) -1\n", fname);
      output ("_S\n");
      output ("%%%%BeginDocument: %s\n", fname);
    }

  message (1, _("passing through %s file \"%s\"\n"),
	   pass_through_magics[i].name, fname);
  is->bufpos += pass_through_magics[i].revert_delta;
  do
    {
      /* Note: this will be written directly to the <ofp>. */
      fwrite (is->buf + is->bufpos, 1, is->data_in_buf - is->bufpos, ofp);
      is->bufpos = is->data_in_buf;

      /* Read more data to the input buffer. */
      ch = is_getc (is);
      is->bufpos = 0;
    }
  while (ch != EOF);

  if (ps_header_dumped)
    {
      /*
       * XXX How to end a PCL file mixed between ASCII files?
       */
      output ("%%%%EndDocument\n");
      output ("_R\n");
    }

  return 1;
}


static void
print_line_number (double x, double y, double space, double margin,
		   unsigned int linenum)
{
  double len = 0.0;
  char buf[20];
  int i;

  /* Do not print linenumbers for wrapped lines. */
  if (linenum == print_line_number_last)
    return;
  print_line_number_last = linenum;

  /* Count linenumber string length. */
  sprintf (buf, "%d", linenum);
  for (i = 0; buf[i]; i++)
    len += CHAR_WIDTH (buf[i]);

  output ("%g %g M (%s:) s\n", x + space - len, y, buf);
}


static void
#if defined(HAVE_STDARG_H) && defined(__STDC__) && __STDC__
  output (char *fmt, ...)
#else
  output (va_alist)
     va_dcl
#endif
{
  va_list args;
#if defined(HAVE_STDARG_H) && defined(__STDC__) && __STDC__
  va_start (args, fmt);
#else
  char *fmt;
  
  va_start (args);
  fmt = va_arg (args, char *);
#endif

  if (cofp == NULL)
    cofp = ofp;

  if (do_print)
    {
#if HAVE_VPRINTF
      vfprintf (cofp, fmt, args);
#else
      _doprnt (fmt, args, cofp);
#endif
    }

  va_end (args);
}


/*
 * The name of the divert file, shared between divert() and undivert()
 * functions.
 */
static char divertfname[512];

static void
divert ()
{
  char buf[512];
  char *cp;

  assert (divertfp == NULL);

  /* Open divert file. */

  cp = tmpnam (buf);
  if (cp == NULL)
    fatal (_("couldn't create divert file name: %s"), strerror (errno));

  strcpy (divertfname, cp);

  divertfp = fopen (divertfname, "w+b");
  if (divertfp == NULL)
    fatal (_("couldn't create divert file \"%s\": %s"), divertfname,
	   strerror (errno));

  if (remove (divertfname) == 0)
    /* Remove successfull, no need to remove file in undivert(). */
    divertfname[0] = '\0';

  cofp = divertfp;
}


static void
undivert ()
{
  char buf[1024];
  int doc_level = 0;
  char *cp;

  assert (divertfp != NULL);

  if (fseek (divertfp, 0, SEEK_SET) != 0)
    fatal (_("couldn't rewind divert file: %s"), strerror (errno));

  while (fgets (buf, sizeof (buf), divertfp))
    {
      if (strncmp (buf, "%%BeginDocument", 15) == 0)
	doc_level++;
      else if (strncmp (buf, "%%EndDocument", 13) == 0)
	doc_level--;

      if (doc_level == 0)
	{
	  if (strncmp (buf, "% User defined strings", 22) == 0)
	    {
	      fputs (buf, ofp);
	      while (fgets (buf, sizeof (buf), divertfp))
		{
		  if (strncmp (buf, "%%EndPageSetup", 14) == 0)
		    break;

		  /* Patch total pages to the user defined strings. */
		  cp = strchr (buf, '\001');
		  if (cp)
		    {
		      *cp = '\0';
		      fputs (buf, ofp);
		      fprintf (ofp, "%d", total_pages_in_file);
		      fputs (cp + 1, ofp);
		    }
		  else
		    fputs (buf, ofp);
		}
	    }
	}

      fputs (buf, ofp);
    }

  fclose (divertfp);
  divertfp = NULL;

  /* Do we have to remove the divert file? */
  if (divertfname[0])
    (void) remove (divertfname);

  cofp = ofp;
}
