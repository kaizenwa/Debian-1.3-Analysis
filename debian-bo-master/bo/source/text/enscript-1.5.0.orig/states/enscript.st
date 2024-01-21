/*
 * States definitions file for GNU Enscript.
 * Copyright (c) 1997 Markku Rossi.
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

/*
 * $Id: enscript.st,v 1.20 1997/02/21 14:34:26 mtr Exp $
 */

/*
 * Guildelines for writing new highlighting rules for the GNU Enscript.
 *
 * - all pretty-printing states should have a document comment like this:
 *
 *   /**
 *    * Name: c
 *    * Description: C programming language.
 *    * ...
 *
 *   It is used by enscript's --help-pretty-print option to print
 *   description about supported pretty-printing modes. 
 *
 * - BEGIN and END rules must call header() and trailer() subroutines:
 *
 *   state c
 *   {
 *     BEGIN {
 *       header ();
 *     }
 *     END {
 *       trailer ();
 *     }
 *
 * - each state and possible help-states should have LANGUAGE_SPECIALS rule:
 * 
 *     LANGUAGE_SPECIALS {
 *       language_print ($0);
 *     }
 *
 * - all printing should be done with the language_print() procedure
 *   instead of the print() primitive. 
 *
 * These rules ensures that enscript's --help-pretty-print option works
 * and that the many2html utility script can convert all the
 * --pretty-print supported languages to HTML.
 */

/* This script needs at least this version of the States program. */
prereq ("1.4.5");

/*
 * Constants, fonts, etc.
 */

debug = "0";

/* Boolean values. */
true = 1;
false = 0;

font_spec = "Courier@10";

/* These components are resolved from <font_spec>. */
font = "";
ptsize = "";

/*
 * Highlight levels.  Possible values are:
 * - none       no highlighting
 * - light      light highlighting (comments and highlights)
 * - heavy      give all
 */
hl_level = "heavy";

/*
 * Colormodel.  Possible values are:
 * - blackwhite         no colors, just black and white
 * - emacs              similar to emacs' font lock highlighting
 */
colormodel = "blackwhite";

/*
 * Target language.  Possible values are:
 * - enscript           generate enscript special escapes
 * - html               generate HTML
 * - texinfo            generate Texinfo
 */

language = "enscript";


/*
 * Color definitions.
 */

cindex = 0;
rgb_values = list ();

sub define_color (name, r, g, b)
{
  rgb_values[cindex] = list (name, r, g, b);
  cindex = cindex + 1;
}

sub color_index (name)
{
  local i;

  for (i = 0; i < length (rgb_values); i = i + 1)
    if (strcmp (rgb_values[i][0], name) == 0)
      return i;

  return -1;
}

sub language_color (name)
{
  local idx;

  idx = color_index (name);
  if (idx < 0)
    panic ("unknown color `", name, "'");

  /*
   * The map_color() subroutine is language specific and defined in 
   * *_faces() subroutine.
   */
  map_color (rgb_values[idx][1], rgb_values[idx][2], rgb_values[idx][3]);
}

/* RGB definitions for colors.  These are borrowed from X's rgb.txt file. */

define_color ("black",          0, 0, 0);
define_color ("firebrick",      178, 34, 34);
define_color ("purple",         160, 32, 240);
define_color ("forest green",   34, 139, 34);
define_color ("rosy brown",     188, 143, 143);
define_color ("blue",           0, 0, 255);


/*
 * General helpers.
 */

sub debug (msg)
{
  if (debug_level)
    print ("DEBUG: ", msg, "\n");
}

sub is_prefix (prefix, string)
{
  return strncmp (prefix, string, length (prefix)) == 0;
}

sub strchr (string, ch)
{
  local len = length (string), i;

  for (i = 0; i < len; i = i + 1)
    if (string[i] == ch)
      return i;

  return -1;
}


/*
 * Output face definitions for different target languages.
 */

sub enscript_faces ()
{
  /* RGB -> PostScript color mapper function. */
  sub map_color (r, g, b)
    {
      return sprintf ("%f %f %f", r div 255.0, g div 255.0, b div 255.0);
    }

  /* No language special characters. */
  LANGUAGE_SPECIALS = 0;

  sub language_print (str)
    {
      print (str);
    }

  sub header ()
    {
      /* Nothing here. */
    }

  sub trailer ()
    {
      /* Nothing here. */
    }

  sub font (name)
    {
      print ("\0font{", name, "@", ptsize, "}");
    }

  sub color (name)
    {
      print ("\0color{", name, "}");
    }

  sub default () 
    {
      print ("\0font{default}");
      if (color)
        print ("\0color{default}");
    }

  sub bold (on)
    {
      if (on)
        {
          font (bold_font);
          if (color)
            color (bold_color);
        }
      else
        default ();
    }

  sub italic (on)
    {
      if (on)
        {
          font (italic_font);
          if (color)
            color (italic_color);
        }
      else
        default ();
    }

  sub bold_italic (on)
    {
      if (on)
        {
          font (bold_italic_font);
          if (color)
            color (bold_italic_color);
        }
      else
        default ();
    }

  sub comment_face (on)
    {
      if (on)
        {
          font (comment_face_font);
          if (color)
            color (comment_face_color);
        }
      else
        default ();
    }

  sub function_name_face (on)
    {
      if (on)
        {
          font (function_name_face_font);
          if (color)
            color (function_name_face_color);
        }
      else
        default ();
    }

  sub keyword_face (on)
    {
      if (on)
        {
          font (keyword_face_font);
          if (color)
            color (keyword_face_color);
        }
      else
        default ();
    }

  sub reference_face (on)
    {
      if (on)
        {
          font (reference_face_font);
          if (color)
            color (reference_face_color);
        }
      else
        default ();
    }

  sub string_face (on)
    {
      if (on)
        {
          font (string_face_font);
          if (color)
            color (string_face_color);
        }
      else
        default ();
    }
}

sub html_faces ()
{
  /* RGB -> HTML color mapper function. */
  sub map_color (r, g, b)
    {
      return sprintf ("#%02X%02X%02X", r, g, b);
    }

  LANGUAGE_SPECIALS = /[<>\&\"]/;

  sub language_print (str)
    {
      str = regsuball (str, /\&/, "&amp;");
      str = regsuball (str, /</, "&lt;");
      str = regsuball (str, />/, "&gt;");
      str = regsuball (str, /\"/, "&quot;");
      print (str);
    }

  sub header ()
    {
      print ("<html>
<head><title>", filename, "</title></head>
",
             color ? "<body bgcolor=\"#FFFFFF\">" : "",
             "<h1>", filename, "</h1>
<hr>
<pre>\n");
    }
  
  sub trailer ()
    {
      print ("</pre>
<hr>
<address>By ", version, "</address>
</body>
</html>\n");
    }

  sub color_on (name)
    {
      print ("<font color=\"", name, "\">");
    }

  sub color_off ()
    {
      print ("</font>");
    }

  sub bold (on)
    {
      if (on)
        print ("<b>");
      else
        print ("</b>");
    }

  sub italic (on)
    {
      if (on)
        print ("<i>");
      else
        print ("</i>");
    }

  sub bold_italic (on)
    {
      if (on)
        print ("<b><i>");
      else
        print ("</i></b>");
    }

  sub comment_face (on)
    {
      if (on)
        {
          print ("<i>");
          if (color)
            color_on (comment_face_color);
        }
      else
        {
          if (color)
            color_off ();
          print ("</i>");
        }
    }

  sub function_name_face (on)
    {
      if (on)
        {
          print ("<b>");
          if (color)
            color_on (function_name_face_color);
        }
      else
        {
          if (color)
            color_off ();
          print ("</b>");
        }
    }

  sub keyword_face (on)
    {
      if (on)
        {
          print ("<b>");
          if (color)
            color_on (keyword_face_color);
        }
      else
        {
          if (color)
            color_off ();
          print ("</b>");
        }
    }

  sub reference_face (on)
    {
      if (on)
        {
          print ("<b>");
          if (color)
            color_on (reference_face_color);
        }
      else
        {
          if (color)
            color_off ();
          print ("</b>");
        }
    }

  sub string_face (on)
    {
      if (on)
        {
          if (color)
            color_on (string_face_color);
          print ("<b>");
        }
      else
        {
          if (color)
            color_off ();
          print ("</b>");
        }
    }
}


sub texinfo_faces ()
{
  /* Nop since Texinfo doesn't have colors. */
  sub map_color (r, g, b)
    {
      return "";
    }

  LANGUAGE_SPECIALS = /[{}@]/;
  
  sub language_print (str)
    {
      str = regsuball (str, /@/, "@@");
      str = regsuball (str, /{/, "@{");
      str = regsuball (str, /}/, "@}");
      print (str);
    }

  sub header ()
    {
      print ("@example\n");
    }

  sub trailer ()
    {
      print ("@end example\n");
    }

  sub bold (on)
    {
      if (on)
        print ("@strong{");
      else
        print ("}");
    }

  sub italic (on)
    {
      if (on)
        print ("@emph{");
      else
        print ("}");
    }

  sub bold_italic (on)
    {
    }

  sub comment_face (on)
    {
      italic (on);
    }

  sub function_name_face (on)
    {
      bold (on);
    }

  sub keyword_face (on)
    {
      bold (on);
    }

  sub reference_face (on)
    {
      bold (on);
    }

  sub string_face (on)
    {
    }
}


sub define_faces ()
{
  if (strcmp (language, "enscript") == 0) 
    enscript_faces ();
  else if (strcmp (language, "html") == 0)
    html_faces ();
  else if (strcmp (language, "texinfo") == 0)
    texinfo_faces ();
  else
    panic ("unknown output language `", language, "'");
}


/*
 * Initializations.
 */

start 
{
  /* Set debug level. */
  debug_level = int (debug);

  /* Resolve fonts. */
  idx = strchr (font_spec, '@');
  if (idx < 0)
    panic ("malformed font spec: `", font_spec, "'");

  font = substring (font_spec, 0, idx);
  ptsize = substring (font_spec, idx + 1, length (font_spec));

  debug (concat ("start: ", font, "@", ptsize));

  /* Construct bold, italic, etc. fonts for our current body font. */
  if (is_prefix ("Times", font))
    {
      bold_font = "Times-Bold";
      italic_font = "Times-Italic";
      bold_italic_font = "Times-BoldItalic";
    }
  else
    {
      bold_font = concat (font, "-Bold");
      italic_font = concat (font, "-Oblique");
      bold_italic_font = concat (font, "-BoldOblique");
    }

  /* Select highlight level. */
  if (strcmp (hl_level, "none") == 0)
    {
      comment_face_font =       font;
      function_name_face_font = font;
      keyword_face_font =       font;
      reference_face_font =     font;
      string_face_font =        font;
    }
  else if (strcmp (hl_level, "light") == 0)
    {
      comment_face_font =       italic_font;
      function_name_face_font = bold_font;
      keyword_face_font =       bold_font;
      reference_face_font =     bold_font;
      string_face_font =        font;
    }
  else if (strcmp (hl_level, "heavy") == 0)
    {
      comment_face_font =       italic_font;
      function_name_face_font = bold_font;
      keyword_face_font =       bold_font;
      reference_face_font =     bold_font;
      string_face_font =        bold_font;
    }
  else
    panic ("unknown highlight level `", hl_level, "'");

  /* Define output faces. */
  define_faces ();

  /* Select colormodel. */
  if (strcmp (colormodel, "blackwhite") == 0)
    {
      color = 0;
    }
  else if (strcmp (colormodel, "emacs") == 0)
    {
      color = 1;
      bold_color =                      language_color ("black");
      italic_color =                    language_color ("black");
      bold_italic_color =               language_color ("black");
      comment_face_color =              language_color ("firebrick");
      function_name_face_color =        language_color ("blue");
      keyword_face_color =              language_color ("purple");
      reference_face_color =            language_color ("forest green");
      string_face_color =               language_color ("rosy brown");
    }
  else
    panic ("unknown color model `", colormodel, "'");

  /* Resolve start state. */
  if (check_startrules ())
    debug ("startstate from startrules");
  if (check_namerules ())
    debug ("startstate from namerules");
}

namerules
{
  /\.(c|h)$/                    c;
  /\.(c++|C|H|cpp|cc|cxx)$/     cpp;
  /\.m$/                        objc;
  /\.(scm|scheme)$/             scheme;
  /\.st$/                       states;
  /(M|m)akefile.*/              makefile;
  /\.tcl$/                      tcl;
  /\.(v|vh)$/                   verilog;
  /\.html?$/                    html;
  /\bChangeLog$/                changelog;
  /\.(vhd|vhdl)$/               vhdl;
  /\.(scr|.syn|.synth)$/        synopsys;
}

startrules
{
  /.\010.\010.\010./                    nroff;
  /-\*- [Cc] -\*-/                      c;
  /-\*- [Cc]\+\+ -\*-/                  cpp;
  /-\*- [Oo][Bb][Jj][Cc] -\*-/          objc;
  /-\*- [Ss][Cc][Hh][Ee][Mm][Ee] -\*-/  scheme;
  /-\*- [Tt][Cc][Ll] -\*-/              tcl;
  /-\*- [Vv][Hh][Dd][Ll] -\*-/          vhdl;
  /^From:/                              mail;
}


/*
 * Helper subroutines and states.
 */

state eat_one_line
{
  /\n/ {
    language_print ($0);
    return;
  }
}


/*
 * Describe all known highlight languages.
 */

state describe_me
{
  / \*$/ {
  }
  / \* (.*)/ {
    print ($1);
  }

  /\*\\\/.*/ {
    /* All done. */
    return;
  }
}

state describe_languages
{
  BEGIN {
    print ("Highlighting is supported for the following languages and file formats:\n");
  }
  END {
    print ("\n");
  }
  /^\/\*\*.*$/ {
    call (describe_me);
  }
  /[^\\\/]+/ {
    /* NOP */
  }
  /./ {
    /* NOP */
  }
}


/*
 * Language specific states
 */

/**
 * Name: c
 * Description: C programming language.
 */

state c_comment
{
  /\*\\\// {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
   language_print ($0);
  }
}

state c_string
{
  /\\\\./ {
    language_print ($0);
  }
  /\"/ {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state c_ppline
{
  /\/\*/ {
    /* Comment within a pre-processor line. */
    reference_face (false);
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
    reference_face (true);
  }
  /\n/ {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state c
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Pre-processor lines. */
  /^#/ {
    reference_face (true);
    language_print ($0);
    call (c_ppline);
    reference_face (false);
  }

  /* Character constants. */
  /'.'|'\\\\.'/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Keywords.
     (build-re '(auto break case char const continue default do double else 
     enum extern float for goto if int long register return short signed 
     sizeof static struct switch typedef union unsigned void volatile while))
   */
  /\b(auto|break|c(ase|har|on(st|tinue))|d(efault|o(|uble))\
|e(lse|num|xtern)|f(loat|or)|goto|i(f|nt)|long|re(gister|turn)\
|s(hort|i(gned|zeof)|t(atic|ruct)|witch)|typedef|un(ion|signed)\
|vo(id|latile)|while)\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: cpp
 * Description: C++ programming language.
 */

state cpp
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
  }
  /\/\// {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Pre-processor lines. */
  /^#/ {
    reference_face (true);
    language_print ($0);
    call (c_ppline);
    reference_face (false);
  }

  /* Character constants. */
  /'.'|'\\\\.'/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Keywords.
    (build-re '(asm auto bool break case catch char class compless const 
    continue default delete do double else enum extern float for friend goto 
    if inline int long new operator overload private protected public
    register return short signed sizeof static struct switch this 
    template throw try typedef union unsigned virtual void volatile
    while))
  */
  /\b(a(sm|uto)|b(ool|reak)|c(a(se|tch)|har|lass|o(mpless|n(st|tinue)))\
|d(e(fault|lete)|o(|uble))|e(lse|num|xtern)|f(loat|or|riend)|goto\
|i(f|n(line|t))|long|new|o(perator|verload)|p(r(ivate|otected)|ublic)\
|re(gister|turn)|s(hort|i(gned|zeof)|t(atic|ruct)|witch)\
|t(emplate|h(is|row)|ry|ypedef)|un(ion|signed)|v(irtual|o(id|latile))\
|while)\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: objc
 * Description: Objective-C programming language.
 */

state objc
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }
  
  /* Comments. */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
  }
  /\/\// {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Pre-processor lines. */
  /^#/ {
    reference_face (true);
    language_print ($0);
    call (c_ppline);
    reference_face (false);
  }

  /* Compiler directives. */
  /^@/ {
    reference_face (true);
    language_print ($0);
    call (c_ppline);
    reference_face (false);
  }

  /* Character constants. */
  /'.'|'\\\\.'/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Keywords.  Basicly C + some extras.
     (build-re '(auto break case char const continue default do double else 
     enum extern float for goto if int long register return short signed 
     sizeof static struct switch typedef union unsigned void volatile while
     self super _cmd id Class SEL IMP BOOL YES NO nil Nil
     ))
   */
  /\b(BOOL|Class|IMP|N(O|il)|SEL|YES|_cmd|auto|break\
|c(ase|har|on(st|tinue))|d(efault|o(|uble))|e(lse|num|xtern)\
|f(loat|or)|goto|i(d|f|nt)|long|nil|re(gister|turn)\
|s(elf|hort|i(gned|zeof)|t(atic|ruct)|uper|witch)|typedef\
|un(ion|signed)|vo(id|latile)|while)\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: scheme
 * Description: Scheme programming language.
 */

state scheme
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /;/ {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Keywords. 
     (build-re '(else => define unquote unquote-splicing quote lambda
     if set! begin cond and or case let let* letrec do delay quasiquote))
   */
  /\b(=>|and|begin|c(ase|ond)|d(e(fine|lay)|o)|else|if|l(ambda|et(|*|rec))\
|or|qu(asiquote|ote)|set!|unquote(|-splicing))\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: states
 * Description: States program's definition files.
 */

state states_regexp
{
  /\\\\./ {
    language_print ($0);
  }
  /\// {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state states
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Regular expressions. */
  /\\\// {
    string_face (true);
    language_print ($0);
    call (states_regexp);
    string_face (false);
  }

  /* Keywords. 
     (build-re '(BEGIN END div else if local namerules return start startrules 
     state sub while))
   */
  /\b(BEGIN|END|div|else|if|local|namerules|return|s(ta(rt(|rules)|te)|ub)\
|while)\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: makefile
 * Description: Make program's definition files.
 */

state sh_eat_to_apostrophe
{
  /\'/ {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state sh_eat_to_grave
{
  /`/ {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state makefile
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /#/ {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }
  /* An escapec double quote, this doesn't start a string constant. */
  /\\\"/ {
    language_print ($0);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Shell apostrophe quote. */
  /\'/ {
    string_face (true);
    language_print ($0);
    call (sh_eat_to_apostrophe);
    string_face (false);
  }

  /* Shell grave quote. */
  /`/ {
    string_face (true);
    language_print ($0);
    call (sh_eat_to_grave);
    string_face (false);
  }

  /* Variable references. */
  /(\$[\(\{])([^\)\}]+)([\)\}])/ {
    language_print ($1);
    reference_face (true);
    language_print ($2);
    reference_face (false);
    language_print ($3);
  }

  /* Targets. */
  /^[^ \t\r\n]+:/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: tcl
 * Description: Tcl programming language.
 */

state tcl_comment
{
  /[^\\\\]\n/ {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state tcl
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /#/ {
    comment_face (true);
    language_print ($0);
    call (tcl_comment);
    comment_face (false);
  }
  /#\n/ {
    comment_face (true);
    language_print ($0);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Keywords.
     (build-re '(append array break case catch cd close concat continue 
     default elseif else eof error eval exec exit expr file flush for
     foreach format gets glob global history if incr info join lappend
     library lindex linsert list llength lrange lreplace lsearch lsort open
     pid proc puts pwd read regexp regsub rename return scan seek set
     source split string switch tell then time trace unknown unset uplevel
     upvar while))
   */
  /\b(a(ppend|rray)|break|c(a(se|tch)|d|lose|on(cat|tinue))|default\
|e(lse(|if)|of|rror|val|x(ec|it|pr))|f(ile|lush|or(|each|mat))\
|g(ets|lob(|al))|history|i(f|n(cr|fo))|join\
|l(append|i(brary|n(dex|sert)|st)|length|r(ange|eplace)|s(earch|ort))\
|open|p(id|roc|uts|wd)|re(ad|g(exp|sub)|name|turn)\
|s(can|e(ek|t)|ource|plit|tring|witch)|t(ell|hen|ime|race)\
|u(n(known|set)|p(level|var))|while)\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: verilog
 * Description: Verilog hardware description language
 *
 * Send love letters or hate mail to eda@ultranet.com
 */
state verilog
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Verilog takes C++ style comments */

  /* Comments. */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
  }
  /\/\// {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Macro expansions start with '`' and continue one word. */
  /`([a-zA-Z_0-9]+)/ {
    reference_face (true);
    language_print ($0);
    reference_face (false);
  }

  /* Keywords.
     (build-re '(always and assign begin buf bufif0 bufif1 case casex 
     casez cmos deassign default defparam disable edge else end endcase 
     endmodule endfunction endprimitive endspecify endtable endtask event 
     for force forever fork function highz0 highz1 if initial inout input 
     integer join large macromodule medium module nand negedge nmos nor 
     not notif0 notif1 or output parameter pmos posedge primitive pull0 
     pull1 pullup pulldown rcmos reg release repeat rnmos rpmos rtran 
     rtranif0 rtranif1 scalared small specify specparam strength strong0 
     strong1 supply0 supply1 table task time tran tranif0 tranif1 tri tri0 
     tri1 triand trior trireg vectored wait wand weak0 weak1 while wire wor 
     xnor xor 
     $bitstoreal $countdrivers $display $fclose $fdisplay $fmonitor 
     $fopen $fstrobe $fwrite $finish $getpattern $history $incsave $input
     $itor $key $list $log $monitor $monitoroff $monitoron $nokey $nolog
     $printtimescale $readmemb $readmemh $realtime $realtobits $reset 
     $reset_count $reset_value $restart $rtoi $save $scale $scope 
     $showscopes $showvariables $showvars $sreadmemb $sreadmemh $stime 
     $stop $strobe $time $timeformat $write $vcdpluson $vcdplusoff 
     $vcdplustraceon $vcdplustraceoff $dumpvars
     ;; prefix G stands for grave `
     Gaccelerate Gautoexpand_vectornets Gcelldefine Gdefault_nettype Gdefine 
     Gelse Gendcelldefine Gendif Gendprotect Gendprotected
     Gexpand_vectornets Gifdef Ginclude Gnoaccelerate
     Gnoexpand_vectornets Gnoremove_gatenames Gnoremove_netnames
     Gnounconnected_drive Gprotect Gprotected Gremove_gatenames
     Gremove_netnames Gresetall Gtimescale Gunconnected_drive
     Guselib
     ))
   */
  /\$(bitstoreal|countdrivers|d(isplay|umpvars)\
|f(close|display|inish|monitor|open|strobe|write)|getpattern|history\
|i(n(csave|put)|tor)|key|l(ist|og)|monitor(|o(ff|n))|no(key|log)\
|printtimescale\
|r(e(a(dmem(b|h)|lt(ime|obits))|s(et(|_(count|value))|tart))|toi)\
|s(ave|c(ale|ope)|how(scopes|var(iables|s))|readmem(b|h)|t(ime|op|robe))\
|time(|format)|vcdplus(o(ff|n)|traceo(ff|n))|write)\b\
|`(a(ccelerate|utoexpand_vectornets)|celldefine|def(ault_nettype|ine)\
|e(lse|nd(celldefine|if|protect(|ed))|xpand_vectornets)|i(fdef|nclude)\
|no(accelerate|expand_vectornets|remove_(gatenames|netnames)\
|unconnected_drive)\
|protect(|ed)|re(move_(gatenames|netnames)|setall)|timescale\
|u(nconnected_drive|selib))\b\
|\b(a(lways|nd|ssign)|b(egin|uf(|if(0|1)))|c(ase(|x|z)|mos)\
|d(e(assign|f(ault|param))|isable)\
|e(dge|lse|nd(|case|function|module|primitive|specify|ta(ble|sk))|vent)\
|f(or(|ce|ever|k)|unction)|highz(0|1)|i(f|n(itial|out|put|teger))|join\
|large|m(acromodule|edium|odule)|n(and|egedge|mos|o(r|t(|if(0|1))))\
|o(r|utput)|p(arameter|mos|osedge|rimitive|ull(0|1|down|up))\
|r(cmos|e(g|lease|peat)|nmos|pmos|tran(|if(0|1)))\
|s(calared|mall|pec(ify|param)|tr(ength|ong(0|1))|upply(0|1))\
|t(a(ble|sk)|ime|r(an(|if(0|1))|i(|0|1|and|or|reg)))|vectored\
|w(a(it|nd)|eak(0|1)|hile|ire|or)|x(nor|or))\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: mail
 * Description: Mail and news articles.
 */

state mail_body
{
  BEGIN {
    reference_face (false);
  }
  /^[ \t]+>/ {
    reference_face (true);
    language_print ($0);
    call (eat_one_line);
    reference_face (false);
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state mail
{
  BEGIN {
    header ();
    reference_face (true);
  }
  END {
    trailer ();
  }

  /^[ \t]*$/ {
    /* Move to the mail body. */
    call (mail_body);
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: nroff
 * Description: Manual pages formatted with the nroff program.
 */

state nroff_italic
{
  BEGIN {
  }
  /_\010(.)/ {
    language_print ($1);
  }
  /([^_])\010.\010.\010./ {
    bold (true);
    language_print ($1);
    call (nroff_bold);
    bold (false);
    italic (true);
  }
  /.|\n/ {
    italic (false);
    language_print ($0);
    return;
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state nroff_bold
{
  /([^_])\010.\010.\010./ {
    language_print ($1);
  }
  /_\010(.)/ {
    italic (true);
    language_print ($1);
    call (nroff_italic);
    italic (false);
    bold (true);
  }
  /.|\n/ {
    bold (false);
    language_print ($0);
    return;
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state nroff
{
  BEGIN {
    header ();
  }

  END {
    trailer ();
  }

  /_\010(.)/ {
    italic (true);
    language_print ($1);
    call (nroff_italic);
  }
  /([^_])\010.\010.\010./ {
    bold (true);
    language_print ($1);
    call (nroff_bold);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: html
 * Description: Hypertext markup language HTML.
 */

state html_tag
{
  />/ {
    language_print ($0);
    return;
  }
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
    keyword_face (true);
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state html_entity
{
  /;/ {
    language_print ($0);
    return;
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}

state html
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /</ {
    keyword_face (true);
    language_print ($0);
    call (html_tag);
    keyword_face (false);
  }
  /&/ {
    keyword_face (true);
    language_print ($0);
    call (html_entity);
    keyword_face (false);
  }
  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: changelog
 * Description: ChangeLog files.
 */

state changelog
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Date entries. */
  /^[a-zA-Z0-9]+/ {
    function_name_face (true);
    language_print ($0);
    call (eat_one_line);
    function_name_face (false);
  }

  /* File descriptions with function names. */
  /(^\t\* )([^\(]+)(\()([^\)]+)(\):)/ {
    print ($1);
    string_face (true);
    print ($2);
    string_face (false);
    print ($3);
    keyword_face (true);
    print ($4);
    keyword_face (false);
    print ($5);
  }

  /* File descriptions without functions names. */
  /(^\t\* )([^ :]+)(:)/ {
    print ($1);
    string_face (true);
    print ($2);
    string_face (false);
    print ($3);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
}


/**
 * Name: vhdl
 * Description: VHSIC Hardware Description Language (VHDL)
 *              Highlights keywords, comments and special vhdl 
 *              constructs.  Please send comments or suggestions.
 *
 * By: Brian Silveira (brian@nortel.ca)
 *
 */
state vhdl
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /* Comments. */
  /--/ {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Keywords.
     abs access after alias all and architecture array
     assert attribute begin begin block body body buffer bus case
     component configuration configuration constant disconnect downto
     else elsif end entity exit file for function function generate
     generic guarded if in inout is label library linkage loop map
     mod nand new next nor not null of on open or others out package
     port procedure process range record register rem report return
     reverse select severity signal subtype then to transport type
     units until use variable wait when while with with xor
     
     NOTE In VHDL all keywords are case insensitive...and hence
     the following regular expression.
   */
  /\b([Aa]([Bb][Ss]|[Cc][Cc][Ee][Ss][Ss]|[Ff][Tt][Ee][Rr]|[Ll][Ii][Aa][Ss]\
|[Ll][Ll]|[Nn][Dd]|[Rr][Cc][Hh][Ii][Tt][Ee][Cc][Tt][Uu][Rr][Ee]\
|[Rr][Rr][Aa][Yy]|[Ss][Ss][Ee][Rr][Tt]|[Tt][Tt][Rr][Ii][Bb][Uu][Tt][Ee])\
|[Bb]([Ee][Gg][Ii][Nn]|[Ll][Oo][Cc][Kk]|[Oo][Dd][Yy]\
|[Uu][Ff][Ff][Ee][Rr]|[Uu][Ss])\
|[Cc]([Aa][Ss][Ee]|[Oo][Mm][Pp][Oo][Nn][Ee][Nn][Tt]\
|[Oo][Nn][Ff][Ii][Gg][Uu][Rr][Aa][Tt][Ii][Oo][Nn]\
|[Oo][Nn][Ss][Tt][Aa][Nn][Tt])\
|[Dd]([Ii][Ss][Cc][Oo][Nn][Nn][Ee][Cc][Tt]|[Oo][Ww][Nn][Tt][Oo])\
|[Ee]([Ll][Ss][Ee]|[Ll][Ss][Ii][Ff]|[Nn][Dd]|[Nn][Tt][Ii][Tt][Yy]\
|[Xx][Ii][Tt])\
|[Ff]([Ii][Ll][Ee]|[Oo][Rr]|[Uu][Nn][Cc][Tt][Ii][Oo][Nn])\
|[Gg]([Ee][Nn][Ee][Rr][Aa][Tt][Ee]|[Ee][Nn][Ee][Rr][Ii][Cc]\
|[Uu][Aa][Rr][Dd][Ee][Dd])\
|[Ii]([Ff]|[Nn]|[Nn][Oo][Uu][Tt]|[Ss])\
|[Ll]([Aa][Bb][Ee][Ll]|[Ii][Bb][Rr][Aa][Rr][Yy]|[Ii][Nn][Kk][Aa][Gg][Ee]\
|[Oo][Oo][Pp])\
|[Mm]([Aa][Pp]|[Oo][Dd])\
|[Nn]([Aa][Nn][Dd]|[Ee][Ww]|[Ee][Xx][Tt]|[Oo][Rr]|[Tt][Oo]|[Uu][Ll][Ll])\
|[Oo]([Ff]|[Nn]|[Pp][Ee][Nn]|[Rr]|[Tt][Hh][Ee][Rr][Ss]|[Uu][Tt])\
|[Pp]([Aa][Cc][Kk][Aa][Gg][Ee]|[Oo][Rr][Tt]\
|[Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]|[Rr][Aa][Gg][Mm][Aa]\
|[Rr][Oo][Cc][Ee][Ss][Ss])\
|[Rr]([Aa][Nn][Gg][Ee]|[Ee][Cc][Oo][Rr][Dd]|[Ee][Gg][Ii][Ss][Tt][Ee][Rr]\
|[Ee][Mm]|[Ee][Pp][Oo][Rr][Tt]|[Ee][Tt][Uu][Rr][Nn]\
|[Ee][Vv][Ee][Rr][Ss][Ee])\
|[Ss]([Ee][Ll][Ee][Cc][Tt]|[Ee][Vv][Ee][Rr][Ii][Tt][Yy]\
|[Ii][Gg][Nn][Aa][Ll]|[Uu][Bb][Tt][Yy][Pp][Ee])\
|[Tt]([Hh][Ee][Nn]|[Oo]|[Rr][Aa][Nn][Ss][Pp][Oo][Rr][Tt]|[Yy][Pp][Ee])\
|[Uu]([Nn][Ii][Tt][Ss]|[Nn][Tt][Ii][Ll]|[Ss][Ee])\
|[Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee]\
|[Ww]([Aa][Ii][Tt]|[Hh][Ee][Nn]|[Hh][Ii][Ll][Ee]\
|[Ii][Tt][Hh]|[Ii][Tt][Hh])|[Xx][Oo][Rr])\b/{
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
  

  /<=|=>/
        {
          reference_face (true);
          language_print ($0);
          reference_face(false);
        }
}


/**
 * Name: synopsys
 * Description: Synopsys dc shell scripting language
 *
 * By: Brian Silveira   (brian@nortel.ca)
 *     Hartley Horwitz (hars@nortel.ca)
 *
 */
state synopsys
{
  BEGIN {
    header ();
  }
  END {
    trailer ();
  }

  /*
   * Synopsys allows globing...so pick out constructs like
   * /foo/bar/* and just print them out.  i.e. don't treat the
   * ending like a comment!
   */
  /[A-z0-9_-]\/\*/{
    language_print($0);
  }


  /* Comments. 
   * Synopsys DC-shell uses C-style comments
   */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* I use make-regexp in emacs which uses a list of strings to 
     generate a  regular expression.
     (setq synopsys-keywords-enscript 
      '("alias" "all_connected" "analyze" "balance_buffer"
        "balance_registers" "break" "cd" "change_link" "change_names"
        "characterize" "check_design" "check_test" "check_timing"
        "continue" "compile" "copy_design" "create_clock" "drive_of"
        "echo" "elaborate" "else" "exit" "get_attribute" "get_license"
        "get_unix_variable" "group" "group_path" "include"
        "insert_scan" "insert_test" "link" "list_designs"
        "list_instances" "list_libs" "load_of" "quit" "read"
        "read_lib" "read_timing" "remove_attribute"
        "remove_constraint" "remove_design" "remove_input_delay"
        "remove_lib" "remove_clock" "remove_cell" "remove_license"
        "remove_output_delay" "remove_unconnected_ports"
        "rename_design" "reoptimize_design" "report_area"
        "report_attribute" "report_cell" "report_constraint"
        "report_design" "report_design_lib" "report_hierarchy"
        "report_internal_loads" "report_lib" "report_multicycles"
        "report_net" "report_port" "report_power" "report_reference"
        "report_resources" "report_test" "report_timing"
        "reset_design" "set_attribute" "set_boundary_optimization"
        "set_clock_skew" "set_dont_touch" "set_dont_touch_network"
        "set_dont_use" "set_drive" "set_driving_cell" "set_equal"
        "set_disable_timing" "set_false_path" "set_flatten"
        "set_implementation" "set_fix_hold" "set_input_delay"
        "set_load" "set_logic_one" "set_logic_zero" "set_max_area"
        "set_max_capacitance" "set_max_fanout" "set_max_transition"
        "set_multicycle_path" "set_operating_conditions"
        "set_output_delay" "set_scan_style" "set_signal_type"
        "set_structure" "set_test_methodology" "set_unconnected"
        "set_wire_load" "unalias" "sh" "ungroup" "uniquify"
        "update_lib" "which" "write" "write_constraints"
        "write_script" "write_timing" "if" "foreach" "find" "while"
        "-all" "-all_violators" "-allowed" "-attributes" "-base_name"
        "-boundary_optimization" "-cell" "-cell_name" "-clock"
        "-context" "-connections" "-constraints" "-delay" "-design"
        "-design_name" "-depth" "-drive" "-except" "-f" "-fall_delay"
        "-flat" "-format" "-from" "-hierarchy" "-hier" "-hold"
        "-incremental_mapping" "-ideal" "-ignored" "-in_place"
        "-logic" "-library" "-map_effort" "-mode" "-max_paths"
        "-max_scan_chain_length" "-no_disable" "-methodology" "-name"
        "-net" "-new_name" "-none" "-nosplit" "-nworst" "-output"
        "-path" "-parameters" "-period" "-pin_load" "-propagated"
        "-reference" "-rise_delay" "-rules" "-skew" "-setup"
        "-through" "-to" "-type" "-uncertainty" "-plus_uncertainty"
        "-minus_uncertainty" "-update" "-verify" "-verbose"
        "-waveform" "-wire_load" "-work" "-weight" "-worst"
        "actel_qbar_opto" "actel_seq_opto" "auto_link_disable"
        "auto_link_options" "auto_wire_load_selection"
        "bc_enable_chaining" "bc_enable_multi_cycle"
        "bc_enable_speculative_execution" "bc_fsm_coding_style"
        "bc_time_all_sequential_op_bindings" "bus_extraction_style"
        "bus_inference_style" "bus_naming_style"
        "change_names_dont_change_bus_members"
        "change_names_update_inst_tree" "command_log_file" "company"
        "compatibility_version"
        "compile_assume_fully_decoded_three_state_busses"
        "compile_create_mux_op_hierarchy"
        "compile_default_critical_range"
        "compile_disable_area_opt_during_inplace_opt"
        "compile_disable_hierarchical_inverter_opt"
        "compile_dont_touch_annotated_cell_during_inplace_opt"
        "compile_fix_multiple_port_nets"
        "compile_ignore_area_during_inplace_opt"
        "compile_ignore_footprint_during_inplace_opt"
        "compile_implementation_selection"
        "compile_inplace_changed_list_file_name"
        "compile_instance_name_prefix" "compile_instance_name_suffix"
        "compile_mux_no_boundary_optimization"
        "compile_negative_logic_methodology"
        "compile_no_new_cells_at_top_level"
        "compile_ok_to_buffer_during_inplace_opt"
        "compile_preserve_subdesign_interfaces"
        "compile_preserve_sync_resets"
        "compile_update_annotated_delays_during_inplace_opt"
        "compile_use_fast_delay_mode" "compile_use_low_timing_effort"
        "context_check_status" "current_design" "current_instance"
        "dc_shell_status" "default_name_rules" "design_library_file"
        "designer" "duplicate_ports" "echo_include_commands"
        "enable_page_mode" "exit_delete_filename_log_file"
        "filename_log_file" "find_converts_name_lists"
        "find_ignore_case" "hdl_keep_licenses" "hdl_naming_threshold"
        "hdl_preferred_license" "hdl_variables"
        "hdlin_advisor_directory" "hdlin_auto_save_templates"
        "hdlin_check_no_latch"
        "hdlin_dont_infer_mux_for_resource_sharing"
        "hdlin_enable_advisor" "hdlin_ff_always_async_set_reset"
        "hdlin_ff_always_sync_set_reset" "hdlin_files"
        "hdlin_hide_resource_line_numbers" "hdlin_infer_mux"
        "hdlin_keep_feedback" "hdlin_keep_inv_feedback"
        "hdlin_mux_size_limit" "hdlin_reg_report_length"
        "hdlin_replace_synthetic" "hdlin_report_inferred_modules"
        "hdlin_resource_allocation" "hdlin_resource_implementation"
        "hdlin_source_to_gates_mode" "hdlin_sync_set_reset"
        "hdlin_synch_set_reset" "hdlin_translate_off_skip_text"
        "link_force_case" "link_library"
        "port_complement_naming_style"
        "reoptimize_design_changed_list_file_name"
        "sdfin_fall_cell_delay_type" "sdfin_fall_net_delay_type"
        "sdfin_min_fall_cell_delay" "sdfin_min_fall_net_delay"
        "sdfin_min_rise_cell_delay" "sdfin_min_rise_net_delay"
        "sdfin_rise_cell_delay_type" "sdfin_rise_net_delay_type"
        "sdfin_top_instance_name"
        "sdfout_allow_non_positive_constraints"
        "sdfout_min_fall_cell_delay" "sdfout_min_fall_net_delay"
        "sdfout_min_rise_cell_delay" "sdfout_min_rise_net_delay"
        "sdfout_time_scale" "sdfout_top_instance_name"
        "sdfout_write_to_output" "search_path" "shell_prompt"
        "suppress_errors" "synlib_dont_get_license"
        "syntax_check_status" "synthetic_library" "target_library"
        "uniquify_naming_style" "verbose_messages"))
        */

  /\ba(l(ias|l_connected)|nalyze|uto_(link_(disable|options)\
|wire_load_selection))|b(alance_(buffer|registers)\
|c_(enable_(chaining|multi_cycle|speculative_execution)\
|fsm_coding_style|time_all_sequential_op_bindings)|reak\
|us_(extraction_style|inference_style|naming_style))\
|c(d|h(a(nge_(link|names(|_(dont_change_bus_members|update_inst_tree)))\
|racterize)|eck_(design|t(est|iming)))|o(m(mand_log_file|p(a(ny\
|tibility_version)|ile(|_(assume_fully_decoded_three_state_busses\
|create_mux_op_hierarchy|d(efault_critical_range\
|isable_(area_opt_during_inplace_opt|hierarchical_inverter_opt)\
|ont_touch_annotated_cell_during_inplace_opt)|fix_multiple_port_nets\
|i(gnore_(area_during_inplace_opt|footprint_during_inplace_opt)\
|mplementation_selection|n(place_changed_list_file_name\
|stance_name_(prefix|suffix)))|mux_no_boundary_optimization\
|n(egative_logic_methodology|o_new_cells_at_top_level)\
|ok_to_buffer_during_inplace_opt|preserve_s(ubdesign_interfaces\
|ync_resets)|u(pdate_annotated_delays_during_inplace_opt\
|se_(fast_delay_mode|low_timing_effort))))))|nt(ext_check_status|inue)\
|py_design)|reate_clock|urrent_(design|instance))|d(c_shell_status\
|e(fault_name_rules|sign(_library_file|er))|rive_of|uplicate_ports)|e(cho(\
|_include_commands)|l(aborate|se)|nable_page_mode|xit(\
|_delete_filename_log_file))|f(i(lename_log_file|nd(|_(converts_name_lists\
|ignore_case)))|oreach)|g(et_(attribute|license|unix_variable)|roup(\
|_path))|hdl(_(keep_licenses|naming_threshold|preferred_license|variables)\
|in_(a(dvisor_directory|uto_save_templates)|check_no_latch\
|dont_infer_mux_for_resource_sharing|enable_advisor\
|f(f_always_(async_set_reset|sync_set_reset)|iles)\
|hide_resource_line_numbers|infer_mux|keep_(feedback|inv_feedback)\
|mux_size_limit|re(g_report_length|p(lace_synthetic|ort_inferred_modules)\
|source_(allocation|implementation))|s(ource_to_gates_mode|ync(_set_reset\
|h_set_reset))|translate_off_skip_text))|i(f|n(clude|sert_(scan|test)))\
|l(i(nk(|_(force_case|library))|st_(designs|instances|libs))|oad_of)\
|port_complement_naming_style|quit|re(ad(|_(lib|timing))|move_(attribute\
|c(ell|lock|onstraint)|design|input_delay|li(b|cense)|output_delay\
|unconnected_ports)|name_design|optimize_design(|_changed_list_file_name)\
|port_(a(rea|ttribute)|c(ell|onstraint)|design(|_lib)|hierarchy\
|internal_loads|lib|multicycles|net|po(rt|wer)|re(ference|sources)|t(est\
|iming))|set_design)|s(df(in_(fall_(cell_delay_type|net_delay_type)\
|min_(fall_(cell_delay|net_delay)|rise_(cell_delay|net_delay))\
|rise_(cell_delay_type|net_delay_type)|top_instance_name)\
|out_(allow_non_positive_constraints|min_(fall_(cell_delay|net_delay)\
|rise_(cell_delay|net_delay))|t(ime_scale|op_instance_name)\
|write_to_output))|e(arch_path|t_(attribute|boundary_optimization\
|clock_skew|d(isable_timing|ont_(touch(|_network)|use)|riv(e|ing_cell))\
|equal|f(alse_path|ix_hold|latten)|i(mplementation|nput_delay)|lo(ad\
|gic_(one|zero))|m(ax_(area|capacitance|fanout|transition)|ulticycle_path)\
|o(perating_conditions|utput_delay)|s(can_style|ignal_type|tructure)\
|test_methodology|unconnected|wire_load))|h(|ell_prompt)|uppress_errors\
|yn(lib_dont_get_license|t(ax_check_status|hetic_library)))|target_library\
|u(n(alias|group|iquify(|_naming_style))|pdate_lib)|verbose_messages\
|w(hi(ch|le)|rite(|_(constraints|script|timing)))\b/{
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  LANGUAGE_SPECIALS {
    language_print ($0);
  }
  
}


/* 
Local variables:
mode: c
indent-tabs-mode: nil
End: 
*/
