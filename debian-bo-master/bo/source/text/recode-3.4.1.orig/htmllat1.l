/* Conversion of files between different charsets and usages.
   Copyright (C) 1990, 1993 Free Software Foundation, Inc.
   Francois Pinard <pinard@iro.umontreal.ca>, 1988.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Step name: html_latin1.  */

%%

&#[0-9][0-9]?[0-9]?;	{ if (diacritics_only)
			    ECHO;
			  else
			    {
			      int value = atoi (yytext + 2);

			      if (value >= 256)
				ECHO;
			      else
				output (value);
			    }
			}
&quot;			{ if (diacritics_only) ECHO; else output (34); }
&amp;			{ if (diacritics_only) ECHO; else output (38); }
&lt;			{ if (diacritics_only) ECHO; else output (60); }
&gt;			{ if (diacritics_only) ECHO; else output (62); }

&nbsp;			{ output (160); }
&iexcl;			{ output (161); }
&cent;			{ output (162); }
&pound;			{ output (163); }
&curren;		{ output (164); }
&yen;			{ output (165); }
&(brvbar|brkbar);	{ output (166); }
&sect;			{ output (167); }
&uml;			{ output (168); }
&copy;			{ output (169); }
&ordf;			{ output (170); }
&laquo;			{ output (171); }
&not;			{ output (172); }
&shy;			{ output (173); }
&reg;			{ output (174); }
&hibar;			{ output (175); }
&deg;			{ output (176); }
&plusmn;		{ output (177); }
&sup2;			{ output (178); }
&sup3;			{ output (179); }
&acute;			{ output (180); }
&micro;			{ output (181); }
&para;			{ output (182); }
&middot;		{ output (183); }
&cedil;			{ output (184); }
&sup1;			{ output (185); }
&ordm;			{ output (186); }
&raquo;			{ output (187); }
&frac14;		{ output (188); }
&frac12;		{ output (189); }
&frac34;		{ output (190); }
&iquest;		{ output (191); }
&Agrave;		{ output (192); }
&Aacute;		{ output (193); }
&Acircu?;		{ output (194); }
&Atilde;		{ output (195); }
&A(uml|diaer);		{ output (196); }
&Aring;			{ output (197); }
&AE(lig)?;		{ output (198); }
&Ccedil;		{ output (199); }
&Egrave;		{ output (200); }
&Eacute;		{ output (201); }
&Ecircu?;		{ output (202); }
&E(uml|diaer);		{ output (203); }
&Igrave;		{ output (204); }
&Iacute;		{ output (205); }
&Icircu?;		{ output (206); }
&I(uml|diaer);		{ output (207); }
&ETH;			{ output (208); }
&Ntilde;		{ output (209); }
&Ograve;		{ output (210); }
&Oacute;		{ output (211); }
&Ocircu?;		{ output (212); }
&Otilde;		{ output (213); }
&O(uml|diaer);		{ output (214); }
&(times|MULT);		{ output (215); }
&O(slash|stroke);	{ output (216); }
&Ugrave;		{ output (217); }
&Uacute;		{ output (218); }
&Ucircu?;		{ output (219); }
&U(uml|diaer);		{ output (220); }
&Yacute;		{ output (221); }
&THORN;			{ output (222); }
&(szlig|ssharp);	{ output (223); }
&agrave;		{ output (224); }
&aacute;		{ output (225); }
&acircu?;		{ output (226); }
&atilde;		{ output (227); }
&a(uml|diaer);		{ output (228); }
&aring;			{ output (229); }
&ae(lig)?;		{ output (230); }
&ccedil;		{ output (231); }
&egrave;		{ output (232); }
&eacute;		{ output (233); }
&ecircu?;		{ output (234); }
&e(uml|diaer);		{ output (235); }
&igrave;		{ output (236); }
&iacute;		{ output (237); }
&icircu?;		{ output (238); }
&i(uml|diaer);		{ output (239); }
&eth;			{ output (240); }
&ntilde;		{ output (241); }
&ograve;		{ output (242); }
&oacute;		{ output (243); }
&ocircu?;		{ output (244); }
&otilde;		{ output (245); }
&o(uml|diaer);		{ output (246); }
&(divide|DIVIS);	{ output (247); }
&o(slash|stroke);	{ output (248); }
&ugrave;		{ output (249); }
&uacute;		{ output (250); }
&ucircu?;		{ output (251); }
&u(uml|diaer);		{ output (252); }
&yacute;		{ output (253); }
&thorn;			{ output (254); }
&y(uml|diaer);		{ output (255); }

%%

void
module_html_latin1 (void)
{
  declare_alias ("HTML", "ISO_8879:1986");
  declare_step ("HTML", "latin1", MANY_TO_ONE, NULL, file_html_latin1);

  declare_alias ("entity", "HTML");
  declare_alias ("HTF", "HTML");
  declare_alias ("SGML", "HTML");
  declare_alias ("WWW", "HTML");
  declare_alias ("w3", "HTML");
}
