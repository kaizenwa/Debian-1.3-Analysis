/* ########################################################################
   
   SMAC FILE USED BY XCORAL EDITOR
   
   File: color.sc
   Path: /home/emery/Xcoral/color.sc
   Description: 
   Created: Mon Nov 21 17:43:37 MET 1994
   Author: Lionel Fournigault
   Modified: Fri Feb 10 13:46:05 MET 1995
   Last maintained by: Thierry Emery
   
   RCS $Revision$ $State$
   
   
   ########################################################################
   
   Note: First I would like to say that regexp for LaTeX, Perl, Ada
   Fortran, Shell-script and make, are derived from emacs19 hilight.el
   
   Requires: utilities.sc
   
   Defines: color_region, color_buffer.
   
   Suggested bindings: 
   
   Procedure: 
   
   ########################################################################
   
   Copyright (c) : Lionel Fournigault
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */
/*
 * ###########################################################################
 *                       Generics colors for all modes
 * ###########################################################################
 */

{
  if (! function("delete_chars"))
    load_file("utilities.sc");
}

char *gen_comment_color = "limegreen";
char *gen_include_color = "springgreen";
char *gen_define_color = "goldenrod";
char *gen_keyword_color = "gold";
char *gen_string_color = "lightgray";
char *gen_varfunc_color = "lightskyblue";
char *gen_decl_color = "palegreen";
char *gen_defun_color = "lightskyblue";

char *gen_string = "[^']\\(\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"\\)";

/*
 * ###########################################################################
 *                       C/C++ colors
 * ###########################################################################
 */
char *cpp_comment_color = gen_comment_color;
char *cpp_base_type_color = gen_decl_color;
char *cpp_class_color = "green";
char *cpp_delete_new_color = "red";
char *cpp_return_color = "lightsteelblue";
char *cpp_keyword_color = gen_keyword_color;
char *cpp_alloc_class_color = gen_keyword_color;
char *cpp_func_color = gen_varfunc_color;
char *cpp_define_color = gen_define_color;
char *cpp_ifdef_color = gen_define_color;
char *cpp_include_color = gen_include_color;
char *cpp_string_color = gen_string_color;

/*
 * ###########################################################################
 *                       LaTeX colors
 * ###########################################################################
 */
char *latex_comment_color = gen_comment_color;
char *latex_keyword_color = gen_keyword_color;
char *latex_defun_color = gen_decl_color;
char *latex_define_color = gen_define_color;
char *latex_decl_color = gen_decl_color;
char *latex_label_color = gen_keyword_color; 
char *latex_include_color = gen_include_color;
char *latex_italic_color = gen_decl_color;
char *latex_bold_color = gen_varfunc_color;
char *latex_ref_color = gen_comment_color;

/*
 * ###########################################################################
 *                       HTML colors
 * ###########################################################################
 */
char *html_title_color = gen_comment_color;
char *html_ibtt_color = gen_keyword_color;
char *html_pre_color = gen_define_color;
char *html_img_color = gen_decl_color;
char *html_ref_color = gen_varfunc_color;
char *html_list_color = gen_define_color;
char *html_forms_color = gen_include_color;
char *html_hds_color = gen_keyword_color;
char *hltml_string_color = gen_string_color;

/*
 * ###########################################################################
 *                       Edir (edit directory) colors
 * ###########################################################################
 */
char *edir_directory_color = gen_comment_color;
char *edir_link_color = "tan";
char *edir_c_file_color = gen_decl_color;
char *edir_tex_file_color = gen_decl_color;
char *edir_sc_file_color = gen_decl_color;
char *edir_h_file_color = "darkseagreen";
char *edir_o_file_color = "slategray";
char *edir_tilde_file_color = "plum";
char *edir_makefile_color = "plum";
char *edir_readme_color = "red";
  
/*
 * ###########################################################################
 *                       Fortran colors
 * ###########################################################################
 */
char *fortran_comment_color = gen_comment_color;
char *fortran_include_color = gen_include_color;
char *fortran_keyword_color = gen_keyword_color;
char *fortran_func_color = gen_varfunc_color;
char *fortran_decl_color = gen_decl_color;
char *fortran_string_color = gen_string_color;
char *fortran_type_color = gen_decl_color;

/*
 * ###########################################################################
 *                       Ada colors
 * ###########################################################################
 */
char *ada_comment_color = gen_comment_color;
char *ada_glob_struct_color = gen_varfunc_color;
char *ada_struct_color = gen_keyword_color;
char *ada_decl_color = gen_decl_color;
char *ada_include_color = gen_include_color;
char *ada_string_color = gen_string_color;

/*
 * ###########################################################################
 *                       Perl colors
 * ###########################################################################
 */
char *perl_comment_color = gen_comment_color;
char *perl_string_color = gen_string_color;
char *perl_label_color = gen_define_color;
char *perl_include_color = gen_include_color;
char *perl_decl_color = "lightskyblue";
char *perl_defun_color = "green";
char *perl_keyword_color = gen_keyword_color;

/*
 * ###########################################################################
 *                       Shell script colors
 * ###########################################################################
 */
char *shell_comment_color = gen_comment_color;
char *shell_string_color = gen_string_color;
char *shell_include_color = gen_include_color;
char *shell_define_color = gen_varfunc_color;
char *shell_var_color = gen_decl_color;
char *shell_keyword_color = gen_keyword_color;

/*
 * ###########################################################################
 *                 Makefile makefile and Imakefile colors
 * ###########################################################################
 */
char *make_comment_color = gen_comment_color;
char *make_rules_color = gen_varfunc_color;
char *make_define_color = gen_define_color;
char *make_keyword_color = gen_keyword_color;
char *make_defun_color = gen_defun_color;
char *make_include_color = gen_include_color;

/*
 * ###########################################################################
 *                       Internal color functions.
 * ###########################################################################
 */
/*
**	Function name : color_regexp_in_region
**
**	Description : Fonction de colorisation d'une regexp entre les positions
**         Smac start et end.
**	Input : La regexp, le numero de la re_substring, debut et fin de la
**         region et la couleur.  
**	Output :
*/
color_regexp_in_region ( char *regexp, int nsub, int start, int end, char *color)
{
    int orig = current_position();
    int pos, re_begin, re_end;

    goto_char(start);
    while (1) {
	pos = re_forward_search (regexp);
	if ((pos == -1) || (pos == -2) || (pos > end))
	  break;
	re_begin = re_match_beginning(nsub);
	re_end = re_match_end(nsub);
	color_area (re_begin, (re_end > end) ? end : re_end, color);

	goto_char (re_match_end(nsub));
	goto_next_char();

	if (current_position() >= end)
	  break;
    }
    goto_char (orig);
}

/*
**	Function name : color_comment_in_region
**
**	Description : Fonction speciale pour les commentaires car
**         la recherche se fait en 2 fois (debut et fin du commentaire).
**
**	Input : Le debut et la fin de la region et la couleur.
**	Output :
*/
color_comment_in_region ( char * re_cpp_begin_comment, char * re_cpp_end_comment, int start, int end, char * color)
{
    int orig = current_position ();
    int begin_comment, end_comment, pos;

    /* Les commentaires sur plusieurs lignes */
    goto_char(start);
    while (1) {
	pos = re_forward_search (re_cpp_begin_comment);
	if ((pos == -1) || (pos == -2) || (pos > end))
	  break;

	begin_comment = re_match_beginning(0);
	if (begin_comment >= end)
	  break;
	goto_char (begin_comment);
	
	/* Recherche de la fin du commentaire */
	pos = re_forward_search (re_cpp_end_comment);
	if ((pos == -1) || (pos == -2) || (pos > end))
	  break;
	
	end_comment = (re_match_end(0) > end) ? end : re_match_end(0);
	color_area (begin_comment, end_comment, color);
	goto_char (end_comment);

	if (current_position() >= end)
	  break;
    }
    goto_char (orig);
}

/*
 * ###########################################################################
 *                       C/C++ Regexp
 * ###########################################################################
 */
/* ================== */    
/* identificateur C++ */
/* ================== */
char * re_cpp_identificator = "[a-zA-Z_][a-zA-Z_0-9]*";

/* =========== */    
/* espaces C++ */
/* =========== */

/* Regexp pour un espace optionnel en C++ (saut de ligne non permis) */
char * re_cpp_opt_space = "[ \t]*";

/* Regexp pour un espace obligatoire en C++ (saut de ligne non permis) */
char * re_cpp_space = "[ \t]+";

/* Regexp pour un espace optionnel en C++ (saut(s) de ligne permis) */
char * re_cpp_opt_space_with_ret = "[ \t\n]*";

/* Regexp pour un espace obligatoire en C++ (saut(s) de ligne permis) */
char * re_cpp_space_with_opt_ret = "[ \t\n]+";

/*  Regexp pour un const optionnel en C++ */
char * re_cpp_opt_const = 
  concat4 ( "\\(", "const", re_cpp_space_with_opt_ret, "\\)?");

/*  Regexp pour un unsigned optionnel en C++ */
char * re_cpp_opt_unsigned = 
  concat4 ( "\\(", "unsigned", re_cpp_space_with_opt_ret, "\\)?" );

/*  Regexp pour un int optionnel en C++ */
char * re_cpp_opt_int = 
  concat4 ( "\\(", "int", re_cpp_space_with_opt_ret, "\\)?" );
  
/*  Regexp pour une * optionelle */
char * re_cpp_opt_pointer = "\\*?";

/*  
 *  Regexp pour des * ou un & optionnel(les) suivi(e)(s) d'espace(s)
 *  optionnel(s)
 */   
char * re_cpp_opt_pointers_or_reference = 
  concat2 ( concat4 ( re_cpp_opt_space_with_ret, "\\(", "\\*+", "\\|" ),
	   concat4 ( "&", "\\|","\\)", re_cpp_opt_space_with_ret ));

/*
 =========================================================================
 type C++

 NB : comprend les eventuels espaces qui le suivent
      (dans cpp_opt_pointers_or_reference)
       => quand on l'utilise dans les regexps, ne pas le faire suivre de
          cpp_space ni cpp_space_with_ret ni cpp_space_with_opt_ret
 =========================================================================
*/ 
/*   Regexp pour un type C++ */

char * re_cpp_type = 
  concat4 ( concat4 ( re_cpp_opt_const, "\\(", re_cpp_opt_unsigned, "\\(" ),
	   concat4 ( "\\(short\\|long\\)", re_cpp_space, re_cpp_opt_int, "\\|" ),
	   concat4 ( "char", "\\)", "\\|", re_cpp_identificator ),
	   concat2 ( "\\)", re_cpp_opt_pointers_or_reference ));
  
/* =========================================== */
/* type de retour C++ : a priori tout type C++ */
/* pouvant etre precede de static              */
/* =========================================== */

char * re_cpp_return_type = concat4("\\(static", re_cpp_space_with_opt_ret, 
				    "\\)?", re_cpp_type);

/* =============== */
/* superclasse C++ */
/* =============== */

char * re_cpp_super_class = 
  concat4 ( concat4 ( "\\(", "virtual", re_cpp_space_with_opt_ret, "\\|" ),
	   concat4 ( "\\)", "\\(", "private", "\\|" ),
	   concat4 ( "public", "\\|", "protected", "\\)" ),
	   concat2 ( re_cpp_space_with_opt_ret, re_cpp_identificator ));


/* ############################################### */
/*	       Definitions de toplevel             */
/* ############################################### */

/* =========================== */
/* definition d'une classe C++ */
/* =========================== */
char * re_cpp_class_definition = 
  concat4 ( concat4 ( "^class", re_cpp_space_with_opt_ret,
		     re_cpp_identificator, re_cpp_opt_space_with_ret ),
	   concat4 ( "\\(", ":", re_cpp_opt_space_with_ret, re_cpp_super_class ),
	   concat4 ( re_cpp_opt_space_with_ret, "\\(", ",", re_cpp_opt_space_with_ret ),
	   concat4 ( re_cpp_super_class, re_cpp_opt_space_with_ret, "\\)*",
		    concat2 ( "\\)?", "{" )));

/*  Regexp de fin de definition de classe */
char * re_cpp_class_definition_end = "^[ \t]*};";

/* ========================== */
/* definition d'une macro C++ */
/* ========================== */
char * re_cpp_macro_definition =
  concat3 ( "^#define", re_cpp_space_with_opt_ret, re_cpp_identificator );

/* ============================ */
/* definition d'une methode C++ */
/* ============================ */
char * re_cpp_method_definition = 
  concat3 ( concat4 ( "^", re_cpp_return_type, re_cpp_identificator, 
		     re_cpp_opt_space_with_ret ),
	   concat4 ( "::", re_cpp_opt_space_with_ret,
		    re_cpp_identificator, re_cpp_opt_space_with_ret ),
	   "(" );

/* ============================= */
/* definition d'un operateur C++ */
/* ============================= */
char * re_cpp_operator_symbol = 
  concat4 ( concat4 ( "\\(", "\\+=?", "\\|-=?", "\\|\\*=?" ),
	   concat4 ( "\\|/=?", "\\|%=?", "\\|\\^=?", "\\|&=?" ),
	   concat4 ( "\\||=?", "\\|~", "\\|!=?", "\\|==?" ),
	   concat4 ( concat4 ( "\\|<", "\\|<=", "\\|>=?", "\\|\\+\\+" ),
		    concat4 ( "\\|--", "\\|<<=?", "\\|>>=?", "\\|&&" ),
		    concat4 ( "\\|||", "\\|,", "\\|->\\*", "\\|->" ),
		    concat3 ( concat4 ( "\\|new", "\\|delete", "\\|()",
				       "\\|\\[\\]" ),
			     concat3 ( "\\|\\[\\]", "\\|\\.", "\\|\\.\\*" ),
			     concat3 ( "\\|::", "\\|\\?:", "\\)" ))));

/*  Regexp pour la (re)definition d'un operateur C++ */
char * re_cpp_operator_definition =
  concat4 ( concat4 ( "^", re_cpp_return_type, "\\(", "operator" ),
	   concat4 ( re_cpp_opt_space_with_ret, "\\(", "<<", "\\|" ),
	   concat4 ( ">>", "\\)", "\\|",re_cpp_identificator ),
	   concat3 ( concat3 ( re_cpp_opt_space_with_ret, "::",
			      re_cpp_opt_space_with_ret ),
		    concat3 ( "operator", re_cpp_opt_space_with_ret, 
			     re_cpp_operator_symbol ),
		    concat3 ( "\\)", re_cpp_opt_space_with_ret, "(" )));

/*  Regexp simplifiee pour la (re)definition d'un operateur C++ */
char * re_cpp_operator_like_definition =
  concat2 ( concat4 ( "^", re_cpp_return_type, "operator",
		     re_cpp_opt_space_with_ret ),
	   "[^ \t\n]+" );

/* ============================= */
/* definition d'une fonction C++ */
/* ============================= */
char * re_cpp_function_definition = 
  concat2 ( concat4 ( "^", re_cpp_return_type, re_cpp_identificator,
		     re_cpp_opt_space_with_ret ),
	   "(" );

/* =============================== */
/* definition d'un destructeur C++ */
/* =============================== */
char * re_cpp_destructor_definition =
  concat4 ( concat4 ( "^", re_cpp_identificator, re_cpp_opt_space_with_ret, "::" ),
	   concat4 ( re_cpp_opt_space_with_ret, "~",
		    re_cpp_opt_space_with_ret, re_cpp_identificator ),
	   re_cpp_opt_space_with_ret, 
	   "(" );
  
/* ================================ */
/* definition d'un constructeur C++ */
/* ================================ */
char * re_cpp_constructor_definition =
  concat2 ( concat4 ( "^", re_cpp_identificator, re_cpp_opt_space_with_ret, "::" ),
	   concat4 ( re_cpp_opt_space_with_ret, re_cpp_identificator,
		    re_cpp_opt_space_with_ret, "(") );

/* ========================================= */
/* definition d'une fonction, d'une methode, */
/* d'un constructeur ou d'un destructeur     */
/* ========================================= */
/*   "Regexp pour la definition d'une entite operationnelle C++") */
char * re_cpp_operational_entity_definition = 
  concat4 ( concat4 ( "^", "\\(", re_cpp_return_type, "\\)?" ),
	   concat4 ( "\\(", re_cpp_identificator, re_cpp_opt_space_with_ret, "::" ),
	   concat4 ( re_cpp_opt_space_with_ret, "~?", re_cpp_opt_space_with_ret, "\\)?" ),
	   concat3 ( re_cpp_identificator, re_cpp_opt_space_with_ret, "(" ));

/* ======================== */
/* definition d'un type C++ */
/* ======================== */
/*  Regexp pour la definition d'un type C++ par typedef */
char * re_cpp_typedef_type_definition = 
  concat4 ( concat4 ( "^typedef", re_cpp_space_with_opt_ret, re_cpp_type, "\\(" ),
	   concat4 ( "\\(", re_cpp_identificator, "\\|", "(" ),
	   concat4 ( re_cpp_opt_space, re_cpp_opt_pointer,
		    re_cpp_identificator, re_cpp_opt_space ),
	   concat2 ( concat3 ( ")", "\\)", "\\|" ),
		    concat2 ( "[^;]*;", "\\)" )));
		     
/* Regexp pour la definition d'un type d'enumeration C++ */
char * re_cpp_enum_type_definition = 
  concat4 ( concat4 ( "^enum", re_cpp_space_with_opt_ret,
		     re_cpp_identificator, re_cpp_space_with_opt_ret ),
	   "{[^}]+}", 
	   re_cpp_opt_space_with_ret,
	   ";" );

/* Regexp pour la definition d'un type C++ */
char * re_cpp_type_definition = 
  concat2 ( concat4 ( "\\(", re_cpp_typedef_type_definition, "\\|",
		     re_cpp_enum_type_definition ),
	   "\\)" );

/* ============================== */
/* definition d'une constante C++ */
/* ============================== */
char * re_cpp_constant_definition =
  concat4 ( concat4 ( "^", re_cpp_type, re_cpp_identificator, "\\(" ),
	   concat4 ( "\\|", re_cpp_opt_space_with_ret, "\\[.*\\]", "\\)" ),
	   re_cpp_opt_space_with_ret,
	   "=" );

/* =================================================== */
/* definition d'une variable instance d'une classe C++
/* =================================================== */
/*
   "Regexp pour un debut de definition de variable
   instance d'une classe C++
*/   
char * re_cpp_class_instance_variable_definition_start = 
  concat2 ( concat4 ( "^", re_cpp_type, re_cpp_identificator,
		     re_cpp_opt_space_with_ret ),
	   "(.*)" );

/* Regexp pour la definition d'une variable instance d'une classe C++ */
char * re_cpp_class_instance_variable_definition =
  concat3 ( re_cpp_class_instance_variable_definition_start, re_cpp_opt_space, ";" );

/*  Regexp pour la definition de variables instances d'une classe C++ */
char * re_cpp_class_multiple_instance_variable_definition = 
  concat3 ( re_cpp_class_instance_variable_definition_start, re_cpp_opt_space, "," );

/* =================================== */
/* initialisation d'un attribut static */
/* =================================== */
/*  Regexp d'initialisation d'un attribut static dans un .C */
char * re_cpp_static_attribute_initialization =
  concat3 ( concat4 ( "^", re_cpp_type, re_cpp_identificator,
		     re_cpp_opt_space_with_ret ),
	   concat4 ( "::", re_cpp_opt_space_with_ret,
		    re_cpp_identificator, re_cpp_opt_space_with_ret ),
	   "=" );

/*
   Regexp d'appel d'une macro de definition, calculee d'apres
   c++-defining-macros
*/   

/* =============================================== */
/* ensemble des expressions toplevel interessantes */
/* pour les TAGS et la documentation               */
/* =============================================== */
/*  Les expressions interessantes pour les TAGS et la documentation C++ */
char * re_cpp_interesting_expressions = 
  concat4 ( concat4 ( "\\(", re_cpp_type_definition,
		     "\\|", re_cpp_class_definition ),
	   concat4 ( "\\|", re_cpp_operational_entity_definition,
		    "\\|", re_cpp_operator_like_definition ),
	   concat4 ( "\\|", re_cpp_constant_definition,
		    "\\|", re_cpp_class_instance_variable_definition ),
	   concat2 ( concat4 ( "\\|",
			      re_cpp_class_multiple_instance_variable_definition,
			      "\\|", re_cpp_static_attribute_initialization ),
		    concat3 ( "\\|", re_cpp_macro_definition, "\\)" )));

/* ========================================================= */
/* ensemble des expressions toplevel sur lesquelles s'arrete */
/* beginning-of-defun                                        */
/* ========================================================= */
/*
   Les expressions toplevel pour beginning-of-defun
   (devant s'arreter avant le {)
*/   
char * re_cpp_beginning_of_defun_regexp =
  concat4 ( concat4 ( "\\(", substring (re_cpp_class_definition, 0,
					strlen(re_cpp_class_definition) -1),
		     "\\|", substring (re_cpp_function_definition, 0,
				       strlen(re_cpp_function_definition) -1)),
	   concat4 ( "\\|", substring (re_cpp_destructor_definition, 0,
				       strlen(re_cpp_destructor_definition) -1),
		    "\\|", substring (re_cpp_constructor_definition, 0,
				      strlen(re_cpp_constructor_definition) -1)),
	   concat4 ( "\\|", substring (re_cpp_method_definition, 0,
				       strlen(re_cpp_method_definition) -1),
		    "\\|", substring (re_cpp_operator_definition, 0,
				      strlen(re_cpp_operator_definition) -1)),
	   "\\)" );
  
/* ########################################### */
/*   Definitions dans une definition de classe */
/* ########################################### */    
/*   Regexp de definition d'un attribut dans un .H */
char * re_cpp_attribute_definition_start =
  concat4 ( concat4 ( re_cpp_type, "\\(", re_cpp_identificator,"\\|" ),
	   concat4 ( "(", re_cpp_opt_space, "\\*", re_cpp_opt_space ),
	   concat4 ( re_cpp_identificator, re_cpp_opt_space, ")",
		re_cpp_opt_space_with_ret ),
	   concat2 ( "\\[.*\\]", "\\)" ));

/*  Regexp de definition d'un seul attribut dans un .H */
char * re_cpp_attribute_definition =
  concat2 ( concat4 ( "^", re_cpp_opt_space, re_cpp_attribute_definition_start,
		     re_cpp_opt_space ),
	   ";" );

/* Regexp de definition de plusieurs attributs de meme type dans un .H */
char * re_cpp_multiple_attribute_definition =
  concat2 ( concat4 ( "^", re_cpp_opt_space, re_cpp_attribute_definition_start,
		     re_cpp_opt_space ),
	   "," );

/*   Regexp de definition d'un attribut static dans un .H */
char * re_cpp_static_attribute_definition =
  concat4 ( concat4 ( "^", re_cpp_opt_space, "static", re_cpp_space ),
	   re_cpp_attribute_definition_start,
	   re_cpp_opt_space, 
	   ";" );

/*   Regexp de declaration de methode dans un .H */
char * re_cpp_method_declaration =
  concat3 ( concat4 ( "^", re_cpp_opt_space, re_cpp_return_type, re_cpp_identificator ),
	   re_cpp_opt_space_with_ret,
	   "(" );

/*  Regexp de declaration de methode dans un .H */
char * re_cpp_virtual_method_declaration =
  concat2 ( concat4 ( "^", re_cpp_opt_space, "virtual", re_cpp_opt_space ),
	   concat4 ( re_cpp_return_type, re_cpp_identificator,
		    re_cpp_opt_space_with_ret, "(" ));

/* Regexp de declaration de redefinition d'un operateur dans un .H */
char * re_cpp_operator_declaration =
  concat4 ( "^", re_cpp_opt_space, re_cpp_return_type, "operator" );

/*   Regexp de declaration virtuelle d'un operateur dans un .H */
char * re_cpp_virtual_operator_declaration =
  concat3 ( concat4 ( "^", re_cpp_opt_space, "virtual", re_cpp_opt_space ),
	  re_cpp_return_type, "operator" );

/*   Regexp de declaration d'un destructeur dans un .H */
char * re_cpp_destructor_declaration =
  concat4 ( concat4 ( "^", re_cpp_opt_space, "~", re_cpp_opt_space ),
	   re_cpp_identificator,
	   re_cpp_opt_space_with_ret,
	   "(" );

/*   Regexp de declaration d'un destructeur dans un .H */
char * re_cpp_virtual_destructor_declaration =
  concat3 ( concat4 ( "^", re_cpp_opt_space, "virtual", re_cpp_opt_space ),
	   concat4 ( "~", re_cpp_opt_space, re_cpp_identificator,
		    re_cpp_opt_space_with_ret ),
	   "(" );

/*   Regexp de declaration d'un destructeur dans un .H */
char * re_cpp_constructor_declaration =
  concat4 ( concat4 ( "^", re_cpp_opt_space, re_cpp_identificator,
		     re_cpp_opt_space_with_ret ), 
	   "(",
	   re_cpp_opt_space,
	   "[^ \t\n*]" );

/* ======================================================================== */ 
/* ensemble des expressions interessantes pour les TAGS et la documentation */
/* a l'interieur d'une definition de classe                                 */
/* ======================================================================== */ 
/*
   Les expressions de classe interessantes pour les TAGS et la
   documentation C++
*/   
char * re_cpp_interesting_expressions_inside_class_definition =
  concat4 ( concat4 ( "\\(", re_cpp_attribute_definition,
		     "\\|", re_cpp_multiple_attribute_definition ),
	   concat4 ( "\\|", re_cpp_static_attribute_definition,
		    "\\|", re_cpp_method_declaration ),
	   concat4 ( "\\|", re_cpp_virtual_method_declaration,
		    "\\|", re_cpp_operator_declaration ),
	   concat3 ( concat4 ( "\\|", re_cpp_virtual_operator_declaration,
			      "\\|", re_cpp_destructor_declaration ),
		    concat4 ( "\\|", re_cpp_virtual_destructor_declaration,
			     "\\|", re_cpp_constructor_declaration ),
		    "\\)" ));

/*
 * ###########################################################################
 *                       Elements a colorier dans l'ordre
 * ###########################################################################
 */

char * re_cpp_define = "^\\(#[ \t]*\\(undef\\|define\\)\\)[ \t]*.*$";
char * re_cpp_ifdef = "^\\(#[ \t]*\\(ifdef\\|else\\|ifndef\\|if\\|endif\\)\\)[ \t]*.*$";
char * re_cpp_include = "^\\(#[ \t]*include\\)[ \t]+.*$";

/* A voir */
char * re_cpp_base_type = 
  concat3 ( "[ \n\t({]",
	   "\\(int\\|char\\|long\\|float\\|double\\)",
	   "[ \n\t)};]" );

char * re_cpp_class = 
  concat3 ( "^[ \t]*\\(",
	   "template\\|typedef\\|struct\\|union\\|class\\|enum",
	   "\\)[ \t<]+.*$" );

char * re_cpp_delete_new = "[^_]\\<\\(delete\\|new\\|free\\|malloc\\)\\>[^_]";

char * re_cpp_the_return = "[^_]\\<\\(return\\)\\>[^_]";

char * re_cpp_keyword = 
  concat4 ( "[^_]\\<\\(",
	   "goto\\|if\\|else\\|case\\|default\\|switch",
	   "\\|break\\|continue\\|while\\|do\\|for",
	   "\\)\\>[^_]" );

char * re_cpp_alloc_class = 
  concat4 ( "[ \n\t({]\\(",
	   "extern\\|static\\|public\\|protected",
	   "\\|private\\|friend\\|virtual",
	   "\\)[ \n\t):};]" );
  
char *re_cpp_func_meth_cons_dest_class_name =
  concat4 ( concat4 ( "^\\(",
		     substring (re_cpp_class_definition,
				0, strlen(re_cpp_class_definition) -1),
		     "\\|",
		     substring (re_cpp_constructor_definition,
				0, strlen(re_cpp_constructor_definition) -1) ),
	   concat4 ( "\\|",
		     substring (re_cpp_destructor_definition,
				0, strlen(re_cpp_destructor_definition) -1),
		    "\\|",
		     substring (re_cpp_operator_definition,
				0, strlen(re_cpp_operator_definition) -1)),
	   concat4 ( "\\|",
		     substring (re_cpp_method_definition,
				0, strlen(re_cpp_method_definition) -1),
		    "\\|",
		    substring (re_cpp_function_definition,
				0, strlen(re_cpp_function_definition) -1)),
	   "\\)" );

char * re_cpp_comment = "//.*$";
char * re_cpp_begin_comment = "/\\*";
char * re_cpp_end_comment = "\\*/";
char * re_cpp_string = gen_string;

/*
**	Function name : c_cpp_color_region
**
**	Description : Colorisation complete d'une region pour 
**         toutes les regexp definies.
**         On suppose qu'il ya une marque avant ou apres le curseur.
**
**	Input :
**	Output :
*/
c_cpp_color_region (int start, int end) 
{
    color_regexp_in_region ( re_cpp_string, 1, start, end, cpp_string_color );
    color_regexp_in_region ( re_cpp_base_type, 1, start, end, cpp_base_type_color );
    color_regexp_in_region ( re_cpp_delete_new, 1, start, end, cpp_delete_new_color );
    color_regexp_in_region ( re_cpp_the_return, 1, start, end, cpp_return_color );
    color_regexp_in_region ( re_cpp_func_meth_cons_dest_class_name, 0, start, end, cpp_func_color );
    color_regexp_in_region ( re_cpp_class, 1, start, end, cpp_class_color );
    color_regexp_in_region ( re_cpp_alloc_class, 1, start, end, cpp_alloc_class_color ); 
    color_regexp_in_region ( re_cpp_define, 1, start, end, cpp_define_color );
    color_regexp_in_region ( re_cpp_ifdef, 1, start, end, cpp_ifdef_color );
    color_regexp_in_region ( re_cpp_keyword, 1, start, end, cpp_keyword_color );
    color_regexp_in_region ( re_cpp_include, 1, start, end, cpp_include_color );

    /* Les commentaires C++ sur une ligne */
    color_regexp_in_region (re_cpp_comment, 0, start, end, cpp_comment_color );

    /* Les commentaires sur plusieurs lignes */
    color_comment_in_region ( re_cpp_begin_comment, re_cpp_end_comment,start, end, cpp_comment_color );
}


/*
 * ###########################################################################
 *                       Regexp for LaTeX
 * ###########################################################################
 */
char *re_latex_comment = "\\(^\\|[^\\]\\)\\(%.*\\)$";
char *re_latex_keyword =
  concat3 ( concat4 ( "\\(",
		    "\\\\\\(sub\\)*\\(paragraph\\|section\\)\\(\\*\\|\\[.*\\]\\)?{[^}]*}",
		     "\\|",
		     "\\\\\\(address\\|signature\\|opening\\|closing\\|chapter\\|part\\|cleardoublepage\\|multicolumn\\|multirow\\)\\(\\*\\|\\[.*\\]\\)?{[^}]*}"),
	   concat4 ( "\\|",
		    "\\\\footnote\\(mark\\|text\\)?{[^}]*}",
		    "\\|",
		    "\\\\[a-z]+box{[^}]*}"),
	   concat3 ("\\|",
		    "\\\\\\(v\\|h\\)space\\(\\*\\)?{[^}]*}",
		    "\\)" ));
char *re_latex_defun =
  concat3 ( "\\(",
	   concat2 ("\\\\\\(re\\)?new\\(environment\\|command\\)",
		    "\\|"),
	   concat2 ("\\\\new\\(length\\|theorem\\|counter\\)",
		    "\\)" ));
char *re_latex_define =
  concat3 ( "\\(",
	   concat2 ( "\\\\\\(setlength\\|hline\\|hrule\\|cline\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)",
		   "\\|"),
	   concat2 ("\\\\\\(title\\|author\\|date\\|thanks\\)",
		    "\\)" ));
char *re_latex_decl = 
  concat4 ( concat4 ( "\\(",
		    "\\\\documentclass\\(\\[.*\\]\\)?{[^}]*}",
		    "\\|",
		    "\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){[^}]*}"),
	   concat4 ("\\|",
		    "\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b",
		    "\\|",
		    "\\\\\\(pagestyle\\|bibliographystyle\\|usepackage\\|thispagestyle\\|pagenumbering\\){[^}]*}"),
	   concat4 ("\\|",
		    "\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b",
		    "\\|",
		    "\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"),
	   concat3 ("\\|",
		    "\\\\\\(bf\\|em\\|it\\|rm\\|sf\\|sl\\|sc\\|ss\\|tt\\)\\b",
		   "\\)" ));
char *re_latex_label = 
  concat2 ( concat4 ("\\(",
		     "\\\\item\\[\" \"\\]",
		     "\\|",
		     "\\\\item\\b"),
	   concat3 ("\\|",
		    "\\\\caption\\(\\[.*\\]\\)?{[^}]*}",
		    "\\)" ));
char *re_latex_include = "\\\\\\(include\\|input\\|bibliography\\){[^}]*}";
char *re_latex_italic = "{\\\\\\(em\\|it\\|sl\\){[^}]*}";
char *re_latex_bold = "{\\\\bf{[^}]*}";
char *re_latex_ref = "\\\\\\(\\(no\\)?cite\\|\\(page\\)?ref\\|refer\\|label\\|index\\|glossary\\){[^}]*}";

void latex_color_region(int start, int end)
{
    color_regexp_in_region ( re_latex_comment, 2, start, end, latex_comment_color );
    color_regexp_in_region ( re_latex_keyword, 0, start, end, latex_keyword_color );
    color_regexp_in_region ( re_latex_defun, 0, start, end, latex_defun_color );
    color_regexp_in_region ( re_latex_define, 0, start, end, latex_define_color );
    color_regexp_in_region ( re_latex_decl, 0, start, end, latex_decl_color );
    color_regexp_in_region ( re_latex_label, 0, start, end, latex_label_color );
    color_regexp_in_region ( re_latex_include, 0, start, end, latex_include_color );
    color_regexp_in_region ( re_latex_italic, 0, start, end, latex_italic_color );
    color_regexp_in_region ( re_latex_bold, 0, start, end, latex_bold_color );
    color_regexp_in_region ( re_latex_ref, 0, start, end, latex_ref_color );
}

/*
 * ###########################################################################
 *                       Regexp for Html 3.0
 * ###########################################################################
 */
char *re_html_ibtt = 
  concat2("<EM>\\|</EM>||\<B>\\|</B>\\|<I>\\|</I>\\|<TT>\\|</TT>\\|<CENTER>\\|</CENTER>\\|",
	  "<em>\\|</em>||\<b>\\|</b>\\|<i>\\|</i>\\|<tt>\\|</tt>\\|<center>\\|</center>");
char *re_html_pre = 
  concat2 ("<PRE>\\|</PRE>\\|<BLOCKQUOTE>\\|</BLOCKQUOTE>\\|",
	   "<pre>\\|</pre>\\|<blockquote>\\|</blockquote>");
char *re_html_img = 
  concat2("<IMG SRC=\\|<IMG ALIGN=TOP SRC=\\|<IMG ALIGN=MIDDLE SRC=\\|",
	  "<img src=\\|<img align=top src=\\|<img align=middle src=");
char *re_html_title = 
  concat3("<HTML>\\|</HTML>\\|<TITLE>\\|</TITLE>\\|<HEAD>\\|</HEAD>\\|<BODY\\|</BODY>\\|",
	  "<ADDRESS>\\|</ADDRESS>\\|<html>\\|</html>\\|<title>\\|</title>\\|<head>\\|",
	  "</head>\\|<body>\\|</body\\|<address>\\|</address>");
char *re_html_ref = "<A\\| HREF=\\|</A>\\|<a\\|href=\\|</a>";
char *re_html_hds = 
  concat4("<HR>\\|<BR>\\|<H1>\\|</H1>\\|<H2>\\|</H2>\\|<H3>\\|</H3>\\|<H4>\\|</H4>\\|",
	  "<H5>\\|</H5>\\|<H6>\\|</H6>\\|<CENTER>\\|</CENTER>\\|<P>\\|</P>\\|",
	  "<hr>\\|<br>\\|<h1>\\|</h1>\\|<h2>\\|</h2>\\|<h3>\\|</h3>\\|<h4>\\|</h4>\\|",
	  "<h5>\\|</h5>\\|<h6>\\|</h6>\\|<center>\\|</center>\\|<p>\\|</p>");
char *re_html_list = 
  concat2("<TR>\\|</TR>\\|<UL>\\|</UL>\\|<LI>\\|<OL>\\|</OL>\\|<DL>\\|</DL>\\|<DT>\\|<DD>\\|",
	  "<tr>\\|</tr>\\|<ul>\\|</ul>\\|<li>\\|<ol>\\|</ol>\\|<dl>\\|</dl>\\|<dt>\\|<dd>");
char *re_html_forms =
  concat2 (
	   concat4("<INPUT\\|TYPE=\\|SRC=\\|<SELECT\\|</SELECT>\\|",
		   "<OPTION>\\|<TEXTAREA\\|</TEXTAREA>\\|",
		   "<TABLE\\|</TABLE>\\|<CAPTION>\\|</CAPTION>\\|",
		   "<FORM\\|</FORM>\\|"),
	   concat4("<input\\|type=\\|src=\\|<select\\|</select>\\|",
		   "<option>\\|<textarea\\|</textarea>\\|",
		   "<table\\|</table>\\|<caption>\\|</caption>\\|",
		   "<form\\|</form>"));

void html_color_region(int start, int end)
{
  color_regexp_in_region (re_html_title, 0, start, end, html_title_color);
  color_regexp_in_region (re_html_ibtt, 0, start, end, html_ibtt_color);
  color_regexp_in_region (re_html_pre, 0, start, end, html_pre_color);
  color_regexp_in_region (re_html_img, 0, start, end, html_img_color);
  color_regexp_in_region (re_html_ref, 0, start, end, html_ref_color);
  color_regexp_in_region (re_html_list, 0, start, end, html_list_color);
  color_regexp_in_region (re_html_forms, 0, start, end, html_forms_color);
  color_regexp_in_region (re_html_hds, 0, start, end, html_hds_color);
  color_regexp_in_region (gen_string, 0, start, end, hltml_string_color);
}

/*
 * ###########################################################################
 *                       Regexp for edit directory 
 * ###########################################################################
 */
char *re_edir_directory = "^d.*$";
char *re_edir_link = "^l.*$";
char *re_edir_c_file = "^-.* \\([^ \n\t]*\\.c\\)$";
char *re_edir_tex_file = "^-.* \\([^ \n\t]*\\.tex\\)$";
char *re_edir_sc_file = "^-.* \\([^ \n\t]*\\.sc\\)$";
char *re_edir_h_file = "^-.* \\([^ \n\t]*\\.h\\)$";
char *re_edir_o_file = "^-.* \\([^ \n\t]*\\.o\\)$";
char *re_edir_tilde_file = "^-.* \\([^ \n\t]*~\\)$";
char *re_edir_makefile = "^-.* \\(makefile\\|Makefile\\|MAKEFILE\\|Imakefile\\)";
char *re_edir_readme = "^-.* \\(readme\\|Readme\\|README\\)";

void edir_color_region(int start, int end)
{
    color_regexp_in_region ( re_edir_directory, 0, start, end, edir_directory_color);
    color_regexp_in_region ( re_edir_link, 0, start, end, edir_link_color);
    color_regexp_in_region ( re_edir_c_file, 1, start, end, edir_c_file_color);
    color_regexp_in_region ( re_edir_tex_file, 1, start, end, edir_tex_file_color);
    color_regexp_in_region ( re_edir_sc_file, 1, start, end, edir_sc_file_color);
    color_regexp_in_region ( re_edir_h_file, 1, start, end, edir_h_file_color);
    color_regexp_in_region ( re_edir_o_file, 1, start, end, edir_o_file_color);
    color_regexp_in_region ( re_edir_tilde_file, 1, start, end, edir_tilde_file_color);
    color_regexp_in_region ( re_edir_makefile, 1, start, end, edir_makefile_color);
    color_regexp_in_region ( re_edir_readme, 1, start, end, edir_readme_color);
}

/*
 * ###########################################################################
 *                       Regexp for Fortran.
 * ###########################################################################
 */
char *re_fortran_comment = "^[*Cc].*$";
char *re_fortran_include = 
  concat4 ("[ \t]\\(",
	   "call\\|stop\\|return\\|end\\|include\\|",
	   "CALL\\|STOP\\|RETURN\\|END\\|INCLUDE",
	   "\\)[' \t\n]");
char *re_fortran_func = 
  concat4 ("[ \t]\\(",
	   "program\\|subroutine\\|function\\|",
	   "PROGRAM\\|SUBROUTINE\\|FUNCTION",
	   "\\)[ \t\n]");
char *re_fortran_type =
  concat4 ("\\([ \t]\\|implicit[ \t]*\\|IMPLICIT[ \t]*\\)\\(",
	   "dimension\\|integer\\|real\\|double[ \t]*precision\\|character\\|logical\\|complex\\|double[ \t]*complex\\|",
	   "DIMENSION\\|INTEGER\\|REAL\\|DOUBLE[ \t]*PRECISION\\|CHARACTER\\|LOGICAL\\|COMPLEX\\|DOUBLE[ \t]*COMPLEX",
	   "\\)\\(\\*[0-9]*\\|[ \t\n]\\)");
char *re_fortran_decl = 
  concat3 ("implicit[ \t]*none",
	   "\\|",
	   "[ \t]\\(parameter[\t\n ]*([^)]*)\\|data\\|save\\|common[ \t\n]*/[^/]*/\\)");
char *re_fortran_string = concat3("\\(",gen_string,"\\|'.*'\\)");
char *re_fortran_keyword = 
  concat4 ("\\([ \t]\\(",
	   "do\\|do[ \t]*[0-9]+\\|go[ \t]*to[ \t]*[0-9]+\\|end[ \t]*do\\|if\\|else[ \t]*if\\|then\\|else\\|end[ \t]*if\\|",
	   "DO\\|DO[ \t]*[0-9]+\\|GO[ \t]*TO[ \t]*[0-9]+\\|END[ \t]*DO\\|IF\\|ELSE[ \t]*IF\\|THEN\\|ELSE\\|END[ \t]*IF",
	   "\\)[ \t\n(]\\|\\(^[ \t]*[0-9]+\\|[ \t]\\(continue\\|CONTINUE\\)[ \t\n]\\|format\\|FORMAT\\)\\)");

void fortran_color_region(int start, int end)
{
   color_regexp_in_region ( re_fortran_include, 0, start, end,
			   fortran_include_color); 
   color_regexp_in_region ( re_fortran_func, 1, start, end,
			   fortran_func_color); 
   color_regexp_in_region ( re_fortran_string, 1, start, end, 
                           fortran_string_color);
   color_regexp_in_region ( re_fortran_decl, 1, start, end, 
			   fortran_decl_color);
   color_regexp_in_region ( re_fortran_keyword, 0, start, end, 
			   fortran_keyword_color);
   color_regexp_in_region ( re_fortran_type, 0, start, end, 
			   fortran_type_color);
   color_regexp_in_region ( re_fortran_comment, 0, start, end, 
			   fortran_comment_color);
}

/*
 * ###########################################################################
 *                       Regexp for Ada.
 * ###########################################################################
 */
char *re_ada_comment = ";;\\|--.*$";
char *re_ada_glob_struct = 
  concat2 ( 
	   concat4 ("\\([ \t\n]procedure[ \t]+[^ \t\n]+[ \t]",
		    "\\|[ \t\n]task[ \t]+[^ \t\n]+[ \t]",
		    "\\|[ \t\n]function[ \t]+[^ \t\n]+[ \t]",
		    "\\|[ \t\n]package[ \t]+[^ \t\n]+[ \t]"),
	   concat2 ("\\|^[ \t]*private[ \t\n]",
		    "\\|^end.*$\\)"));
char *re_ada_struct = 
  concat3 ("[ \n\t]\\(is\\|in\\|out\\|select\\|if\\|else\\|case\\|when\\|and\\|or\\|not\\|accept\\|",
	   "loop\\|do\\|then\\|elsif\\|else\\|for\\|while\\|exit\\|begin\\|end\\|",
	   "declare\\|exception\\|generic\\|raise\\|return\\|package\\|body\\)[ \n\t;]");
char *re_ada_decl = 
  concat3 ("\\(^[ \t]*\\(type\\|subtype\\)[ \t\n]+[^ \t]+[ \t\n]",
	   "\\|",
	   "[ \t]+is record.*$\\)");

char *re_ada_include = "^[ \t]*\\(with\\|pragma\\|use\\)";
char *re_ada_string = gen_string;

void ada_color_region(int start, int end)
{
   color_regexp_in_region ( re_ada_string, 1, start, end, ada_string_color);
   color_regexp_in_region ( re_ada_include, 0, start, end, ada_include_color);
   color_regexp_in_region ( re_ada_glob_struct, 1, start, end, ada_glob_struct_color);
   color_regexp_in_region ( re_ada_struct, 1, start, end, ada_struct_color);
   color_regexp_in_region ( re_ada_decl, 0, start, end, ada_decl_color);
   color_regexp_in_region ( re_ada_comment, 0, start, end, ada_comment_color);
}

/*
 * ###########################################################################
 *                       Regexp for Perl.
 * ###########################################################################
 */
char *re_perl_comment = "\\s #.*$\\|^#.*$";
char *re_perl_string = gen_string;
char *re_perl_label = "^\\(__....?__\\|\\s *\\sw+:\\)";
char *re_perl_include = "^require.*$";
char *re_perl_decl = "^package.*$";
char *re_perl_defun = "^\\s *sub\\s +\\(\\w\\|[_']\\)+";
char *re_perl_keyword = "\\b\\(do\\|if\\|unless\\|while\\|until\\|else\\|elsif\\|for\\|foreach\\|continue\\|next\\|redo\\|last\\|goto\\|return\\|die\\|exit\\)\\b";

void perl_color_region(int start, int end)
{
   color_regexp_in_region ( re_perl_string , 1, start, end, perl_string_color);
    color_regexp_in_region ( re_perl_label , 0, start, end, perl_label_color);
    color_regexp_in_region ( re_perl_include , 0, start, end, perl_include_color);
    color_regexp_in_region ( re_perl_decl , 0, start, end, perl_decl_color);
    color_regexp_in_region ( re_perl_defun , 0, start, end, perl_defun_color);
    color_regexp_in_region ( re_perl_keyword , 0, start, end, perl_keyword_color);
    color_regexp_in_region ( re_perl_comment , 0, start, end, perl_comment_color);
}

/*
 * ###########################################################################
 *                       Regexp for Shell
 * ###########################################################################
 */
char *re_shell_comment = "^#.*\\|[ \t]#.*";
char *re_shell_string = gen_string;
char *re_shell_include = "^source[ \t].*$";
char *re_shell_define = "[^a-zA-Z0-9_#]set[ \t]+[a-zA-Z0-9_]+\\|[^a-zA-Z0-9_#]setenv[ \t]+[a-zA-Z0-9_]+";
char *re_shell_var = "\\$\\??[A-Za-z0-9_]+";
char *re_shell_keyword = 
	   concat3 ( "[^_]\\<\\(if\\|then\\|else\\|fi\\|endif\\|case\\|default\\|switch\\|breaksw\\|endsw\\|continue\\|while\\|do\\|for\\)\\>[^_]",
		    "\\|",
		    "^\\(if\\|then\\|else\\|fi\\|endif\\|case\\|default\\|switch\\|breaksw\\|endsw\\|continue\\|while\\|do\\|for\\)\\>[^_]"
		    );

void shell_color_region(int start, int end)
{
    color_regexp_in_region ( re_shell_string , 1, start, end, shell_string_color);
    color_regexp_in_region ( re_shell_include , 0, start, end, shell_include_color);
    color_regexp_in_region ( re_shell_define , 0, start, end, shell_define_color);
    color_regexp_in_region ( re_shell_var , 0, start, end, shell_var_color);
    color_regexp_in_region ( re_shell_keyword , 0, start, end, shell_keyword_color);
    color_regexp_in_region ( re_shell_comment , 0, start, end, shell_comment_color);
}

/*
 * ###########################################################################
 *                       Regexp for Imake and Makefile.
 * ###########################################################################
 */
char *re_make_comment = "^#.*$\\|[^$]#.*$";
char *re_make_rules = 
  concat3 ( "^[^ \t\n]*%[^ \t\n]*[ \t]*::?[ \t]*[^ \t\n]*[ \t]*\\(#.*\\)?$",
	   "\\|",
	   "^[.][A-Za-z][A-Za-z]?\\..*$");
char *re_make_define = 
  concat2 ( "^[_A-Za-z0-9]+[ \t]*\\+?=",
	   concat4 ( "\\|",
		     "^[_A-Za-z0-9]+[ \t]*:[ \t]*sh[ \t]*=",
		     "\\|",
		     "\\( \\|:=\\)[_A-Za-z0-9]+[ \t]*\\+="));
char *re_make_keyword = "\\$\\([^ \t\n{(/]+\\|[{(]@?[_A-Za-z0-9:.,%/=]+[)}]\\)";
char *re_make_defun = "^[A-Za-z0-9.,/_-]+[ \t]*:[^=\n]*$";
char *re_make_include = "^include ";

void make_color_region(int start, int end)
{
   color_regexp_in_region ( re_make_rules , 0, start, end, make_rules_color);
    color_regexp_in_region ( re_make_define , 0, start, end, make_define_color);
    color_regexp_in_region ( re_make_keyword , 0, start, end, make_keyword_color);
    color_regexp_in_region ( re_make_defun , 0, start, end, make_defun_color);
    color_regexp_in_region ( re_make_include , 0, start, end, make_include_color);  
    color_regexp_in_region ( re_make_comment , 0, start, end, make_comment_color);
}

/*
 * ###########################################################################
 *                     Internal color switch  
 * ###########################################################################
 */
color_internal(int start, int end)
{
    char *name = filename();
    char *file;
    char *mode_name = current_mode();
    
    file = basename(name);
    if ((strcmp(mode_name,"C-mode") == 0)
	||(strcmp(mode_name,"C++mode") == 0)) {
	c_cpp_color_region(start, end);
    }
    
    else if ((strcmp(mode_name,"Edir") == 0)) {
	edir_color_region(start, end);
    }
    else if ((strcmp(mode_name,"Latex") == 0)) {
	latex_color_region(start, end);
    }
    else if ((strcmp(mode_name,"Html") == 0)) {
	html_color_region(start, end);
    }
    else if ((strcmp(mode_name,"Fortran") == 0)) {
	fortran_color_region(start, end);
    }
    else if ((strcmp(mode_name,"Ada") == 0)) {
	ada_color_region(start, end);
    }
    else if ((strcmp(mode_name,"Perl") == 0)) {
	perl_color_region(start, end);
    }
    else if ((strcmp(mode_name,"shell") == 0)) {
	shell_color_region(start, end);
    }
    
    else {
	if (file &&((strcmp(file, "Makefile")== 0)
	    || (strcmp(file, "MAKEFILE")== 0)
	    || (strcmp(file, "makefile")== 0)
	    || (strcmp(file, "Imakefile")== 0))) {
	    make_color_region(start, end);
	}
    }

    if (name)
      free(name);
    if(file)
      free(file);
    if(mode_name)
      free(mode_name);
}

/*
 * ###########################################################################
 *                     Smac user function.
 * ###########################################################################
 */
color_buffer()
{
    int orig = current_position ();

    if(monochrome())
      return;
    
    watch_on();
    remove_colors ();
    color_internal(0, end_of_file());
    goto_char(orig);
    watch_off();
}

/*
 * ###########################################################################
 *                     Smac user function.
 * ###########################################################################
 */
color_region()
{
    char *mode_name = current_mode();
    int orig = current_position ();
    int end, pos, start;
    
    if(monochrome())
      return;
    
    watch_on();
    goto_mark ();
    if ( (pos=current_position()) == orig ) {
	display_message ("Warning : no region defined");
	watch_off();
	return;
    }
    else if (pos > orig) {
	start = orig;
	end = pos;
    }
    else {
	start = pos;
	end = orig;
    }

    color_internal(start, end);

    goto_char (orig);
    watch_off();
}

