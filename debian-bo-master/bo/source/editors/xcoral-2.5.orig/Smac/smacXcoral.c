/* ########################################################################

			      smacXcoral.c

   File: smacXcoral.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/smacXcoral.c
   Description: 
   Created: Tue Feb 21 13:01:08 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:01:08 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

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


#include <stdio.h>
#include <string.h>

/*#include "error.h"*/
#include "FunctionCall.h"
#include "Builtin.h"
#include "Identifier.h"
#include "funcall.h"
#include "smac.h"
#include "control.h"
#include "stack.h"

/* Le buffer courant */

char * i_buffer_courant;


/* Pour lire la region (du buffer courant) pour eval_region */

int i_curseur, i_curseur_sup;


/* Pour lire le minibuffer pour eval_expression */

char * i_minibuffer;


/* Pour lire un fichier pour load_file */

extern FILE * yyin;
extern int yyinflag;
extern char * yyinfilename;
extern int numlig;


/* ---------------------------------
   
   Les fonctions appelees par Xcoral


   Elles retournent 0 lorsque tout va
   bien, sinon une chaine de caracteres
   qui indique l'erreur

   --------------------------------- */

static char * check_parser_error()
{
  if (*err_msg) {
    if (! strncmp(err_msg, "parser error", 12)) {
      extern FCT(void, DisplayWMessage ,(char *, char *, int));
      
      /* Le message n'a pas encore ete affiche */
      DisplayWMessage(err_msg, "Smac error", 1);
    }
    return err_msg;
  }

  return 0;
}

/* Chargement evaluation du fichier filename, le buffer
   courant est buff (qui n'est pas un vrai char *) */

char * ie_load_file(buff, filename)
     char * buff;
     char * filename;
{
  /* Chargement / evaluation d'un fichier */
  char * xcoral_lib = 0;
  char * smaclib = 0;
  
  i_buffer_courant = buff;

  if ((yyin = fopen(filename, "r")) == 0) {
    char * justname = strrchr(filename, '/');
    
    if (justname)
      justname += 1;
    else
      justname = filename;
    
    smaclib = (char *) getenv ("XCORAL_SMACLIB");
    if (!smaclib) {
	xcoral_lib = malloc(strlen(XCORAL_LIB_DIR) + strlen(justname) + 2);
	sprintf(xcoral_lib, "%s/%s", XCORAL_LIB_DIR, justname);
    }
    else {
	xcoral_lib = malloc(strlen(smaclib) + strlen(justname) + 2);
	sprintf(xcoral_lib, "%s/%s", smaclib, justname);
    }
    
    if ((yyin = fopen(xcoral_lib, "r")) == 0) {
      free(xcoral_lib);
      buff = 0;
      yyin = 0;
      sprintf(err_msg, "cannot open file %s\n", filename);
      return err_msg;
    }
    else
      yyinfilename = xcoral_lib;
  }
  else
    yyinfilename = filename;
  
  yyinflag = 1;
  numlig = 0;
  *err_msg = 0;
  clear_ctrl_c();
  rerun_profile();
  
  if (! setjmp(come_back))
    load_file();

  stop_profile();
  fclose(yyin);
  if (xcoral_lib) free(xcoral_lib);
  i_buffer_courant = 0;
  i_curseur = i_curseur_sup = 0;
  yyin = 0;
  yyinflag = 0;
  yyinfilename = 0;

  return check_parser_error();
}


/* Evaluation de la region du buffer buff */

char * ie_eval_region(buff)
     char * buff;
{
  extern FCT(int, ie_mark_position,(char *));
  extern FCT(int, ie_current_position,(char *));
  extern FCT(int, ie_current_line,(char *));
  extern FCT(void, ie_goto_char,(char *, int));
  extern FCT(char *, ie_filename,(char *));
  
  i_buffer_courant = buff;
  yyin = 0;
  yyinflag = 0;
  i_minibuffer = 0;
  *err_msg = 0;
  i_curseur = ie_mark_position(buff);
  i_curseur_sup = ie_current_position(buff);
  if (i_curseur_sup < i_curseur) {
    i_curseur = i_curseur_sup;
    i_curseur_sup = ie_mark_position(buff);
  }
  ie_goto_char(buff, i_curseur);
  numlig = ie_current_line(buff);
  yyinfilename = ie_filename(buff);
  clear_ctrl_c();
  rerun_profile();
  
  if (! setjmp(come_back))
    load_file();

  stop_profile();
  yyinfilename = 0;
  ie_goto_char(buff, i_curseur_sup);
  i_buffer_courant = 0;
  i_curseur = i_curseur_sup = 0;
  
  return check_parser_error();
}


/* Evaluation du minibuffer */

char * ie_eval_expression(buff, expr)
     char * buff;
     char * expr;
{
  int lg = strlen(expr);
  char * lexpr = malloc(lg + 3);

  if (! lexpr) return ("cannot allocate memory\n");

  lexpr[0] = '{';
  strcpy(lexpr + 1, expr);
  lexpr[lg + 1] = '}';
  lexpr[lg + 2] = 0;
  
  yyinfilename = 0;
  i_buffer_courant = buff;
  yyin = 0;
  yyinflag = 0;
  i_minibuffer = lexpr;
  *err_msg = 0;
  i_curseur = i_curseur_sup = 0;
  numlig = 0;
  clear_ctrl_c();
  rerun_profile();
  
  if (! setjmp(come_back))
    load_file();

  stop_profile();
  i_buffer_courant = i_minibuffer = 0;
  i_curseur = i_curseur_sup = 0;
  free(lexpr);
  
  return check_parser_error();
}


/* appel d'une fonction ou non builtin, par exemple indent_line, s'il
   y a des arguments, ceux-ci ne peuvent etre que des int ou char */

char * ie_call_function(buff, fctname, narg, args)
     char * buff;
     char * fctname;
     int narg;
     int * args;
{
  i_buffer_courant = buff;
  i_minibuffer = 0;
  yyin = 0;
  yyinflag = 0;
  *err_msg = 0;
  i_curseur = i_curseur_sup = 0;
  clear_ctrl_c();

  if (! setjmp(come_back)) {
    Identifier * ident = find_identifier(fctname);
    Function * fdef;

    if ((! ident) ||
	(! (fdef = Identifier__FunctionDef(ident)))) {
      sprintf(err_msg, "%s is not a function name", fctname);
      return err_msg;
    }
    else {
      accept_ctrl_c();
      rerun_profile();
      if (! setjmp(come_back))
	if (Function__IsBuiltin(fdef))
	  call_builtin_function_of_xcoral(fdef, narg);
	else
	  call_user_function_from_xcoral(fdef, narg, args);
      stop_profile();
      ignore_ctrl_c();
    }
  }
  
  i_buffer_courant = 0;
  i_curseur = i_curseur_sup = 0;
  
  Reinit_Control();
  Reinit_Error();
  Reinit_Stack();
  
  return (*err_msg) ? err_msg : 0;
}


/* Expansion d'un nom smac (pour eval expression) */

char * ExpandSmacName(s, n, common)
     char * s;
     int * n;
     int * common;
{
  char * result = 0;
  int rlength = 0;
  static HashItem * item = 0;
  static HashItem ** entry = 0;

  *n = 0;
  *common = strlen(s);
  
  while (HashTable_Iter(IdentifierHashTable, &item, &entry), item) {
    char * name = (char *) HashItem__Key(item);
    
    if (strstr(name, s) == name) {
      char * nresult =
	  (char *) malloc((unsigned int) rlength + strlen(name) + 3);
		 
      if (! nresult)
	return result;
	
      if (result) bcopy(result, nresult, rlength);
      strcpy(nresult + rlength, name);
      *n += 1;
      if (Identifier__FunctionDef((Identifier *) HashItem__Data(item))) {
	strcat(nresult + rlength, "()");
	rlength += strlen(name) + 3;
      }
      else
	rlength += strlen(name) + 1;
      if (result) free(result);
      result = nresult;
    }
  }

  if ((*n == 1) && (*(result + strlen(result) - 1) == ')'))
    *(result + strlen(result) - 1) = 0;
  
  return result;
}

/* Retourne la liste des fonctions definies, builtin ou non.

   Pour initialiser le processus init doit etre non nul,
   Les appels retourne un char * qui est le nom d'une fonction
   a chaque appel, 0 en fin de liste. Il ne doit pas y avoir
   de chargement/evaluation de code pendant le parcours */
   
char * ie_function_list(init)
     int init;
{
  static HashItem * item;
  static HashItem ** entry;
  
  if (init) {
    item = 0;
    entry = 0;
  }

  for (;;) {
    HashTable_Iter(IdentifierHashTable, &item, &entry);

    if (! item)
      return 0;
    else if (Identifier__FunctionDef((Identifier *) HashItem__Data(item)))
      return (char *) HashItem__Key(item);
  }
}


/* La meme chose pour les donnees (variables globales) */
   
char * ie_data_list(init)
     int init;
{
  static HashItem * item;
  static HashItem ** entry;
  
  if (init) {
    item = 0;
    entry = 0;
  }

  for (;;) {
    HashTable_Iter(IdentifierHashTable, &item, &entry);

    if (! item)
      return 0;
    else if (! Identifier__FunctionDef((Identifier *) HashItem__Data(item)))
      return (char *) HashItem__Key(item);
  }
}

     
/* ------------------------------------------------
   
   Les fonctions d'Xcoral appelees par l'interprete
   
   ------------------------------------------------ */

Object builtin_current_position(a)		/* la position courante */
     Instruction ** a;
{
  extern FCT(int, ie_current_position,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_current_position(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_current_char(a)			/* le caractere courant */
     Instruction ** a;				/* 0 en fin de fichier  */
{
  extern FCT(char, ie_current_char,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_current_char(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_previous_char(a)			/* le caractere precedant */
     Instruction ** a;				/* 0 en debut de fichier  */
{
  extern FCT(char, ie_previous_char,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_previous_char(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_next_char(a)			/* le caractere suivant */
     Instruction ** a;				/* 0 en debut de fichier  */
{
  extern FCT(char, ie_next_char,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_next_char(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_the_char(a)	/* le caractere correspondant a la position */
     Instruction ** a;		/* 0 en cas d'erreur  */
{
  extern FCT(char, ie_the_char,(char *, int));
  Object result = Eval(*a);

  ignore_ctrl_c();
  result = ie_the_char(i_buffer_courant, result);
  accept_ctrl_c();
  
  return result;
}
Object builtin_goto_previous_char(a)		/* recule d'un caractere */
     Instruction ** a;				/* si possible */
{
  extern FCT(void, ie_goto_previous_char,(char *));

  ignore_ctrl_c();
  ie_goto_previous_char(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_goto_next_char(a)		/* avance d'un caractere */
     Instruction ** a;				/* si possible */
{
  extern FCT(void, ie_goto_next_char,(char *));

  ignore_ctrl_c();
  ie_goto_next_char(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_goto_char(a)			/* change de position */
     Instruction ** a;				/* si possible */
{
  extern FCT(void, ie_goto_char,(char *, int));
  Object arg = Eval(*a);
  
  ignore_ctrl_c();
  ie_goto_char(i_buffer_courant, arg);
  accept_ctrl_c();
  return 0;
}

Object builtin_beginning_of_line(a)	/* la position du debut de ligne */
     Instruction ** a;
{
  extern FCT(int, ie_beginning_of_line,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_beginning_of_line(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_goto_beginning_of_line(a)	/* va en debut de ligne */
     Instruction ** a;
{
  extern FCT(void, ie_goto_beginning_of_line,(char *));

  ignore_ctrl_c();
  ie_goto_beginning_of_line(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_end_of_line(a)		/* la position de la fin de ligne */
     Instruction ** a;
{
  extern FCT(int, ie_end_of_line,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_end_of_line(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_goto_end_of_line(a)		/* va enfin de ligne */
     Instruction ** a;
{
  extern FCT(void, ie_goto_end_of_line,(char *));

  ignore_ctrl_c();
  ie_goto_end_of_line(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_at_end_of_file(a)			/* fin de fichier ? */
     Instruction ** a;
{
  extern FCT(int, ie_at_end_of_file,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_at_end_of_file(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_end_of_file(a)		/* la position de la fin de fichier */
     Instruction ** a;
{
  extern FCT(int, ie_end_of_file,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_end_of_file(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_goto_end_of_file(a)		/* va a la fin du fichier */
     Instruction ** a;
{
  extern FCT(void, ie_goto_end_of_file,(char *));

  ignore_ctrl_c();
  ie_goto_end_of_file(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_goto_previous_line(a)		/* remonte d'une ligne */
    Instruction ** a;
{
  extern FCT(void, ie_goto_beginning_of_line,(char *));
  extern FCT(void, ie_goto_next_char,(char *));
  extern FCT(void, ie_goto_previous_char,(char *));
  extern FCT(int, ie_current_position,(char *));
  extern FCT(char, ie_current_char,(char *));
  int col = ie_current_position(i_buffer_courant);
  
  ignore_ctrl_c();
  ie_goto_beginning_of_line(i_buffer_courant);
  if (ie_current_position(i_buffer_courant)) {
    col -= ie_current_position(i_buffer_courant);
    ie_goto_previous_char(i_buffer_courant);
    ie_goto_beginning_of_line(i_buffer_courant);
    while (col-- && (ie_current_char(i_buffer_courant) != '\n'))
      ie_goto_next_char(i_buffer_courant);
  }
  accept_ctrl_c();
  return 0;
}

Object builtin_goto_next_line(a)		/* descend d'une ligne */
    Instruction ** a;
{
  extern FCT(void, ie_goto_beginning_of_line,(char *));
  extern FCT(void, ie_goto_end_of_line,(char *));
  extern FCT(void, ie_goto_next_char,(char *));
  extern FCT(void, ie_goto_previous_char,(char *));
  extern FCT(int, ie_current_position,(char *));
  extern FCT(char, ie_current_char,(char *));
  int col = ie_current_position(i_buffer_courant);
  
  ignore_ctrl_c();
  ie_goto_beginning_of_line(i_buffer_courant);
  col -= ie_current_position(i_buffer_courant);
  ie_goto_end_of_line(i_buffer_courant);
  ie_goto_next_char(i_buffer_courant);
  while (col-- && (ie_current_char(i_buffer_courant) != '\n'))
    ie_goto_next_char(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_goto_line(a)			/* change de ligne */
    Instruction ** a;
{
  extern FCT(void, ie_goto_beginning_of_line,(char *));
  extern FCT(void, ie_goto_end_of_line,(char *));
  extern FCT(void, ie_goto_end_of_file,(char *));
  extern FCT(void, ie_goto_next_char,(char *));
  extern FCT(void, ie_goto_previous_char,(char *));
  extern FCT(void, ie_goto_char,(char *, int));
  extern FCT(int, ie_current_position,(char *));
  extern FCT(char, ie_current_char,(char *));
  extern FCT(int, ie_line_count,(char *));
  extern FCT(int, ie_current_line,(char *));
  int nline = Eval(*a);
  
  if ((nline >= 0) && 
      (nline < ie_line_count(i_buffer_courant)) &&
      (nline != ie_current_line(i_buffer_courant))) {
    int col = ie_current_position(i_buffer_courant);
  
    ignore_ctrl_c();
    ie_goto_beginning_of_line(i_buffer_courant);
    col -= ie_current_position(i_buffer_courant);
    
    if (nline < ie_current_line(i_buffer_courant))
      if (nline < (ie_current_line(i_buffer_courant) / 2)) {
        ie_goto_char(i_buffer_courant,0);
	while (nline--) {
	  ie_goto_end_of_line(i_buffer_courant);
	  ie_goto_next_char(i_buffer_courant);
	 }
      }
      else
	for (nline = ie_current_line(i_buffer_courant) - nline; nline--;) {
	  ie_goto_previous_char(i_buffer_courant);
	  ie_goto_beginning_of_line(i_buffer_courant);
	}
    else
      if (nline > ((nline + ie_line_count(i_buffer_courant)) / 2)) {
	ie_goto_end_of_file(i_buffer_courant);
	for (nline = ie_current_line(i_buffer_courant) - nline; nline--;) {
	  ie_goto_previous_char(i_buffer_courant);
	  ie_goto_beginning_of_line(i_buffer_courant);
	}
      }
      else
	for (nline -= ie_current_line(i_buffer_courant); nline--;) {
	  ie_goto_end_of_line(i_buffer_courant);
	  ie_goto_next_char(i_buffer_courant);
        }

    /* On est en debut de ligne, retourne a la colonne initiale */
    while (col-- && (ie_current_char(i_buffer_courant) != '\n'))
      ie_goto_next_char(i_buffer_courant);

    accept_ctrl_c();
  }
  
  return 0;
}

Object builtin_current_line_to_top(a)
    Instruction ** a;
{
  extern FCT(void, ie_current_line_to_top,(char *));

  ignore_ctrl_c();
  ie_current_line_to_top(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_current_line(a)
    Instruction ** a;
{
  extern FCT(int, ie_current_line,(char *));
  Object result;
  
  ignore_ctrl_c();
  result = ie_current_line(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_line_count(a)
    Instruction ** a;
{
  extern FCT(int, ie_line_count,(char *));
  Object result;
  
  ignore_ctrl_c();
  result = ie_line_count(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_insert_char(a)			/* insere et avance */
     Instruction ** a;
{
  extern FCT(void, ie_insert_char,(char *, int));
  Object arg = Eval(*a);

  ignore_ctrl_c();
  ie_insert_char(i_buffer_courant, arg);
  accept_ctrl_c();
  return 0;
}

Object builtin_insert_string(a)			/* insere et avance */
     Instruction ** a;
{
  extern FCT(void, ie_insert_char,(char *, int));
  char * str = (char *) Eval(*a);

  ignore_ctrl_c();
#ifdef RUNTIMECHECK
  check_strlen(str);
#endif
  while (*str)
    ie_insert_char(i_buffer_courant, *str++);
  accept_ctrl_c();
  return 0;
}

Object builtin_delete_char(a)			/* detruit caract */
     Instruction ** a;
{
  extern FCT(void, ie_delete_char,(char *));

  ignore_ctrl_c();
  ie_delete_char(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_replace_char(a)		/* remplace sans avancer */
     Instruction ** a;
{
  extern FCT(void, ie_replace_char,(char *, int));
  Object arg = Eval(*a);

  ignore_ctrl_c();
  ie_replace_char(i_buffer_courant, arg);
  accept_ctrl_c();
  return 0;
}

Object builtin_global_replace(a)
    Instruction ** a;
{
  extern FCT(int, ie_global_replace,(char *, char *, char *));
  char * before = (char *) Eval(a[0]);
  char * after = (char *) Eval(a[1]);
  Object result;

#ifdef RUNTIMECHECK
  check_strlen(before);
  check_strlen(after);
#endif
  ignore_ctrl_c();
  result = ie_global_replace(i_buffer_courant, before, after);
  accept_ctrl_c();

  return result;
}

Object builtin_redisplay(a)			/* reaffiche buff */
     Instruction ** a;
{
  extern FCT(void, ie_redisplay,(char *));

  ignore_ctrl_c();
  ie_redisplay(i_buffer_courant);
  accept_ctrl_c();
  return 0;
}

Object builtin_blink(a)				/* blink le caractere sous */
     Instruction ** a;				/* la position donnee */
{						/* en argument */
  extern FCT(void, ie_blink,(char *, int));
  Object arg = Eval(*a);

  ignore_ctrl_c();
  ie_blink(i_buffer_courant, arg);
  accept_ctrl_c();
  return 0;
}

Object builtin_cmd_shell(a)			/* system */
     Instruction ** a;
{
  extern FCT(int, ie_cmd_shell,(char *, char *));
  char * expr = (char *) Eval(*a);
  Object result;

#ifdef RUNTIMECHECK
  check_strlen(expr);
#endif
  ignore_ctrl_c();
  result = ie_cmd_shell(i_buffer_courant, expr);
  accept_ctrl_c();

  return result;
}

Object builtin_cmd_shell_to_string(a)			/* system */
     Instruction ** a;
{
  extern FCT(char *, ie_shell_to_string,(char *, char *));
  char * expr = (char *) Eval(*a);
  Object result;

#ifdef RUNTIMECHECK
  check_strlen(expr);
#endif
  ignore_ctrl_c();
  expr = ie_shell_to_string(i_buffer_courant, expr);
  if (expr) {
    result = (Object) RTCMalloc(strlen(expr) + 1);
    if (result) {
      char * dest = (char *) result;
      
      while ((*dest++ = *expr++) != 0);
    }
  }
  else
    result = 0;
  accept_ctrl_c();

  return result;
}

Object builtin_msearch(a)			/* msearch(str, fin, sens)   */
						/* va sur le premier caract  */
     Instruction ** a;				/* appartenant a str qui est */
{						/* avant/apres fin           */
  extern FCT(int, ie_msearch,(char *, char *, int, int));
  char * expr = (char *) Eval(*a);
  int end = (int) Eval(a[1]);
  int sens = (int) Eval(a[2]);
  Object result;

#ifdef RUNTIMECHECK
  check_strlen(expr);
#endif
  ignore_ctrl_c();
  result = ie_msearch(i_buffer_courant, expr, end, sens);
  accept_ctrl_c();

  return result;
}


Object builtin_backward_search(a)
    Instruction ** a;
{
  extern FCT(int, ie_backward_search,(char *, char *));
  char * str = (char *) Eval(*a);
  Object result;

#ifdef RUNTIMECHECK
  check_strlen(str);
#endif
  ignore_ctrl_c();
  result = ie_backward_search(i_buffer_courant, str);
  accept_ctrl_c();

  return result;
}

Object builtin_forward_search(a)
    Instruction ** a;
{
  extern FCT(int, ie_forward_search,(char *, char *));
  char * str = (char *) Eval(*a);
  Object result;

#ifdef RUNTIMECHECK
  check_strlen(str);
#endif
  ignore_ctrl_c();
  result = ie_forward_search(i_buffer_courant, str);
  accept_ctrl_c();

  return result;
}



Object builtin_create_mode(a)
     Instruction ** a;
{
  extern FCT(void, ie_create_mode,(char *, char *));
  extern FCT(void, ie_set_mode_suffixes,(char *, char *, char *));
  extern FCT(void, ie_set_mode_font,(char *, char *, char *));
  char * nom_mode = (char *) Eval(a[0]);
  char * suffixes;
  char * police;

#ifdef RUNTIMECHECK
  check_strlen(nom_mode);
#endif
  
  if (IsaNotEnoughArg(a[1]))
    suffixes = police = 0;
  else {
    suffixes = (char *) Eval(a[1]);
#ifdef RUNTIMECHECK
    if (suffixes) check_strlen(suffixes);
#endif

    if (IsaNotEnoughArg(a[2]))
      police = 0;
    else {
      police = (char *) Eval(a[2]);
#ifdef RUNTIMECHECK
      if (police) check_strlen(police);
#endif
    }
  }

  ignore_ctrl_c();
  ie_create_mode(i_buffer_courant, nom_mode);
  if (suffixes) ie_set_mode_suffixes(i_buffer_courant, nom_mode, suffixes);
  if (police) ie_set_mode_font(i_buffer_courant, nom_mode, police);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_set_mode_suffixes(a)
     Instruction ** a;
{
  extern FCT(void, ie_set_mode_suffixes,(char *, char *, char *));
  char * nom_mode = (char *) Eval(a[0]);
  char * suffixes = (char *) Eval(a[1]);

#ifdef RUNTIMECHECK
  check_strlen(nom_mode);
  check_strlen(suffixes);
#endif

  ignore_ctrl_c();
  ie_set_mode_suffixes(i_buffer_courant, nom_mode, suffixes);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_set_mode_font(a)
     Instruction ** a;
{
  extern FCT(void, ie_set_mode_font,(char *, char *, char *));
  char * nom_mode = (char *) Eval(a[0]);
  char * font = (char *) Eval(a[1]);

#ifdef RUNTIMECHECK
  check_strlen(nom_mode);
  check_strlen(font);
#endif

  ignore_ctrl_c();
  ie_set_mode_font(i_buffer_courant, nom_mode, font);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_set_mode(a)
    Instruction ** a;
{
  extern FCT(void, ie_set_mode,(char *, char *));
  char * nom_mode = (char *) Eval(a[0]);

#ifdef RUNTIMECHECK
  check_strlen(nom_mode);
#endif

  ignore_ctrl_c();
  ie_set_mode(i_buffer_courant, nom_mode);
  accept_ctrl_c();
  
  return 0;
}


Object builtin_key_def(a)
     Instruction ** a;
{
  extern FCT(void, ie_key_def,(char *, char *, char *, char *));
  char * nom_mode = (char *) Eval(a[0]);
  char * touches = (char *) Eval(a[1]);
  char * nom_fonc = (char *) Eval(a[2]);

#ifdef RUNTIMECHECK
  check_strlen(nom_mode);
  check_strlen(touches);
  if (nom_fonc) check_strlen(nom_fonc);
#endif

  ignore_ctrl_c();
  ie_key_def(i_buffer_courant, nom_mode, touches, nom_fonc);
  accept_ctrl_c();
  
  return 0;
}


Object builtin_last_key(a)			/* retourne le dernier */
     Instruction ** a;				/* caractere tape */
{
  extern FCT(int, ie_last_key,(char *));
  Object result;
  
  ignore_ctrl_c();
  result = ie_last_key(i_buffer_courant);
  accept_ctrl_c();

  return result;
}

Object builtin_set_font(a)
     Instruction ** a;
{
  extern FCT(void, ie_set_font,(char *, char *));
  char * nom_font = (char *) Eval(*a);

#ifdef RUNTIMECHECK
  check_strlen(nom_font);
#endif

  ignore_ctrl_c();
  ie_set_font(i_buffer_courant, nom_font);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_read_file(a)
    Instruction ** a;
{
  extern FCT(int, ie_read_file,(char *, char *));
  char * filename = (char *) Eval(*a);
  Object result;
  
#ifdef RUNTIMECHECK
  check_strlen(filename);
#endif
  ignore_ctrl_c();
  result = ie_read_file(i_buffer_courant, filename);
  accept_ctrl_c();

  return result;
}

Object builtin_save_file(a)
    Instruction ** a;
{
  extern FCT(void, ie_save_file,(char *));
  
  ignore_ctrl_c();
  ie_save_file(i_buffer_courant);
  accept_ctrl_c();

  return 0;
}

Object builtin_write_file(a)
    Instruction ** a;
{
  extern FCT(void, ie_write_file,(char *, char *));
  char * filename = (char *) Eval(*a);
  
#ifdef RUNTIMECHECK
  check_strlen(filename);
#endif
  ignore_ctrl_c();
  ie_write_file(i_buffer_courant, filename);
  accept_ctrl_c();

  return 0;
}

Object builtin_current_buffer_is_modified(a)
    Instruction ** a;
{
  extern FCT(int, ie_current_buffer_is_modified,(char *));
  int result;
  
  ignore_ctrl_c();
  result = ie_current_buffer_is_modified(i_buffer_courant);
  accept_ctrl_c();

  return result;
}

Object builtin_insert_file(a)
    Instruction ** a;
{
  extern FCT(int, ie_insert_file,(char *, char *));
  char * filename = (char *) Eval(*a);
  Object result;
  
#ifdef RUNTIMECHECK
  check_strlen(filename);
#endif
  ignore_ctrl_c();
  result = ie_insert_file(i_buffer_courant, filename);
  accept_ctrl_c();

  return result;
}

Object builtin_filename(a)
    Instruction ** a;
{
  extern FCT(char *, ie_filename,(char *));
  char * filename;
  char * result;
  
  ignore_ctrl_c();
  filename = ie_filename(i_buffer_courant);
  if (filename) {
    result = (char *) RTCMalloc(strlen(filename) + 1);
    if (result) {
      char * dest = result;
      
      while ((*dest++ = *filename++) != 0);
    }
  }
  else
    result = 0;
  accept_ctrl_c();

  return (Object) result;
}

Object builtin_current_mode(a)
    Instruction ** a;
{
  extern FCT(char *, ie_current_mode,(char *));
  char * mode;
  char * result;
  
  ignore_ctrl_c();
  mode = ie_current_mode(i_buffer_courant);
  if (mode) {
    result = (char *) RTCMalloc(strlen(mode) + 1);
    if (result) {
      char * dest = result;
      
      while ((*dest++ = *mode++) != 0);
    }
  }
  else
    result = 0;
  accept_ctrl_c();

  return (Object) result;
}

Object builtin_window_height(a)
    Instruction ** a;
{
  extern FCT(int, ie_window_height,(char *));
  int result;
  
  ignore_ctrl_c();
  result = ie_window_height(i_buffer_courant);
  accept_ctrl_c();

  return result;
}

Object builtin_window_width(a)
    Instruction ** a;
{
  extern FCT(int, ie_window_width,(char *, char));
  char c = Eval(*a);
  int result;
  
  ignore_ctrl_c();
  result = ie_window_width(i_buffer_courant, c);
  accept_ctrl_c();

  return result;
}

Object builtin_file_select(a)
    Instruction ** a;
{
  extern FCT(char *, ie_file_select,(char *));
  char * filename;
  char * result;
  
  ignore_ctrl_c();
  filename = ie_file_select(i_buffer_courant);
  if (filename) {
    result = (char *) RTCMalloc(strlen(filename) + 1);
    if (result) {
      char * dest = result;

      while ((*dest++ = *filename++) != 0);
    }
  }
  else
    result = 0;
  accept_ctrl_c();

  return (Object) result;
}

Object builtin_current_window(a)
    Instruction ** a;
{
  extern FCT(int, ie_current_window,(char *));
  int result;
  
  ignore_ctrl_c();
  result = ie_current_window(i_buffer_courant);
  accept_ctrl_c();

  return result;
}

Object builtin_select_window(a)
    Instruction ** a;
{
  extern FCT(char *, ie_select_window,(char *, int));
  int win = Eval(*a);
  char * selectedwindow;
  int result;
  
  ignore_ctrl_c();
  selectedwindow = ie_select_window(i_buffer_courant, win);
  if (selectedwindow) {
    i_buffer_courant = selectedwindow;
    result = win;
  }
  else
    result = -1;
  accept_ctrl_c();

  return result;
}

Object builtin_new_window(a)
    Instruction ** a;
{
  extern FCT(int, ie_new_window,(char *));
  int result;
  
  ignore_ctrl_c();
  result = ie_new_window(i_buffer_courant);
  accept_ctrl_c();

  return result;
}

Object builtin_kill_window(a)
    Instruction ** a;
{
  extern FCT(int, ie_kill_window,(char *, int));
  int win = Eval(*a);
  
  ignore_ctrl_c();
  ie_kill_window(i_buffer_courant, win);
  accept_ctrl_c();

  return 0;
}

Object builtin_kill_current_buffer(a)
    Instruction ** a;
{
  extern FCT(int, ie_kill_current_buffer,(char *));
  
  ignore_ctrl_c();
  ie_kill_current_buffer(i_buffer_courant);
  accept_ctrl_c();

  return 0;
}

Object builtin_lower_window(a)
    Instruction ** a;
{
  extern FCT(int, ie_lower_window,(char *));
  
  ignore_ctrl_c();
  ie_lower_window(i_buffer_courant);
  accept_ctrl_c();

  return 0;
}

Object builtin_raise_window(a)
    Instruction ** a;
{
  extern FCT(int, ie_raise_window,(char *));
  
  ignore_ctrl_c();
  ie_raise_window(i_buffer_courant);
  accept_ctrl_c();

  return 0;
}

static Object builtin_wprintf(a)
     Instruction ** a;
{
  extern FCT(void, ie_insert_char,(char *, int));
  int result = 0;
  char * fmt = (char *) Eval(*a);

#ifdef RUNTIMECHECK
  check_strlen(fmt);
#endif
  
  a += 1;

  while (*fmt) {
    if (*fmt == '%')
      switch (*++fmt) {
      case 0 :
	return result;
	
      case '%' :
	ie_insert_char(i_buffer_courant, '%');
	fmt += 1;
	break;
	
      default :
	{
	  char * begin = fmt - 1;
	  char save;

	  while (! strchr("diopuxXcs", *fmt))
	    if (! *fmt++)
	      return result;

	  save = *++fmt;
	  *fmt = 0;
	  if (*(fmt - 1) == 's') {
	    char * s = (char *)  Eval(*a);

	    if (s) {
	      int strlg;
	      int lg = 0;

#ifdef RUNTIMECHECK
	      strlg = check_strlen(s);
#else
	      strlg = strlen(s);
#endif
	      sscanf(begin + 1, "%d", &lg);
	      while (lg-- > strlg)
		ie_insert_char(i_buffer_courant, ' ');
	    }
	    else
	      s = "(null)";
	    while (*s) ie_insert_char(i_buffer_courant, *s++);
	  }
	  else {
	    char str[128];
	    char * s = str;
	    
	    sprintf(s, begin, Eval(*a));
	    while (*s) ie_insert_char(i_buffer_courant, *s++);
	  }
	  result += 1;
	  *fmt = save;
	  a += 1;
	}
      }
    else
      ie_insert_char(i_buffer_courant, *fmt++);
  }
  
  return result;
}

Object builtin_display_message(a)
    Instruction ** a;
{
  extern FCT(void, DisplayWMessage ,(char *, char *, int));
  
  char * msg = (char *) Eval(a[0]);
  char * from = "";
  int flag = 0;

#ifdef RUNTIMECHECK
    check_strlen(msg);
#endif
  
  if (! IsaNotEnoughArg(a[1])) {
    from = (char *) Eval(a[1]);
#ifdef RUNTIMECHECK
    check_strlen(from);
#endif

    if (! IsaNotEnoughArg(a[2]))
      flag = (int) Eval(a[2]);
  }
  
  DisplayWMessage(msg, from, flag);
  return 0;
}

/* Choix dans une liste */

static char ** item_list;
static item_list_size;
static int n_item;

Object builtin_select_from_list(a)
    Instruction ** a;
{
  extern FCT(char *, SelectFromListBox ,(char *));
  char * str = (char *) Eval(*a);

#ifdef RUNTIMECHECK
  check_strlen(str);
#endif
  
  if (! n_item) return 0;
  
  if (! (str = SelectFromListBox(str)))
    return 0;
  
  {
    char ** p = item_list;
    
    while (
#ifdef RUNTIMECHECK
	   check_strlen(*p),
#endif
	   strcmp(*p, str))
      p += 1;
    return (Object) *p;
  }
}

Object builtin_add_list_item(a)
    Instruction ** a;
{
  extern FCT(char *, FillList ,(char *));
  char * item = (char *) Eval(*a);

#ifdef RUNTIMECHECK
  check_strlen(item);
#endif
  
  FillList(item);
  if (n_item == item_list_size) {
    if (! item_list_size) {
      if (! (item_list = (char **) calloc(10, sizeof(char *))))
	Error("No more place");
      item_list_size = 10;
    }
    else {
      char ** nl = (char **) calloc(2 * item_list_size, sizeof(char *));
    
      if (! nl)
	Error("No more place");
      memcpy((char *) nl, (char *) item_list, item_list_size * sizeof(char *));
      free(item_list);
      item_list = nl;
      item_list_size *= 2;
    }
  }
  
  item_list[n_item++] = item;
  
  return 0;
}

Object builtin_clear_list(a)
    Instruction ** a;
{
  extern FCT(char *, ClearListBox ,());
  
  ClearListBox();
  if (item_list) {
    free(item_list);
    item_list = 0;
    n_item = item_list_size = 0;
  }
  return 0;
}

/* */

Object builtin_getchar(a)
    Instruction ** a;
{
  char * prompt;
    
  extern FCT(char *, GetStringFromDB, (char * prompt, int one));
  char * str;
    
  if (IsaNotEnoughArg(*a))
    prompt = "getchar ? ";
  else {
    prompt = (char *) Eval(*a);
    
#ifdef RUNTIMECHECK
    check_strlen(prompt);
#endif
  }
  
  str = GetStringFromDB(prompt, 1);
  return (str) ? (Object) *str : (Object) 0;
}

Object builtin_gets(a)
    Instruction ** a;
{
    char * prompt;
    extern FCT(char *, GetStringFromDB, (char * prompt, int one));
    char * str;
    
  if (IsaNotEnoughArg(*a))
    prompt = "gets ? ";
  else {
    prompt = (char *) Eval(*a);
    
#ifdef RUNTIMECHECK
    check_strlen(prompt);
#endif
  }
  
  str = GetStringFromDB(prompt, 0);
  if (! str)
    return (Object) 0;
  {
    char * result = (char *) RTCMalloc(strlen(str) + 1);
    
    if (result) {
      char * dest = result;

      while ((*dest++ = *str++) != 0);
    }
    
    return (Object) result;
  }
}



/* coloriage */

Object builtin_monochrome(a)
     Instruction ** a;
{
  extern FCT(int, ie_monochrome,(char *));

  return ie_monochrome(i_buffer_courant);
}

Object builtin_remove_colors(a)
     Instruction ** a;
{
  extern FCT(void, ie_remove_colors,(char *));

  ignore_ctrl_c();
  ie_remove_colors(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_color_area(a)
     Instruction ** a;
{
  extern FCT(void, ie_color_area,(char *, int, int, char *));
  int start = (int) Eval(a[0]);
  int end = (int) Eval(a[1]);
  char * color = (char *) Eval(a[2]);

#ifdef RUNTIMECHECK
  check_strlen(color);
#endif

  ignore_ctrl_c();
  ie_color_area(i_buffer_courant, start, end, color);
  accept_ctrl_c();
  
  return 0;
}


/* region */

Object builtin_mark_position(a)
     Instruction ** a;
{
  extern FCT(int, ie_mark_position,(char *));
  Object result;

  ignore_ctrl_c();
  result = ie_mark_position(i_buffer_courant);
  accept_ctrl_c();
  
  return result;
}

Object builtin_set_mark(a)
     Instruction ** a;
{
  extern FCT(void, ie_set_mark,(char *, int));
  int pos = (int) Eval(*a);
  
  ignore_ctrl_c();
  ie_set_mark(i_buffer_courant, pos);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_goto_mark(a)
     Instruction ** a;
{
  extern FCT(void, ie_goto_mark,(char *));

  ignore_ctrl_c();
  ie_goto_mark(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_reset_mark(a)
     Instruction ** a;
{
  extern FCT(void, ie_reset_mark,(char *));

  ignore_ctrl_c();
  ie_reset_mark(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_cut_region(a)
     Instruction ** a;
{
  extern FCT(void, ie_cut_region,(char *));

  ignore_ctrl_c();
  ie_cut_region(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_copy_region(a)
     Instruction ** a;
{
  extern FCT(void, ie_copy_region,(char *));

  ignore_ctrl_c();
  ie_copy_region(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_paste_region(a)
     Instruction ** a;
{
  extern FCT(void, ie_paste_region,(char *));

  ignore_ctrl_c();
  ie_paste_region(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}


/* expression regulieres */

Object builtin_re_search_forward(a)
    Instruction ** a;
{
  extern FCT(int, ie_re_search_forward,(char*, char *regex ));
  char * regex = (char *) Eval(*a);
  int result;

#ifdef RUNTIMECHECK
  check_strlen(regex);
#endif
  ignore_ctrl_c();
  result = ie_re_search_forward(i_buffer_courant, regex);
  accept_ctrl_c();
  
  return result;
}

Object builtin_re_search_backward(a)
    Instruction ** a;
{
  extern FCT(int, ie_re_search_backward,(char*, char * ));
  char * regex = (char *) Eval(*a);
  int result;

#ifdef RUNTIMECHECK
  check_strlen(regex);
#endif
  ignore_ctrl_c();
  result = ie_re_search_backward(i_buffer_courant, regex);
  accept_ctrl_c();
  
  return result;
}

Object builtin_re_replace(a)
    Instruction ** a;
{
  char * newstring = (char *) Eval(a[0]);
  int result;
  
#ifdef RUNTIMECHECK
  check_strlen(newstring);
#endif
  if (IsaNotEnoughArg(a[1])) {
    extern FCT(int, ie_re_replace_match, (char *, char *));
    
    ignore_ctrl_c();
    result = ie_re_replace_match(i_buffer_courant, newstring);
  }
  else {
    extern FCT(int, ie_re_replace, (char *, char *, char*));
    char * regex = (char *) Eval(a[1]);
    
#ifdef RUNTIMECHECK
    check_strlen(regex);
#endif
    ignore_ctrl_c();
    result = ie_re_replace(i_buffer_courant, regex, newstring);
  }
  accept_ctrl_c();
  
  return result;
}

Object builtin_re_match_beginning(a)
    Instruction ** a;
{
  extern FCT(int, ie_re_match_beginning,(char *, int));
  int n = (int) Eval(*a);
  int result;
  
  ignore_ctrl_c();
  result = ie_re_match_beginning(i_buffer_courant, n);
  accept_ctrl_c();
  
  return result;
}

Object builtin_re_match_end(a)
    Instruction ** a;
{
  extern FCT(int, ie_re_match_end,(char *, int));
  int n = (int) Eval(*a);
  int result;
  
  ignore_ctrl_c();
  result = ie_re_match_end(i_buffer_courant, n);
  accept_ctrl_c();
  
  return result;
}

/* la montre*/

Object builtin_watch_on(a)
    Instruction ** a;
{
  extern FCT(int, ie_watch_on,(char *));
  
  ignore_ctrl_c();
  ie_watch_on(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}

Object builtin_watch_off(a)
    Instruction ** a;
{
  extern FCT(int, ie_watch_off,(char *));
  
  ignore_ctrl_c();
  ie_watch_off(i_buffer_courant);
  accept_ctrl_c();
  
  return 0;
}

/* La fonction usleep */
Object builtin_usleep(a)
    Instruction ** a;
{
  extern FCT(int, ie_usleep,(char *, int));
  int t = (int) Eval(*a);
  
  ignore_ctrl_c();
  ie_usleep(i_buffer_courant, t);
  accept_ctrl_c();
  
  return 0;
}


/* */

void Init_Smac_Xcoral()
{
  static Type * string_int_int[3];
  static Type * string_string_string[3];
  static Type * string_string_int[3];
  static Type * int_int_string[3];
    
  string_string_int[0] = string_string_int[1] = Type_String;
  string_string_int[2] = Type_Int;
  
  string_string_string[0] =
      string_string_string[1] =
	  string_string_string[2] = Type_String;
    
  string_int_int[0] = Type_String;
  string_int_int[1] = string_int_int[2] = Type_Int;
  
  int_int_string[0] = int_int_string[1] = Type_Int;
  int_int_string[2] = Type_String;
    
  new_builtin("current_position", Type_Int, 0, 0, builtin_current_position);
  new_builtin("current_char", Type_Char, 0, 0, builtin_current_char);
  new_builtin("previous_char", Type_Char, 0, 0, builtin_previous_char);
  new_builtin("next_char", Type_Char, 0, 0, builtin_next_char);
  new_builtin("the_char", Type_Char, 1, &Type_Int, builtin_the_char);
  new_builtin("goto_previous_char", Type_Void, 0, 0, builtin_goto_previous_char);
  new_builtin("goto_next_char", Type_Void, 0, 0, builtin_goto_next_char);
  new_builtin("goto_char", Type_Void, 1, &Type_Int, builtin_goto_char);
  new_builtin("beginning_of_line", Type_Int, 0, 0, builtin_beginning_of_line);
  new_builtin("goto_beginning_of_line", Type_Void, 0, 0, builtin_goto_beginning_of_line);
  new_builtin("end_of_line", Type_Int, 0, 0, builtin_end_of_line);
  new_builtin("goto_end_of_line", Type_Void, 0, 0, builtin_goto_end_of_line);
  new_builtin("at_end_of_file", Type_Int, 0, 0, builtin_at_end_of_file);
  new_builtin("end_of_file", Type_Int, 0, 0, builtin_end_of_file);
  new_builtin("goto_end_of_file", Type_Void, 0, 0, builtin_goto_end_of_file);
  new_builtin("goto_previous_line", Type_Void, 0, 0, builtin_goto_previous_line);
  new_builtin("goto_next_line", Type_Void, 0, 0, builtin_goto_next_line);
  new_builtin("goto_line", Type_Void, 1, &Type_Int, builtin_goto_line);
  new_builtin("current_line_to_top", Type_Void, 0, 0, builtin_current_line_to_top);
  new_builtin("line_count", Type_Int, 0, 0, builtin_line_count);
  new_builtin("current_line", Type_Int, 0, 0, builtin_current_line);
  new_builtin("insert_char", Type_Void, 1, &Type_Char, builtin_insert_char);
  new_builtin("insert_string", Type_Void, 1, &Type_String, builtin_insert_string);
  new_builtin("delete_char", Type_Void, 0, 0, builtin_delete_char);
  new_builtin("replace_char", Type_Void, 1, &Type_Char, builtin_replace_char);
  new_builtin("redisplay", Type_Void, 0, 0, builtin_redisplay);
  new_builtin("blink", Type_Void, 1, &Type_Int, builtin_blink);
  
  new_builtin("cmd_shell", Type_Int, 1, &Type_String, builtin_cmd_shell);
  new_builtin("cmd_shell_to_string", Type_String, 1, &Type_String,
	      builtin_cmd_shell_to_string);

  new_builtin("msearch", Type_Int, 3, string_int_int, builtin_msearch);
  new_builtin("backward_search", Type_Int, 1, &Type_String,
	      builtin_backward_search);
  new_builtin("forward_search", Type_Int, 1, &Type_String,
	      builtin_forward_search);
  new_builtin("global_replace", Type_Int, 2, string_string_string,
	      builtin_global_replace);

  HashTable__Add(builtin_fct_nbre_arg_min,
		 new_builtin("create_mode", Type_Void,
			     3, string_string_string,
			     builtin_create_mode),
		 1 + 1);
  new_builtin("set_mode_suffixes", Type_Void, 2, string_string_string,
	      builtin_set_mode_suffixes);
  new_builtin("set_mode_font", Type_Void, 2, string_string_string,
	      builtin_set_mode_font);
  new_builtin("key_def", Type_Void, 3, string_string_string,
	      builtin_key_def);
  new_builtin("set_mode", Type_Void, 1, &Type_String,
	      builtin_set_mode);

  new_builtin("last_key", Type_Int, 0, 0, builtin_last_key);

  new_builtin("set_font", Type_Void, 1, &Type_String, builtin_set_font);
  
  new_builtin("read_file", Type_Int, 1, &Type_String, builtin_read_file);
  new_builtin("save_file", Type_Void, 0, 0, builtin_save_file);
  new_builtin("write_file", Type_Void, 1, &Type_String, builtin_write_file);
  new_builtin("filename", Type_String, 0, 0, builtin_filename);
  new_builtin("insert_file", Type_Int, 1, &Type_String, builtin_insert_file);
  new_builtin("window_height", Type_Int, 0, 0, builtin_window_height);
  new_builtin("window_width", Type_Int, 1, &Type_Char, builtin_window_width);
  new_builtin("file_select", Type_String, 0, 0, builtin_file_select);
  new_builtin("current_window", Type_Int, 0, 0, builtin_current_window);
  new_builtin("select_window", Type_Int, 1, &Type_Int, builtin_select_window);
  new_builtin("new_window", Type_Int, 0, 0, builtin_new_window);
  new_builtin("kill_window", Type_Void, 1, &Type_Int, builtin_kill_window);
  new_builtin("kill_current_buffer", Type_Void, 0, 0, builtin_kill_current_buffer);
  new_builtin("lower_window", Type_Void, 0, 0, builtin_lower_window);
  new_builtin("raise_window", Type_Void, 0, 0, builtin_raise_window);
  new_builtin("current_buffer_is_modified", Type_Int, 0, 0,
	      builtin_current_buffer_is_modified);

  HashTable__Add(builtin_fct_nbre_arg_min,
		 new_builtin("wprintf", Type_Int, 1, &Type_String,
			     builtin_wprintf),
		 1 + 1);
  HashTable__Add(builtin_fct_nbre_arg_min,
		 new_builtin("display_message", Type_Void, 3,
			     string_string_int, builtin_display_message),
		 1 + 1);

  new_builtin("add_list_item", Type_Void, 1, &Type_String,
	      builtin_add_list_item);
  new_builtin("clear_list", Type_Void, 0, 0, builtin_clear_list);
  new_builtin("select_from_list", Type_String, 1, &Type_String,
	      builtin_select_from_list);
  HashTable__Add(builtin_fct_nbre_arg_min,
		 new_builtin("getchar", Type_Int, 1, &Type_String,
			     builtin_getchar),
		 0 + 1);
  HashTable__Add(builtin_fct_nbre_arg_min,
		 new_builtin("gets", Type_String, 1, &Type_String,
			     builtin_gets),
		 0 + 1);

  new_builtin("monochrome", Type_Int, 0, 0, builtin_monochrome);
  new_builtin("remove_colors", Type_Void, 0, 0, builtin_remove_colors);
  new_builtin("color_area", Type_Void, 3, int_int_string,
	      builtin_color_area);
  
  new_builtin("mark_position", Type_Int, 0, 0, builtin_mark_position);
  new_builtin("cut_region", Type_Void, 0, 0, builtin_cut_region);
  new_builtin("copy_region", Type_Void, 0, 0, builtin_copy_region);
  new_builtin("paste_region", Type_Void, 0, 0, builtin_paste_region);
  new_builtin("set_mark", Type_Void, 1, &Type_Int, builtin_set_mark);
  new_builtin("goto_mark", Type_Void, 0, 0, builtin_goto_mark);
  new_builtin("reset_mark", Type_Void, 0, 0, builtin_reset_mark);
  
  new_builtin("re_forward_search", Type_Int, 1, &Type_String,
	      builtin_re_search_forward);
  new_builtin("re_backward_search", Type_Int, 1, &Type_String,
	      builtin_re_search_backward);
  HashTable__Add(builtin_fct_nbre_arg_min,
		 new_builtin("re_replace", Type_Int, 2, string_string_string,
			     builtin_re_replace),
		 1 + 1);
  new_builtin("re_match_beginning", Type_Int, 1, &Type_Int,
	      builtin_re_match_beginning);
  new_builtin("re_match_end", Type_Int, 1, &Type_Int, builtin_re_match_end);
  
  new_builtin("watch_on", Type_Void, 0, 0, builtin_watch_on);
  new_builtin("watch_off", Type_Void, 0, 0, builtin_watch_off);
  new_builtin("usleep", Type_Void, 1, &Type_Int, builtin_usleep);
  new_builtin("current_mode", Type_String, 0, 0, builtin_current_mode);
}


