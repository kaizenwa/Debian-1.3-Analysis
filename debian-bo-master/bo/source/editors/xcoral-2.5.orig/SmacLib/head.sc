/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: head.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/head.sc
   Description: 
   Created: Sun Jul 31 12:04:39 DST
   Author: Bruno Pages
   Modified: Sun Jul 31 12:04:40 DST
   Last maintained by: Lionel Fournigault

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

/* Define a new class.
   Parent classes names must be separated by `,` or spaces */

void class_header()
{
  char * classname = gets("Class name ? ");
  char * parent;
  
  if (! classname)
    return;
  if  (*classname == 7) {
    free(classname);
    return;
  }
  
  parent = gets("Parent classes names ? ");

  wprintf("//\n//\tClass name : %s\n//\n//\tDescription :\n//\n",
	  classname);
  wprintf("class %s", classname);
  if (parent && (*parent != 7)) {
    char * sep = " : ";
    char * first = parent;
    char * p;
    
    while (p = strtok(first, " ,")) {
      wprintf("%spublic %s", sep, p);
      first = 0;
      sep = ", ";
    }
  }
  wprintf(" {\n");
  wprintf("  public:\n\t%s();\n\t%s(const %s &);\n\t~%s();\n\t\n",
	  classname, classname, classname, classname);
  wprintf("  protected:\n  private:\n};\n\n");
  goto_line(current_line() - 5);
  goto_end_of_line();
  
  free(classname);
  if (parent) free(parent);
}


/* A new method */
   
void method_header()
{
  char * methodname = gets("Method name ? ");
  char * classname;
  
  if (! methodname)
    return;
  if  (*methodname == 7) {
    free(methodname);
    return;
  }
  
  classname = gets("Class name ? ");
  
  if (! classname) {
    free(methodname);
    return;
  }
  if (*classname == 7) {
    free(methodname);
    free(classname);
    return;
  }
  
  wprintf("//\n//\tMethod name : %s\n//\n//\tDescription :\n//\tInput :\n//\tOutput :\n//\n",
	  methodname);
  wprintf("%s::%s()\n{\n  \n}\n\n", classname, methodname);
  goto_line(current_line() - 3);
  goto_end_of_line();
  free(classname);
  free(methodname);
}


/* A new function */

void function_header()
{
  char * functionname = gets("Function name ? ");
  
  if (! functionname)
    return;
  if  (*functionname == 7) {
    free(functionname);
    return;
  }
  
  wprintf("/*\n**\tFunction name : %s\n**\n**\tDescription :\n**\tInput :\n**\tOutput :\n*/\n",
	  functionname);
  wprintf("%s()\n{\n  \n}\n\n", functionname);
  goto_line(current_line() - 3);
  goto_end_of_line();
  free(functionname);
}


/* File header, a suffixe <> .h => a c file */

void include_header()
{
  char * filepath = filename();
  char * name;
  char * ext;
  
  if (! filepath) return;
  
  name = strrchr(filepath, '/');
  ext = strrchr(filepath, '.');
  
  if (! name)
    name = filepath;
  else
    name += 1;
  
  if (ext) {
    *ext++ = 0;
    if (*ext == 'h') {
      wprintf("#ifndef _%s_h\n#define _%s_h\n\n\n\n#endif /* _%s_h */\n",
	      name, name, name);
      goto_line(current_line() - 2);
    }
    else
      wprintf("#include \"%s.h\"\n", name);
  }
  
  free(filepath);
}


