/* Program fixassy-68k.c:

   Copyright (C) 1993, Eric Youngdale.

   Rewritten for m68k by Andreas Schwab.

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This portion of the program is responsible for rewriting the
   operands to machine instructions to add an indirection for all of
   the global variables listed in the input file jump.vars.  New
   instructions are added in order to move the data around as
   required, and a register is saved and restored if needed so that
   nothing gets screwed up.  */

/* On the m68k we have more freedom in adding the indirection by using
   the memory indirect addressing mode.  The compiler generates only a
   subset of the possible addressing modes, and this code assumes that
   inline assembler is similarily restrictive, otherwise you will get
   some error messages.  In 99% of the cases we don't need to add any
   instructions for saving and restoring registers.  */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "utils.h"


FILE *infile;

char buffer[16384];
char tbuff[1024];
const char *const regs[] = { "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7" };
#define isareg(token) ((token) >= 'h' && (token) <= 'o')
#define isreg(token) (((token) >= '0' && (token) <= '7') || isareg (token))

int moved = 0;
LINE *fpnt;
static FILE *foo;
extern char *outname;

#ifdef DEBUG_IMPLIED
void send_log (char type, char *name);
#endif

void rewrite_instruction (struct instruction ins, LINE *wpnt);
void print_instruction ();

/* Insert one line of text into the text file at the current position.  */
void 
insert_text (char *cpnt)
{
  LINE *pnt = NULL;

  pnt = xmalloc (sizeof (LINE));
  pnt->next = fpnt->next;
  pnt->text = fpnt->text;
  fpnt->next = pnt;
  fpnt->text = xstrdup (cpnt);
  if (foo)
    fprintf (foo, "Rplc:%s", cpnt);
  fpnt = fpnt->next;
}

void 
insert_text_trailing (char *cpnt)
{
  LINE *pnt = NULL;

  pnt = xmalloc (sizeof (LINE));
  pnt->next = fpnt->next;
  pnt->text = xstrdup (cpnt);
  if (foo)
    fprintf (foo, "(trailing)Rplc:%s", cpnt);
  fpnt->next = pnt;
}

/* This function actually does the fixup work on the operands.  It is
   assumed that the instruction and all of it's operands have all been
   parsed into the ins structure, and each token in each operand has
   also been properly classified so that it is relatively easy to
   figure out what we need to do.  */

void 
fixup ()
{
  int usereg;
  int i;
  int is_cmp, usereg2;

  /* First we rewrite some special cases. */

  /*
   * lea G,an     -->  movel __GOT_G,an
   * lea G+/-N,an -->  movel __GOT_G,an; addl/subl #N,an
   */

  if (strncmp (ins.opcode, "lea", 3) == 0)
    {
      strcpy (ins.opcode, "movel ");
      sprintf (tbuff, "__GOT_%s", ins.ops[0].strings[0]);
      xfree (ins.ops[0].strings[0]);
      ins.ops[0].strings[0] = xstrdup (tbuff);
      if (ins.ops[0].ntokens > 1)
	{
	  unsigned long offset;
	  if (ins.ops[0].ntokens != 3
	      || (ins.ops[0].token[1] != '+' && ins.ops[0].token[1] != '-')
	      || ins.ops[0].token[2] != '#')
	    {
	      error ("Constant offset expected in `%s'\n", fpnt->text);
	      exit (1);
	    }
	  /* If the constant fits into a word, use addw/subw. */
	  offset = strtoul (ins.ops[0].strings[2], 0, 0);
	  sprintf (tbuff, "\t%s%s #%s,%s\n",
		   ins.ops[0].token[1] == '+' ? "add" : "sub",
		   offset < 32768 ? "w" : "l",
		   ins.ops[0].strings[2], ins.ops[1].strings[0]);
	  insert_text_trailing (tbuff);
	}
      for (i = 1; i < ins.ops[0].ntokens; i++)
	{
	  xfree (ins.ops[0].strings[i]);
	  ins.ops[0].strings[i] = 0;
	}
      print_instruction ();
      return;
    }

  /*
   * pea G        -->  movel __GOT_G,sp@-
   * pea G+/-N    -->  movel __GOT_G,sp@-; addl/subl #N,sp@
   */

  if (strncmp (ins.opcode, "pea", 3) == 0)
    {
      strcpy (ins.opcode, "movel ");
      sprintf (tbuff, "__GOT_%s", ins.ops[0].strings[0]);
      xfree (ins.ops[0].strings[0]);
      ins.ops[0].strings[0] = xstrdup (tbuff);
      ins.ops[1].strings[0] = xstrdup ("sp@-");
      ins.ops[1].strings[1] = NULL;
      ins.operands = 2;
      if (ins.ops[0].ntokens > 1)
	{
	  if (ins.ops[0].ntokens != 3
	      || (ins.ops[0].token[1] != '+' && ins.ops[0].token[1] != '-')
	      || ins.ops[0].token[2] != '#')
	    {
	      error ("Constant offset expected in `%s'\n", fpnt->text);
	      exit (1);
	    }
	  sprintf (tbuff, "\t%sl #%s,sp@\n",
		   ins.ops[0].token[1] == '+' ? "add" : "sub",
		   ins.ops[0].strings[2]);
	  insert_text_trailing (tbuff);
	}
      for (i = 1; i < ins.ops[0].ntokens; i++)
	{
	  xfree (ins.ops[0].strings[i]);
	  ins.ops[0].strings[i] = 0;
	}
      print_instruction ();
      return;
    }

  /* Now change absolute into memory indirect addressing mode.  Change
     label G into `@(__GOT_G)@(N)', where N is the optional offset.
     The second pair of parens is always needed, even if N is not
     present.  */

  for (i = 0; i < ins.operands; i++)
    {
      if (ins.ops[i].needs_fix
	  /* Immediate operands are handled specially later */
	  && ins.ops[i].strings[0][0] != '#')
	{
	  sprintf (tbuff, "@(__GOT_%s)@(", ins.ops[i].strings[0]);
	  if (ins.ops[i].ntokens > 1 && ins.ops[i].token[1] != '{')
	    {
	      if ((ins.ops[i].ntokens != 3 && ins.ops[i].token[3] != '{')
		  || (ins.ops[i].token[1] != '+' && ins.ops[i].token[1] != '-')
		  || ins.ops[i].token[2] != '#')
		{
		  error ("Constant offset expected in `%s'\n", fpnt->text);
		  exit (1);
		}
	      strcat (tbuff, ins.ops[i].strings[1]); /* "+" or "-" */
	      strcat (tbuff, ins.ops[i].strings[2]);
	      xfree (ins.ops[i].strings[1]);
	      xfree (ins.ops[i].strings[2]);
	      ins.ops[i].strings[1] = xstrdup ("");
	      ins.ops[i].strings[2] = xstrdup ("");
	    }
	  strcat (tbuff, ")");
	  ins.ops[i].strings[0] = xstrdup (tbuff);
	  ins.ops[i].needs_fix = 0;
	}
    }

  /* Handle the remaining cases where the first operand is immediate
     and needs fixing.  There are always atleast two operands.  */

  if (ins.ops[0].needs_fix)
    {
      if (ins.operands < 2)
	{
	  error ("Unrecognized insn `%s'\n", fpnt->text);
	  exit (1);
	}

      /* First look for the easy cases:

	 movel #G,EA
	 INST #G,REG

	 They get rewritten into movel/INST __GOT_G,EA. */

      if (ins.ops[0].ntokens == 1
	  && (strncmp (ins.opcode, "move", 4) == 0
	      || (ins.ops[1].ntokens == 1 && isreg (ins.ops[1].token[0]))))
	{
	  sprintf (tbuff, "__GOT_%s", ins.ops[0].strings[0] + 1);
	  xfree (ins.ops[0].strings[0]);
	  ins.ops[0].strings[0] = xstrdup (tbuff);
	  print_instruction ();
	  return;
	}

      /* These cases are more complicated:

	 movel #G+/-N,EA
	 addl #G+/-N,REG
	 subl #G+/-N,REG

	 Rewrite them into

	 INST __GOT_G,EA; addl/subl #N,EA

	 where we have to watch out for pre-decr and post-incr with
	 move. Note that the condition codes are preserved. */

      if (ins.ops[0].ntokens > 1
	  && (strncmp (ins.opcode, "move", 4) == 0
	      || (ins.ops[1].ntokens == 1 && isreg (ins.ops[1].token[0])
		  && (strncmp (ins.opcode, "add", 3) == 0
		      || strncmp (ins.opcode, "sub", 3) == 0))))
	{
	  if (ins.ops[0].ntokens != 3
	      || (ins.ops[0].token[1] != '+' && ins.ops[0].token[1] != '-')
	      || ins.ops[0].token[2] != '#')
	    {
	      error ("Constant offset expected in `%s'\n", fpnt->text);
	      exit (1);
	    }

	  sprintf (tbuff, "__GOT_%s", ins.ops[0].strings[0] + 1);
	  xfree (ins.ops[0].strings[0]);
	  ins.ops[0].strings[0] = xstrdup (tbuff);

	  if (ins.ops[0].ntokens > 1)
	    {
	      int is_pre_decr = 0, is_post_incr = 0;
	      char *insn, *size;

	      if (ins.ops[0].ntokens != 3
		  || (ins.ops[0].token[1] != '+' && ins.ops[0].token[1] != '-')
		  || ins.ops[0].token[2] != '#')
		{
		  error ("Constant offset expected in `%s'\n", fpnt->text);
		  exit (1);
		}
	      if (ins.ops[1].ntokens == 3
		  && ins.ops[1].token[0] >= 'h' && ins.ops[1].token[0] <= 'o'
		  && ins.ops[1].token[1] == '@')
		{
		  /* AREG@+ or AREG@- */
		  if (ins.ops[1].token[2] == '+')
		    {
		      is_post_incr = 1;
		      /* Rewrite into AREG@ */
		      ins.ops[1].ntokens--;
		      xfree (ins.ops[1].strings[2]);
		      ins.ops[1].strings[2] = 0;
		    }
		  else if (ins.ops[1].token[2] == '-')
		    is_pre_decr = 1;
		}

	      /* Now construct the add/sub instruction */
	      if (ins.ops[0].token[1] == '+'
		  && strncmp (ins.opcode, "sub", 3) != 0)
		insn = "add";
	      else
		insn = "sub";
	      /* If the destination is an address register and the
                 offset fits into a word, use addw/subw. */
	      if (ins.ops[1].ntokens == 1 && isareg (ins.ops[1].token[0]))
		{
		  unsigned long offset = strtoul (ins.ops[0].strings[2], 0, 0);
		  if (offset < 32768)
		    size = "w";
		  else
		    size = "l";
		}
	      else
		size = "l";

	      sprintf (tbuff, "\t%s%s #%s,", insn, size, ins.ops[0].strings[2]);
	      for (i = 0; i < ins.ops[1].ntokens; i++)
		strcat (tbuff, ins.ops[1].strings[i]);
	      if (is_post_incr)
		{
		  /* Add '+' again */
		  strcat (tbuff, "+");
		}
	      else if (is_pre_decr)
		{
		  /* Remove final '-' */
		  tbuff[strlen (tbuff) - 1] = 0;
		}
	      strcat (tbuff, "\n");
	      insert_text_trailing (tbuff);
	    }
	  for (i = 1; i < ins.ops[0].ntokens; i++)
	    {
	      xfree (ins.ops[0].strings[i]);
	      ins.ops[0].strings[i] = 0;
	    }
	  print_instruction ();
	  return;
	}

      /* Now the real hard ones remain. These are:

	 INST #G,EA
	 INST #G+/-N,EA

	 where INST is one of {or,and,sub,add,eor,cmp} (other cases
	 are not possible).  We have to rewrite this into:

	 movel REG,sp@-
	 movel __GOT_G,REG
	 addl/subl #N,REG		if N present
	 INST REG,EA
	 moveml sp@+,REG

	 where REG is a data register not used in EA. */

      /* There is a real nasty case, though: cmp does not come with
	 REG,EA. We must reload the second argument into another
	 register, which must be saved as well. */

      is_cmp = strncmp (ins.opcode, "cmp", 3) == 0;

      /* The first step in a fixup is to figure out what registers are
	 available to be used to move numbers around.  In general, we
	 do not want to use registers that are already in use for this
	 instruction.  */

      for (usereg = 0; usereg < 8; usereg++)
	if ((ins.ops[1].regused & (1 << usereg)) == 0)
	  break;

      if (is_cmp)
	{
	  /* There is no need to search for an unused register, just
	     use d0 or d1. */
	  usereg2 = usereg == 0;
	}

      if (is_cmp)
	sprintf (tbuff, "\tmoveml %s/%s,", regs[usereg], regs[usereg2]);
      else
	sprintf (tbuff, "\tmovel %s,", regs[usereg]);
      if (ins.uses_stack)
	{
	  /* I don't think gcc ever generates such an instruction which
	     uses the stack, at least it doesn't do for any libc module.
	     Some inline assembler code could use it, but it should be
	     avoided as much as possible since it defeats reentrancy.  */
	  strcat (tbuff, "__REG_SAVE__\n");
	  if (moved < 2)
	    moved = is_cmp ? 2 : 1;
	}
      else
	strcat (tbuff, "sp@-\n");
      insert_text (tbuff);
      sprintf (tbuff, "\tmovel __GOT_%s,%s\n", ins.ops[0].strings[0] + 1,
	       regs[usereg]);
      insert_text (tbuff);

      if (ins.ops[0].ntokens > 1)
	{
	  if (ins.ops[0].ntokens != 3
	      || (ins.ops[0].token[1] != '+' && ins.ops[0].token[1] != '-')
	      || ins.ops[0].token[2] != '#')
	    {
	      error ("Constant offset expected in `%s'\n", fpnt->text);
	      exit (1);
	    }
	  sprintf (tbuff, "\t%sl #%s,%s\n",
		   ins.ops[0].token[1] == '+' ? "add" : "sub",
		   ins.ops[0].strings[2], regs[usereg]);
	  insert_text (tbuff);
	}

      xfree (ins.ops[0].strings[0]);
      ins.ops[0].strings[0] = xstrdup (regs[usereg]);

      for (i = 1; i < ins.ops[0].ntokens; i++)
	{
	  xfree (ins.ops[0].strings[i]);
	  ins.ops[0].strings[i] = 0;
	}

      if (is_cmp)
	{
	  /* Reload the second argument */
	  strcpy (tbuff, "\tmovel ");
	  for (i = 0; i < ins.ops[1].ntokens; i++)
	    strcat (tbuff, ins.ops[1].strings[i]);
	  sprintf (tbuff + strlen (tbuff), ",%s\n", regs[usereg2]);
	  insert_text (tbuff);

	  xfree (ins.ops[1].strings[0]);
	  ins.ops[1].strings[0] = xstrdup (regs[usereg2]);

	  for (i = 1; i < ins.ops[0].ntokens; i++)
	    {
	      xfree (ins.ops[0].strings[i]);
	      ins.ops[0].strings[i] = 0;
	    }
	}

      /* Must use movem to preserve flags.  */
      if (ins.uses_stack)
	strcpy (tbuff, "\tmoveml __REG_SAVE__,");
      else
	strcpy (tbuff, "\tmoveml sp@+,");
      if (is_cmp)
	sprintf (tbuff + strlen (tbuff), "%s/%s\n",
		 regs[usereg], regs[usereg2]);
      else
	sprintf (tbuff + strlen (tbuff), "%s\n", regs[usereg]);
      insert_text_trailing (tbuff);
    }

  print_instruction ();
}

/* This function is responsible for identifying the type of each token
   in each operand.  Many tokens represent single characters, like
   '(', '+', etc.  The only non-single character tokens are numbers,
   symbols and register names.  We need to record which registers are
   in use for this instruction because we may need to use a register
   to add the indirection, and we must not use one that is already in
   use in the instruction.  */
/* Tokens used: 01234567hijklmno: registers, G#g */
int 
classify_operands ()
{
  char *pnt;
  int i, j, k;
  GLOBAL *gpnt;
  struct symbol *spnt;
  char c;
  for (i = 0; i < ins.operands; i++)
    {
      for (j = 0; j < 30; j++)
	{
	  pnt = ins.ops[i].strings[j];
	  if (!pnt)
	    break;
	  if (ins.ops[i].token[j])
	    continue;
	  k = 0;
	  while (pnt[k] == ' ' || pnt[k] == '\t')
	    k++;
	  if (pnt[k + 2] == 0)
	    {
	      c = 0;
	      /* length 2, register ? */
	      if (pnt[k] == 'd' && pnt[k + 1] >= '0' && pnt[k + 1] <= '7')
		c = pnt[k + 1];
	      else if (pnt[k] == 'a' && pnt[k + 1] >= '0' && pnt[k + 1] <= '7')
		c = pnt[k + 1] - '0' + 'h';
	      else if (pnt[k] == 's' && pnt[k + 1] == 'p')
		c = 'o';	/* stack pointer */
	      else if (pnt[k] == 'f' && pnt[k + 1] == 'p')
		c = 'n';	/* frame pointer */
	      if (c)
		{
		  ins.ops[i].token[j] = c;
		  if (c >= '0' && c <= '7')		/* d0-d7 */
		    ins.ops[i].regused |= 1 << (c - '0');
		  else if (c >= 'j' && c <= 'm')	/* a0-a5 */
		    ins.ops[i].regused |= 1 << (c - 'h' + 8);
		  if (c == 'o')
		    ins.uses_stack = 1;
		  continue;
		}
	    }
	  /* Now we have to decide if this is an integer constant or a
             symbol */
	  k = 0;
	  if (pnt[0] == '#')
	    k++;
	  if (pnt[k] == 'L')
	    {			/* Local symbol */
	      ins.ops[i].token[j] = pnt[k];
	      continue;
	    }
	  while (pnt[k] == ' ' || pnt[k] == '\t')
	    k++;
	  if (pnt[k] == '-' && (!k || pnt[k - 1] == '#'))
	    k++;

	  if (pnt[k] == '0' && pnt[k + 1] == 'x')
	    {
	      k += 2;
	      while (pnt[k]
		     && ((pnt[k] >= '0' && pnt[k] <= '9')
			 || (pnt[k] >= 'A' && pnt[k] <= 'F')
			 || (pnt[k] >= 'a' && pnt[k] <= 'f')))
		k++;
	    }
	  else
	    while (pnt[k] && pnt[k] >= '0' && pnt[k] <= '9')
	      k++;

	  while (pnt[k] == ' ' || pnt[k] == '\t')
	    k++;

	  if (pnt[k])
	    ins.ops[i].token[j] = 'G';
	  else
	    ins.ops[i].token[j] = '#';

	  k = 0;
	  if (pnt[0] == '#')
	    k++;

	  /* OK, we have a symbol.  We need to see if it is global.
	     The first step is to see if this symbol is declared
	     global in this file.  If not, then who cares what the
	     global definition for this symbol is, because we have a
	     static variant here that has nothing to do with the
	     global version */

	  if (ins.ops[i].token[j] == 'G')
	    {
	      spnt = shead;
	      while (spnt)
		{
		  if (strcmp (&pnt[k], spnt->name) == 0)
		    break;
		  spnt = spnt->next;
		}
	      if (spnt)
		ins.ops[i].token[j] = 'g';	/* No need for indirection */
	    }

	  /* Call instructions do not need to be routed through the
	     GOT, because we assume that these go through the jump
	     table.  */

	  if (ins.ops[i].token[j] == 'G'
	      && strncmp (ins.opcode, "jbsr", 4) == 0
	      && ins.operands == 1 && ins.ops[i].ntokens == 1)
	    {
	      ins.ops[i].token[j] = 'g';

	      /* Time for a sanity check here.  Make sure that this
	         symbol is not listed in jump.vars. */
	      gpnt = gdhead;
	      while (gpnt)
		{
		  if (strcmp (&pnt[k], gpnt->name) == 0)
		    break;
		  gpnt = gpnt->next;
		}
	      if (gpnt)
		error ("Function %s incorrectly listed in jump.vars\n",
		       gpnt->name);
	    }

	  /* Branch instructions do not need to go through the GOT.
	     Fortunately, they all start with the letter 'j' */

	  if (ins.ops[i].token[j] == 'G' && ins.opcode[0] == 'j'
	      && ins.operands == 1 && ins.ops[i].ntokens == 1)
	    ins.ops[i].token[j] = 'g';

	  if (ins.ops[i].token[j] == 'G')
	    {
	      gpnt = gdhead;	/* Explicitly exported variable list */
	      while (gpnt)
		{
		  if (strcmp (&pnt[k], gpnt->name) == 0)
		    break;
		  gpnt = gpnt->next;
		}
	      if (!gpnt)
		{
		  /* OK, we are not exporting this symbol.  See if we
		     are importing it.  If not, we do not need to add
		     the indirection */
#ifdef IMPLIED_IMPORT
		  gpnt = gfhead;	/* Global function list */
		  while (gpnt)
		    {
		      if (strcmp (&pnt[k], gpnt->name) == 0)
			break;
		      gpnt = gpnt->next;
		    }

		  if (!gpnt)
		    {
		      gpnt = gihead;	/* Ignore list */
		      while (gpnt)
			{
			  if (strcmp (&pnt[k], gpnt->name) == 0)
			    break;
			  gpnt = gpnt->next;
			}
		    }
		  if (gpnt)
		    ins.ops[i].token[j] = 'g';	/* No need for indirection */
#ifdef DEBUG_IMPLIED
		  if (!gpnt)
		    send_log ('i', &pnt[k]);
#endif
#else
		  gpnt = gphead;	/* Explicit import */
		  while (gpnt)
		    {
		      if (strcmp (&pnt[k], gpnt->name) == 0)
			break;
		      gpnt = gpnt->next;
		    }
		  if (!gpnt)
		    ins.ops[i].token[j] = 'g';	/* No need for indirection */
#endif
		}
	    }

	  if (ins.ops[i].token[j] == 'G')
	    {
	      ins.ops[i].needs_fix++;
	      ins.needs_fix++;
	    }
	}
    }
  return 1;
}

/* Add one token to the current operand.  */
void 
add_token (char *pnt, int len, int token)
{
  int token_number = ins.ops[ins.operands].ntokens;
  if (len == 0)
    return;
  ins.ops[ins.operands].strings[token_number] = (char *) malloc (len + 1);
  strncpy (ins.ops[ins.operands].strings[token_number], pnt, len);
  ins.ops[ins.operands].strings[token_number][len] = 0;
  ins.ops[ins.operands].token[token_number] = token;
  ins.ops[ins.operands].ntokens++;
}

/* This function prints out the current contents of the ins structure,
   putting all of the tokens back together into a single string, and
   then substitutes this string back into the instruction stream.  */
void 
print_instruction ()
{
  rewrite_instruction (ins, fpnt);
}

void 
rewrite_instruction (struct instruction ins, LINE *wpnt)
{
  int i, j;
  char *cpnt;
  cpnt = tbuff;

  tbuff[0] = '\t';
  strcpy (tbuff + 1, ins.opcode);
  cpnt += (1 + strlen (ins.opcode));

  for (i = 0; i < ins.operands; i++)
    {
      if (i)
	*cpnt++ = ',';
      for (j = 0; j < 30; j++)
	{
	  if (!ins.ops[i].strings[j])
	    break;
	  strcpy (cpnt, ins.ops[i].strings[j]);
	  cpnt += strlen (ins.ops[i].strings[j]);
	}
    }
  *cpnt++ = '\n';
  *cpnt++ = 0;
  xfree (wpnt->text);
  if (foo)
    fprintf (foo, "Rplc:%s", tbuff);
  wpnt->text = xstrdup (tbuff);
}

/* This function cleans out the entire ins structure, getting it ready
   for the next instruction.  We should free all malloced memory that
   we have used here.  */
static void 
cleanup ()
{
  int i, j;
  for (i = 0; i < ins.operands; i++)
    {
      for (j = 0; j < 30; j++)
	{
	  if (!ins.ops[i].strings[j])
	    break;
	  xfree (ins.ops[i].strings[j]);
	  ins.ops[i].strings[j] = 0;
	}
      ins.ops[i].ntokens = 0;
      ins.ops[i].needs_fix = 0;
      ins.ops[i].regused = 0;
    }
  ins.uses_stack = 0;
  ins.needs_fix = 0;
}

/* This function does the first level parse of one operand.  It
   divides up the operand into tokens, and classifies some of the
   single character tokens so that classify_operands does not have to
   switch on the single characters again.  */
void 
parse_operand (char *pnt)
{
  int i;
  int flag;
  char c[2];
  char *pnt1;
  pnt1 = pnt;
  flag = 0;
  i = 0;
  do
    {
      switch (pnt[i])
	{
	case '+': case '-': case '(': case ')': case '*':
	case ',': case '@': case '{': case '}': case ':':
	  if (pnt[i] == '-' && (flag ? pnt[i - 1] == '#' : 1))
	    {
	      /* This is a unary minus (i.e. a negative number), not a
		 subtraction.  */
	      i++;
	      flag++;
	      break;
	    }
	  flag++;
	  c[0] = pnt[i];
	  c[1] = 0;
	  if (i)
	    add_token (pnt, i, 0);
	  add_token (c, 1, pnt[i]);
	  pnt += ++i;
	  i = 0;
	  break;

	default:
	  flag++;
	  i++;
	}
    }
  while (pnt[i]);
  add_token (pnt, i, 0);
  ins.operands++;
}

/* This is the entry point for this portion of the program.  Here we
   scan the entire file (which should already be in memory), and fix
   operands as required.  */
int
fixassy ()
{
  int i;
  int opstart;
  char *pnt;
  char *pnt1;

#ifdef LOGFILE
  {
    char filename[256], *filedir = NULL;
    filedir = getenv ("JUMP_DIR");
    strcpy (filename, filedir);
    strcat (filename, "/");
    strcat (filename, JUMPAS_SUBLOG);
    foo = fopen (filename, "a");
  }
#endif

  for (fpnt = fhead; fpnt; fpnt = fpnt->next)
    {
      strcpy (buffer, fpnt->text);
      pnt = buffer;

      /* First locate instruction opcode */
      if (buffer[0] != '\t')
	continue;

      if (buffer[1] == '|')
	continue;

      opstart = 1;
      /* Now locate operands */
      if (buffer[1] == '.')
	/* not pseudo ops */
	continue;

      i = 1;

      while (buffer[i] != '\n' && buffer[i] != ' ' && buffer[i] != '\t')
	i++;
      /* No operands */
      if (buffer[i] == '\n')
	continue;

      /* Now find start of first operand */
      while (buffer[i] == ' ' || buffer[i] == '\t')
	i++;
      strncpy (ins.opcode, &buffer[opstart], i - opstart);
      ins.opcode[i - opstart] = 0;
      ins.operands = 0;
      pnt = &buffer[i];

      /* Now pick out operands one by one and pass them to the parse
	 routine.  */
      while (1)
	{
	  pnt1 = pnt;
	  while (1)
	    {
	      if (pnt1[0] == '|')
		{
		  /* If this is a comment, trim trailing spaces.  */
		  int j;
		  pnt1[0] = 0;
		  j = -1;
		  while ((pnt1[j] == ' ' || pnt1[j] == '\t')
			 && pnt <= &pnt1[j])
		    j--;
		  pnt1[j + 1] = 0;
		  pnt1 = 0;	/* This is a comment */
		  break;
		}
	      if (*pnt1 == ',')
		break;
	      if (*pnt1 == '\n' || *pnt1 == 0)
		{
		  pnt1 = 0;
		  break;
		}
	      if (*pnt1 == '(')
		while (*pnt1 != ')')
		  pnt1++;
	      pnt1++;
	    }
	  if (!pnt1)
	    {
	      pnt1 = strchr (pnt, '\n');
	      if (pnt1)
		*pnt1 = 0;
	      parse_operand (pnt);
	      break;
	    }
	  *pnt1 = 0;
	  parse_operand (pnt);
	  pnt = ++pnt1;
	}

      /* OK, all of the operands have been parsed.  Now classify all
         of the components in all of the operands.  */

      classify_operands ();

      /* OK, all of the operands have been parsed.  Now start to
         fiddle with them.  */

      if (ins.needs_fix)
	fixup ();

      cleanup ();
    }
  /* Now add the declaration for the temporary variable we use.  */
  if (moved)
    {
      LINE *pnt = NULL;
      fpnt = fhead;
      while (fpnt->next)
	fpnt = fpnt->next;
      pnt = xmalloc (sizeof (LINE));
      pnt->next = NULL;
      sprintf (tbuff, ".lcomm __REG_SAVE__,%d\n", moved * 4);
      pnt->text = xstrdup (tbuff);
      fpnt->next = pnt;
    }
  if (foo)
    fclose (foo);
  return 0;
}
