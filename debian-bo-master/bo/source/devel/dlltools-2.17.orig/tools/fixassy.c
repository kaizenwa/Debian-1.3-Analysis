/* Program fixassy.c:

   Copyright (C) 1993, Eric Youngdale.

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

/* 
 * This portion of the program is responsible for rewriting the
 * operands to machine instructions to add an indirection for all of the
 * global variables listed in the input file jump.vars.  New instructions
 * are added in order to move the data around as required, and a register is
 * save and restored so that nothing gets screwed up.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "utils.h"


FILE * infile;

char buffer[16384];
char tbuff[1024];
char * regs[] = {"%eax","%ebx","%ecx","%edx"};

char reg_contains[1024];
int usereg = -1;
int moved = 0;
int moved2 = 0;
LINE *fpnt;
static FILE * foo;
extern char *outname;

#ifdef DEBUG_IMPLIED
void send_log(char type, char * name);
#endif

void rewrite_instruction(struct instruction ins, LINE * wpnt);
void print_instruction(int flag);
void restore_register();

/*
 * Insert one line of text into the text file at the current position
 */
void insert_text(char * cpnt){
  LINE *pnt = NULL;

  pnt = xmalloc(sizeof(LINE));
  pnt->next = fpnt->next;
  pnt->text = fpnt->text;
  fpnt->next = pnt;
  fpnt->text = xstrdup(cpnt);
  if (foo) fprintf(foo,"Rplc:%s",cpnt);
  fpnt = fpnt->next;
}

void insert_text_trailing(char * cpnt, int flag){
  LINE *pnt = NULL;

  pnt = xmalloc(sizeof(LINE));
  pnt->next = fpnt->next;
  pnt->text = xstrdup(cpnt);
  if (foo) {
    if (!flag)
      fprintf(foo,"(trailing)Rplc:%s",cpnt);
    else
      fprintf(foo,"Old:%s",cpnt);
  }
  fpnt->next = pnt;
}

/* We have an instruction that becomes too complicated when we try to add the
   indirection.  We try and copy the result to a temporary variable */

int spill(int regno, int regno2){
  char regname[6];
  char * pnt;
  int j;

  if(regno2 == -1) error("Ran out of registers in instruction %s\n",
			 fpnt->text);

  if(ins.needs_fix > 1 && (strncmp(ins.opcode, "mov", 3) == 0 || 
			   strncmp(ins.opcode, "cmp", 3) == 0 ) &&
     strchr("bwl",ins.opcode[3])){
    if(ins.uses_stack)
      sprintf(tbuff, "\tmovl %s,__REG_SAVE2__\n",regs[regno2]);
    else
      {
	restore_register();  /* We need to make sure that these get on the
				stack in the correct order */
	sprintf(tbuff, "\tpushl %s\n",regs[regno2]);
      };
    insert_text(tbuff); 

    if(ins.uses_stack)
      sprintf(tbuff, "\tmovl __REG_SAVE2__,%s\n",regs[regno2]);
    else
      sprintf(tbuff, "\tpopl %s\n",regs[regno2]);

    insert_text_trailing(tbuff,0); 
    /* Now generate the name of the register that we will use for
       various manipulations */
    pnt = regname;
    *pnt++ = '%';
    if(ins.opcode[3] == 'l') *pnt++ = 'e';
    *pnt++ = regs[regno2][2];
    if(ins.opcode[3] == 'b') *pnt++ = 'l';
    else *pnt++ = 'x';
    *pnt++ = 0;
    
    
    ins1 = ins;

    strcpy(ins.opcode, "movl ");  /* We need to do a move... */
    ins.opcode[3] = ins1.opcode[3]; /* Get same operand size */

    insert_text_trailing(fpnt->text,1);  /* Add a copy of the same instruction */
    ins.ops[1].strings[0] = xstrdup(regname);
    ins.ops[1].token[0] = 'g';
    for(j=1;j<10;j++) ins.ops[1].strings[j] = 0;
    ins.ops[1].ntokens = 1;
    ins.ops[1].needs_fix = 0;
    ins.needs_fix--;
    
    ins1.ops[0].strings[0] = xstrdup(regname);
    ins1.ops[0].token[0] = 'g';
    for(j=1;j<10;j++) ins1.ops[0].strings[j] = 0;
    ins1.ops[0].ntokens = 1;
    ins1.ops[0].needs_fix = 0;
    ins1.needs_fix--;
    
    rewrite_instruction(ins1, fpnt->next);
    if (ins.uses_stack)
      moved2++;
    return 1;
  };
  return 0;
}

/*
 * If a register has been save, restore it so that it contains the correct
 * contents.  We do not do this immediately after we are done using it because
 * the next instruction may need the same number again, so we wait until we
 * are sure that this is not the case before we restore the register
 */
void restore_register(){
  if(usereg != -1) {
    if(usereg & 0x20) return;
    if(usereg & 0x10)
      sprintf(tbuff, "\tpopl %s\n",regs[usereg & 0xf]);
    else {
      sprintf(tbuff, "\tmovl __REG_SAVE__,%s\n",regs[usereg]);
      moved++;
    };
    insert_text(tbuff); 
    reg_contains[0] = 0;
    usereg = -1;
  };
}

/*
 * This function actually does the fixup work on the operands.  It is assumed
 * that the instruction and all of it's operands have all been parsed into the
 * ins structure, and each token in each operand has also been properly
 * classified so that it is relatively easy to figure out what we need to do.
 */
void fixup(){
  char * pnt;
  int phase;
  int usereg1, usereg2;
  int needs_indirection;
  int doing_exchange;
  int flags_pushed;
  char newreg[64];
  int start_token;
  int registers[5];
  int i, j, k;

/* The first step is to figure out whether the operands are too complicated
   and if we need to split the instruction into two.  */


  /* The first step in a fixup is to figure out what registers are available
     to be used to move numbers around.  In general, we do not want to use
     registers that are already in use for this instruction */

  if(strncmp(ins.opcode,"push",4) == 0) ins.uses_stack |= 0x10;
  if(strncmp(ins.opcode,"pop",3) == 0) ins.uses_stack |= 0x20;

  needs_indirection = 0;
  doing_exchange = 0;
  flags_pushed = 0;
  for(i=0;i<5;i++) registers[i] = 0;

  for(i=0;i<ins.operands;i++){
    if(ins.ops[i].regused & 1) registers[0]++;
    if(ins.ops[i].regused & 2) registers[1]++;
    if(ins.ops[i].regused & 4) registers[2]++;
    if(ins.ops[i].regused & 8) registers[3]++;
    if(ins.ops[i].regused & 16) registers[4]++;
  };
  usereg1 = -1;
  if(registers[1] == 0) usereg1 = 1;
  else if(registers[0] == 0) usereg1 = 0;
  else if(registers[2] == 0) usereg1 = 2;
  else if(registers[3] == 0) usereg1 = 3;
  if(usereg1 == -1) exit(1);

  if(usereg1 != (usereg & 0x0f) || ins.uses_stack)
    restore_register();

  /* These are the most common types of "unfixable instructions" */
  if(strncmp(ins.opcode, "mov", 3) == 0 || strncmp(ins.opcode, "cmp", 3) == 0){
    usereg2 = -1;
    if(usereg1 != 1 && registers[1] == 0)      usereg2 = 1;
    else if(usereg1 != 0 && registers[0] == 0) usereg2 = 0;
    else if(usereg1 != 2 && registers[2] == 0) usereg2 = 2;
    else if(usereg1 != 3 && registers[3] == 0) usereg2 = 3;

    if (ins.needs_fix == 2) spill(usereg1,usereg2);

    if (ins.needs_fix == 1 && ins.ops[0].needs_fix == 1){ 
      for(j=0;j<ins.ops[1].ntokens;j++)
	if(ins.ops[1].token[j] == '(') spill(usereg1,usereg2);
    };
    if (ins.needs_fix == 1 && ins.ops[1].needs_fix == 1){ 
      for(j=0;j<ins.ops[0].ntokens;j++)
	if(ins.ops[0].token[j] == '(') spill(usereg1,usereg2);
    };
  };
    
  if(ins.needs_fix > 1) {
    fprintf(stderr, "Unable to rewrite this instruction:%s \n", fpnt->text);
    exit(1);
  };

  if(usereg1 != (usereg & 0x0f) || ins.uses_stack) {
    usereg = usereg1;
    if (foo) {
      int i,j,k;
      char keys[100];
      k = 0;
      for(i=0;i<ins.operands;i++){
	for(j=0;j<10;j++) {
	  if (!ins.ops[i].strings[j]) break;
	  keys[k++] = ins.ops[i].token[j];
	};
	keys[k++] = ',';
      };
      keys[--k] = 0;
      fprintf(foo,"\n\nOld(%s:%s):%s",outname, keys, fpnt->text);
    };
#if 1
    if(ins.uses_stack && (strncmp(ins.opcode,"pushl",5)))
#else
    if(ins.uses_stack)
#endif
      sprintf(tbuff, "\tmovl %s,__REG_SAVE__\n",regs[(usereg & 0xf)]);
    else
      {
	sprintf(tbuff, "\tpushl %s\n",regs[(usereg & 0xf)]);
	usereg |= 0x10;
      };
    insert_text(tbuff); 
  };

  /* Now dig out the symbol that needs to be rewritten */
  for(i=0;i<ins.operands;i++){
    phase = 0;
    start_token = -1;
    if(!ins.ops[i].needs_fix) continue;
    for(j=0;j<10;j++) {
      pnt  = ins.ops[i].strings[j];
      if (!pnt) break;
      if(ins.ops[i].token[j] == 'G' && phase == 0) {
	start_token = j;
	k = 0;
	if (*pnt == '$') k++;
	sprintf(tbuff, "\tmovl __GOT_%s,%s\n", &pnt[k], regs[(usereg & 0xf)]);
	if(strcmp(reg_contains, tbuff)){
	  strcpy(reg_contains, tbuff);
	  insert_text(tbuff); 
	};

#if 1
	if(strncmp(ins.opcode,"pushl",5) == 0){
	  strcpy(ins.opcode,"xchg ");
	  doing_exchange = 1;
	  usereg |= 0x20;
	  ins.operands = 2;
	  ins.ops[1].strings[0] = xstrdup("(%esp)");
	  ins.ops[1].token[0] = 's';
	  sprintf(newreg,"%s",regs[(usereg & 0xf)]);
	  if(!(k == 1 && pnt[0] == '$'))
	    needs_indirection = 1;
	} else {
#else
	  {
#endif
	  if(k == 1 && pnt[0] == '$')
	    sprintf(newreg,"%s",regs[(usereg & 0xf)]);
	  else
	    sprintf(newreg,"(%s)",regs[(usereg & 0xf)]);
	};

	xfree(ins.ops[i].strings[j]);
	ins.ops[i].strings[j] = xstrdup(newreg);
	/* Now find out if there are offsets that need to be added */
	phase++;
	continue;
      };
      if(ins.ops[i].token[j] == '+' && phase == 1) {
	if (flags_pushed++ == 0) insert_text("\tpushf\n");
	sprintf(tbuff, "\taddl $%s,%s\n",ins.ops[i].strings[j+1],regs[(usereg & 0xf)]);
	reg_contains[0] = 0;
	insert_text(tbuff); 
	/* Now find out if there are offsets that need to be added */
	j++;
	phase++;
	continue;
      };
      if(ins.ops[i].token[j] == '-' && phase == 1) {
	if (flags_pushed++ == 0) insert_text("\tpushf\n");
	sprintf(tbuff,"\tsubl $%s,%s\n",ins.ops[i].strings[j+1],regs[(usereg & 0xf)]);
	reg_contains[0] = 0;
	insert_text(tbuff); 
	/* Now find out if there are offsets that need to be added */
	j++;
	phase++;
	continue;
      };
      if(ins.ops[i].token[j] == '(' && ins.ops[i].token[j+2] == ')' && 
	 phase > 0) {
	if (flags_pushed++ == 0) insert_text("\tpushf\n");
	sprintf(tbuff,"\taddl %s,%s\n",ins.ops[i].strings[j+1],regs[(usereg & 0xf)]);
	reg_contains[0] = 0;
	insert_text(tbuff); 
	j += 2;
	phase++;
	continue;
      };
      if(ins.ops[i].token[j] == '(' && ins.ops[i].token[j+2] == ',' && 
	 ins.ops[i].token[j+4] == ')' &&  phase > 0) {
	if (flags_pushed++ == 0) insert_text("\tpushf\n");
	sprintf(tbuff,"\taddl %s,%s\n",ins.ops[i].strings[j+1],regs[(usereg & 0xf)]);
	reg_contains[0] = 0;
	insert_text(tbuff); 

	sprintf(newreg,"(%s,%s)",regs[(usereg & 0xf)],ins.ops[i].strings[j+3]);
	xfree(ins.ops[i].strings[start_token]);
	ins.ops[i].strings[start_token] = xstrdup(newreg);
	j += 4;
	phase++;
	continue;
      };
      if(ins.ops[i].token[j] == '(' && ins.ops[i].token[j+1] == ',' && 
	 ins.ops[i].token[j+5] == ')' &&  phase > 0) {
	sprintf(newreg,"(%s,%s,%s)",regs[(usereg & 0xf)],ins.ops[i].strings[j+2]
		,ins.ops[i].strings[j+4]);
	xfree(ins.ops[i].strings[start_token]);
	ins.ops[i].strings[start_token] = xstrdup(newreg);
	j += 5;
	phase++;
	continue;
      };
      if(ins.ops[i].token[j] == '(' && ins.ops[i].token[j+2] == ',' && 
	 ins.ops[i].token[j+6] == ')' &&  phase > 0) {
	if (flags_pushed++ == 0) insert_text("\tpushf\n");
	sprintf(tbuff,"\taddl %s,%s\n",ins.ops[i].strings[j+1],regs[(usereg & 0xf)]);
	reg_contains[0] = 0;
	insert_text(tbuff); 
	sprintf(newreg,"(%s,%s,%s)",regs[(usereg & 0xf)],ins.ops[i].strings[j+3]
		,ins.ops[i].strings[j+5]);
	xfree(ins.ops[i].strings[start_token]);
	ins.ops[i].strings[start_token] = xstrdup(newreg);
	j += 6;
	phase++;
	continue;
      };
      if(phase) {
	printf("Error, confused by :%s\n", fpnt->text);
	exit(1);
      };
    };
    for(j=start_token+1;j<10;j++) {
	xfree(ins.ops[i].strings[j]);
	ins.ops[i].strings[j] = 0;
      };
    ins.ops[i].ntokens = start_token + 1;
    }
    if (flags_pushed) 	insert_text("\tpopf\n");

    /* This happens if the operand requires an indirection and it is not
       complex (i.e. it is not have a '(' in it. Complex operands are handled
       next.  We only do this for pushl instructions. */
  if (needs_indirection && !ins.ops[0].complex){
    sprintf(tbuff,"\tmovl (%s),%s\n",regs[(usereg & 0xf)],regs[(usereg & 0xf)]);
    insert_text(tbuff); 
    reg_contains[0] = 0;
  };

  if(ins.ops[0].complex && doing_exchange){
    if(!needs_indirection){
      fprintf(stderr, "Unable to rewrite this instruction:%s \n", fpnt->text);
      exit(1);
    };
    strcpy(ins.opcode,"movl ");
    xfree(ins.ops[1].strings[0]);
    ins.ops[1].strings[0] = xstrdup(regs[(usereg & 0xf)]);
    sprintf(tbuff,"\txchg %s,(%%esp)\n",regs[(usereg & 0xf)]);
    insert_text_trailing(tbuff,0); 
  };

    /* This register no longer contains our temporary value.  Do not restore
     register from stack.  Do not collect 200$ */
  if(doing_exchange) {
    reg_contains[0] = 0;
    usereg = -1;
  };

  print_instruction(0);
}

/*
 * This function is responsible for identifying the type of each token
 * in each operand.  Many tokens represent single characters, like '(', 
 * '+', etc.  The only non-single character tokens are numbers, symbols
 * and register names.  We need to record which registers are in use for this
 * instruction because we may need to use a register to add the indirection,
 * and it tends to work out a whole lot better if we do select a register that
 * is already in use in the instruction.
 */
/* Tokens used abcdSDBs: registers, G#g */
int classify_operands(){
  char * pnt;
  int i, j, k;
  GLOBAL * gpnt;
  struct symbol * spnt;
  char c;
  for(i=0;i<ins.operands;i++){
    for(j=0;j<10;j++) {
      pnt  = ins.ops[i].strings[j];
      if (!pnt) break;
      if(ins.ops[i].token[j] == '(') ins.ops[i].complex = 1;
      if(ins.ops[i].token[j]) continue;
      k =0 ;
      while(pnt[k] == ' ' || pnt[k] == '\t') k++;
      if(pnt[k] == '%') {
	k++;
	if (pnt[k] == 'e') k++ ;
	c = pnt[k++];
	if(c == 's' && pnt[k] == 'i') c = 'S';
	if(c == 'd' && pnt[k] == 'i') c = 'D';
	if(c == 'b' && pnt[k] == 'p') c = 'B';
	if(c == 's' && pnt[k] == 'p') ins.uses_stack = 1;
	if(c == 's' && pnt[k] == 't') c = 'F';
	ins.ops[i].token[j] = c;

	switch(c){
	case 'a':
	  ins.ops[i].regused |= 1;
	  break;
	case 'b':
	  ins.ops[i].regused |= 2;
	  break;
	case 'c':
	  ins.ops[i].regused |= 4;
	  break;
	case 'd':
	  ins.ops[i].regused |= 8;
	  break;
	case 'S':
	case 's':
	case 'D':
	case 'B':
	  ins.ops[i].regused |= 16;
	  break;
	};
	continue;
      };
      /* Now we have to decide if this is an integer constant or a symbol */
      k = 0;
      if(pnt[0] == '$') k++;
      if(pnt[k] == 'L'){ /* Local symbol */
	ins.ops[i].token[j] = pnt[k];
	continue;
      };
      while(pnt[k] == ' ' || pnt[k] == '\t') k++;
      if(pnt[k] == '-' && (!k || pnt[k-1] == '$')) k++;

      if(pnt[k] == '0' && pnt[k+1] == 'x') {
	k += 2;
	while(pnt[k] && ((pnt[k] >= '0' && pnt[k] <= '9') ||
			 (pnt[k] >= 'A' && pnt[k] <= 'F') ||
			 (pnt[k] >= 'a' && pnt[k] <= 'f'))) k++;
      } else
	while(pnt[k] && pnt[k] >= '0' && pnt[k] <= '9') k++;

      while(pnt[k] == ' ' || pnt[k] == '\t') k++;

      if(pnt[k]) ins.ops[i].token[j] = 'G';
      else ins.ops[i].token[j] = '#';

      k = 0;
      if(pnt[0] == '$') k++;

      /* OK, we have a symbol.  We need to see if it is global.  The first step
	 is to see if this symbol is declared global in this file.  If not,
	 then who cares what the global definition for this symbol is,
	 because we have a static variant here that has nothing to do
	 with the global version */

      if (ins.ops[i].token[j] == 'G'){
	spnt = shead;
	while (spnt){
	  if (strcmp(&pnt[k], spnt->name) == 0)
	    break;
	  spnt = spnt->next;
	};
	if(spnt) ins.ops[i].token[j] = 'g'; /* No need for indirection */
      };



      /* Jump instructions do not need to go through the GOT.  Fortunately,
       they all start with the letter 'j' */

      if (ins.ops[i].token[j] == 'G' && ins.opcode[0] == 'j' &&
	  ins.operands == 1 && ins.ops[i].ntokens == 1)
	ins.ops[i].token[j] = 'g';



      /* Call instructions do not need to be routed through the GOT, because
	 we assume that these go through the jump table.  */

      if (ins.ops[i].token[j] == 'G' && strncmp(ins.opcode, "call", 4) == 0 &&
	  ins.operands == 1 && ins.ops[i].ntokens == 1) {
	ins.ops[i].token[j] = 'g';

	/* Time for a sanity check here.  Make sure that this symbol is not
	   listed in jump.vars. */
	gpnt = gdhead;
	while (gpnt)
	  {
	    if (strcmp(&pnt[k], gpnt->name) == 0) break;
	    gpnt = gpnt->next;
	  };
	if(gpnt) error("Function %s incorrectly listed in jump.vars\n",
		       gpnt->name);
      };
 



      if (ins.ops[i].token[j] == 'G'){
	gpnt = gdhead; /* Explicitly exported variable list */
	while (gpnt)
	  {
	    if (strcmp(&pnt[k], gpnt->name) == 0) break;
	    gpnt = gpnt->next;
	  };
	if(!gpnt) { /* OK, we are not exporting this symbol.  See if we
		       are importing it.  If not, we do not need to add
		       the indirection */
#ifdef IMPLIED_IMPORT
	  gpnt = gfhead;  /* Global function list */
	  while (gpnt)
	    {
	      if (strcmp(&pnt[k], gpnt->name) == 0) break;
	      gpnt = gpnt->next;
	    };
	  
	  if(!gpnt){
	    gpnt = gihead;  /* Ignore list */
	    while (gpnt)
	      {
		if (strcmp(&pnt[k], gpnt->name) == 0) break;
		gpnt = gpnt->next;
	      };
	  };
	  if(gpnt) ins.ops[i].token[j] = 'g'; /* No need for indirection */
#ifdef DEBUG_IMPLIED
	  if(!gpnt) send_log('i',&pnt[k]);
#endif
#else
	  gpnt = gphead;  /* Explicit import */
	  while (gpnt)
	    {
	      if (strcmp(&pnt[k], gpnt->name) == 0) break;
	      gpnt = gpnt->next;
	    };
	  if(!gpnt) ins.ops[i].token[j] = 'g'; /* No need for indirection */
#endif
	};
      }

      /* Here is a relatively simple fixup.  If the operand is $_foo, we just
	 rewrite it as __GOT__foo.  We have to be extremely careful, becuase
	 it is easy to write an invalid instruction through this
	 transformation. Currently we only allow this if the other operand
	 is just a register. We always allow this in single operand
	 instructions  */

      if (ins.ops[i].token[j] == 'G' && k == 1 && pnt[0] == '$' && 
	  ins.ops[i].ntokens == 1){
	int i1;
	int flag = 0;
	for(i1=0;i1<ins.operands;i1++){
	  if(i == i1) continue;
	  if(ins.ops[i1].ntokens > 1) flag++;
	  if(ins.ops[i1].strings[0][0] != '%') flag++;
	};
	if(!flag){
	  sprintf(tbuff, "__GOT_%s", &pnt[k]);
	  xfree(ins.ops[i].strings[j]);
	  ins.ops[i].strings[j]=xstrdup(tbuff);
	  ins.ops[i].token[j] = 'g';
	  print_instruction(1);
	};
      };

      if (ins.ops[i].token[j] == 'G'){
	ins.ops[i].needs_fix++;
	ins.needs_fix++;
      };
    };
  };
  return 1;
}

/*
 * Add one token to the current operand.
 */
void add_token(char * pnt, int len, int token_number, int token){
  if(len == 0) return;
  ins.ops[ins.operands].strings[token_number] = (char *) malloc(len+1);
  strncpy(ins.ops[ins.operands].strings[token_number], pnt, len);
  ins.ops[ins.operands].strings[token_number][len] = 0;
  ins.ops[ins.operands].token[token_number] = token;
  ins.ops[ins.operands].ntokens++;
}

/*
 * This function prints out the current contents of the ins structure, putting
 * all of the tokens back together into a single string, and then substitutes
 * this string back into the instruction stream
 */
void print_instruction(int flag){
  if(flag) restore_register();
  rewrite_instruction(ins, fpnt);
}

void rewrite_instruction(struct instruction ins, LINE * wpnt){
  int i, j;
  char * cpnt;
  cpnt = tbuff;

#if 0
  cpnt +=sprintf(tbuff,"\t%s",ins.opcode);
#endif
  sprintf(tbuff,"\t%s",ins.opcode);
  cpnt += (1+strlen(ins.opcode));

  for(i=0;i<ins.operands;i++){
    for(j=0;j<10;j++) {
      if (!ins.ops[i].strings[j]) break;
/*      printf("%s{%c}",ins.ops[i].strings[j], ins.ops[i].token[j]); */
#if 0
      cpnt += sprintf(cpnt,"%s",ins.ops[i].strings[j]);
#endif
      sprintf(cpnt,"%s",ins.ops[i].strings[j]);
      cpnt += strlen(ins.ops[i].strings[j]);
    };
    if(i != ins.operands - 1) *cpnt++ = ',';
  };
  *cpnt++ = '\n';
  *cpnt++ = 0;
  xfree (wpnt->text);
  if (foo) fprintf(foo, "Rplc:%s",tbuff);
  wpnt->text = xstrdup(tbuff);
}

/* 
 * This function cleans out the entire ins structure, getting it ready for
 * the next instruction.  We should free all malloced memory that we have used
 * here.
 */
static void cleanup(){
  int i, j;
  for(i=0;i<ins.operands;i++){
    for(j=0;j<10;j++) {
      if (!ins.ops[i].strings[j]) break;
      xfree(ins.ops[i].strings[j]);
      ins.ops[i].strings[j] = 0;
    };
    ins.ops[i].ntokens = 0;
    ins.ops[i].needs_fix = 0;
    ins.ops[i].regused = 0;
    ins.ops[i].complex = 0;
  };
  ins.uses_stack = 0;
  ins.needs_fix = 0;
}

/*
 * This function does the first level parse of one operand.  It divides up
 * the operand into tokens, and classifies some of the single character tokens
 * so that classify_operands does not have to switch on the single characters
 * again.
 */
void parse_operand(char * pnt){
  int i;
  int flag;
  char c[2];
  int subcomponent = 0;
  char * pnt1;
  pnt1 = pnt;
  flag = 0;
  i = 0;
  do {
    switch(pnt[i]){
    case '+':
    case '-':
    case '(':
    case ')':
    case '*':
    case ',':
      if(pnt[i] == '-' && ((flag && pnt[i-1] == '$') || !flag)){
	i++;  /* This is a unary minus (i.e. a negative number, 
		 not a subtraction */
	flag++;
	break;
      };
      flag++;
      c[0] = pnt[i];
      c[1] = 0;
      if(pnt[i] == '(') ins.ops[ins.operands].complex = 1;
      if(i) add_token(pnt,i, subcomponent++, 0);
      add_token(c,1, subcomponent++, pnt[i]);
      pnt+= ++i;
      i=0;
      break;
    default:
      flag++;
      i++;
    };
  }  while(pnt[i]);
  add_token(pnt,i, subcomponent++, 0);
  ins.operands++;
}

/*
 * This is the entry point for this portion of the program.  Here we scan
 * the entire file (which should already be in memory), and fix operands as
 * required.
 */
int fixassy(){
  int i;
  int opstart;
  char * pnt;
  char * pnt1;

#ifdef LOGFILE
 {
   char filename[256], *filedir = NULL;
   filedir = getenv("JUMP_DIR");
   strcpy(filename,filedir);
   strcat(filename,"/");
   strcat(filename,JUMPAS_SUBLOG);
   if ((foo = fopen(filename,"a"))==NULL)
	error("Can't open logfile %s (%s)",filename,strerror(errno));
 }
#endif

  for(fpnt = fhead; fpnt; fpnt = fpnt->next)
    {
      strcpy(buffer, fpnt->text);
      pnt = buffer;

      /* First locate instruction opcode */
      if(buffer[0] != '\t') {
	restore_register();
	continue;
      };

      if(buffer[1] == '/' && buffer[2] == '/') continue;


      opstart = 1;
      /* Now locate operands */
      if(buffer[1] == '.') { /* not pseudo ops */
	restore_register();
	continue;
      };

      i = 1;
	  
      while(buffer[i] != '\n' && buffer[i] != ' ' && buffer[i] != '\t') i++;
      /* No operands */
      if (buffer[i] == '\n')  {
	restore_register();
	continue;
      };
      
      
      /* Now find start of first operand */
      while(buffer[i] == ' ' || buffer[i] == '\t') i++;
      strncpy(ins.opcode, &buffer[opstart], i-opstart);
      ins.opcode[i-opstart] = 0;
      ins.operands = 0;
      pnt = &buffer[i];
      
      /* Now pick out operands one by one and pass them to the parse routine */
      while(1==1){
	pnt1 = pnt;
	while(1==1){
	  if(pnt1[0] == '/' && pnt1[1] == '/'){
	    int j;  /* If this is a comment, trim trailing spaces */
	    pnt1[0] = 0;
	    j = -1;
	    while((pnt1[j] == ' ' || pnt1[j] == '\t') && 
		  (unsigned int) pnt <= (unsigned int) &pnt1[j]) j--;
	    pnt1[j+1] = 0;
	    pnt1 = 0;  /* This is a comment */
	    break;
	    };
	  if(*pnt1 == ',') break;
	  if(*pnt1 == '\n' || *pnt1 == 0) {
	    pnt1 = 0;
	    break;
	  };
	  if(*pnt1 == '(') while(*pnt1 != ')') pnt1++;
	  pnt1++;
	};
	if(!pnt1) {
	  pnt1 = strchr(pnt,'\n');
	  if(pnt1) *pnt1 = 0;
	  parse_operand(pnt);
	  break;
	};
	*pnt1 = 0;
	parse_operand(pnt);
	pnt = ++pnt1;
      };
      /* OK, all of the operands have been parsed.  Now classify all of the
	 components in all of the operands */
      
      classify_operands();
      
      /* OK, all of the operands have been parsed.  Now start to fiddle with
	 them */

      if(ins.needs_fix)
	fixup();
      else
	restore_register();

      cleanup();
    };
  /* Now add the declaration for the temporary variable we use */
  if(moved) {
    LINE *pnt = NULL;
    fpnt = fhead;
    while(fpnt->next) fpnt = fpnt->next;
    pnt = xmalloc(sizeof(LINE));
    pnt->next = NULL;
    pnt->text = xstrdup(".lcomm __REG_SAVE__,4\n");
    fpnt->next = pnt;
  };
  if(moved2) {
    LINE *pnt = NULL;
    fpnt = fhead;
    while(fpnt->next) fpnt = fpnt->next;
    pnt = xmalloc(sizeof(LINE));
    pnt->next = NULL;
    pnt->text = xstrdup(".lcomm __REG_SAVE2__,4\n");
    fpnt->next = pnt;
  };
  if (foo) fclose(foo);
  return 0;
}
