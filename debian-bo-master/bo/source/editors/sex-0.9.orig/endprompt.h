/*
 * File:	endprompt.h
 * Purpose:	Declarations for the dialog box when exit without saves.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: endprompt.h,v 1.2 1996/12/22 20:06:11 liw Exp $"
 */

#ifndef endprompt_h
#define endprompt_h

struct endprompt;
struct win;

int endprompt_create(struct endprompt **, struct win *);
void endprompt_destroy(struct endprompt *);
void endprompt_popup(struct endprompt *);
void endprompt_popdown(struct endprompt *);

#endif
