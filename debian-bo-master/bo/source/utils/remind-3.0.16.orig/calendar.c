/***************************************************************/
/*                                                             */
/*  CALENDAR.C                                                 */
/*                                                             */
/*  The code for generating a calendar.                        */
/*                                                             */
/*  This file is part of REMIND.                               */
/*  Copyright (C) 1992-1997 by David F. Skoll                  */
/*                                                             */
/***************************************************************/

static char const RCSID[] = "$Id: calendar.c,v 1.3 1997/01/16 04:14:19 dfs Exp $";

#include "config.h"
#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <ctype.h>
#include "types.h"
#include "protos.h"
#include "expr.h"
#include "globals.h"
#include "err.h"

/* Data structures used by the calendar */
typedef struct cal_entry {
    struct cal_entry *next;
    char *text;
    char *pos;
    int time;
    int priority;
} CalEntry;

/* Global variables */
static CalEntry *CalColumn[7];
static CalEntry *CalPs[7];

static int ColSpaces;

PRIVATE void SortCol ARGS((CalEntry **col));
PRIVATE void DoCalendarOneWeek ARGS ((void));
PRIVATE void DoCalendarOneMonth ARGS ((void));
PRIVATE int WriteCalendarRow ARGS ((void));
PRIVATE void PrintLeft ARGS ((char *s, int width, char pad));
PRIVATE void PrintCentered ARGS ((char *s, int width, char pad));
PRIVATE int WriteOneCalLine ARGS ((void));
PRIVATE int WriteOneColLine ARGS ((int col));
PRIVATE void GenerateCalEntries ARGS ((int col));
PRIVATE void WriteCalHeader ARGS ((void));
PRIVATE void WriteCalTrailer ARGS ((void));
PRIVATE int DoCalRem ARGS ((ParsePtr p, int col));
PRIVATE void WriteSimpleEntries ARGS ((int col, int jul));
PRIVATE void WriteSolidCalLine ARGS ((void));
PRIVATE void WriteIntermediateCalLine ARGS ((void));
PRIVATE void WriteCalDays ARGS ((void));

/***************************************************************/
/*                                                             */
/*  ProduceCalendar                                            */
/*                                                             */
/*  Main loop for generating a calendar.                       */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PUBLIC void ProduceCalendar(void)
#else
void ProduceCalendar()
#endif
{
    int y, m, d;

    ShouldCache = 1;

    ColSpaces = (CalWidth - 9) / 7;
    CalWidth = 7*ColSpaces + 8;

    if (CalMonths) {
	FromJulian(JulianToday, &y, &m, &d);
	JulianToday = Julian(y, m, 1);   
	while (CalMonths--)
	    DoCalendarOneMonth();
	return;
    } else {
	if (MondayFirst) JulianToday -= (JulianToday%7);
	else             JulianToday -= ((JulianToday+1)%7);

	if (!DoSimpleCalendar) {
	    WriteIntermediateCalLine();
	    WriteCalDays();
	    WriteIntermediateCalLine();
	}

	while (CalWeeks--)
	    DoCalendarOneWeek();
	return;
    }
}

/***************************************************************/
/*                                                             */
/*  DoCalendarOneWeek                                          */
/*                                                             */
/*  Write a calendar for a single week                         */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void DoCalendarOneWeek(void)
#else
static void DoCalendarOneWeek()
#endif
{
    int y, m, d, done, i, l, wd;
    char buf[81];
    int LinesWritten = 0;
    int OrigJul = JulianToday;

/* Fill in the column entries */
    for (i=0; i<7; i++) {
	GenerateCalEntries(i);
	JulianToday++;
    }

/* Output the entries */

/* If it's "Simple Calendar" format, do it simply... */
    if (DoSimpleCalendar) {
	if (MondayFirst) wd = JulianToday % 7;
	else             wd = (JulianToday + 1) % 7;
	for (i=0; i<7; i++) {
	    WriteSimpleEntries(i, OrigJul+i-wd);
	}
	return;
    }

/* Here come the first few lines... */
    PutChar('|');
    for (i=0; i<7; i++) {
	FromJulian(OrigJul+i, &y, &m, &d);
	sprintf(buf, "%d %c%c%c ", d, MonthName[m][0], MonthName[m][1], 
		MonthName[m][2]);
	if (OrigJul+i == RealToday)				  
	    PrintLeft(buf, ColSpaces, '*');
	else
	    PrintLeft(buf, ColSpaces, ' ');
	PutChar('|');
    }
    PutChar('\n');
    for (l=0; l<CalPad; l++) {
	PutChar('|');
	for (i=0; i<7; i++) {
	    PrintLeft("", ColSpaces, ' ');
	    PutChar('|');
	}
	PutChar('\n');
    }

/* Write the body lines */
    done = 0;
    while (!done) {
	done = WriteOneCalLine();
	LinesWritten++;
    }

/* Write any blank lines required */
    while (LinesWritten++ < CalLines) {
	PutChar('|');
	for (i=0; i<7; i++) {
	    PrintLeft("", ColSpaces, ' ');
	    PutChar('|');
	}
	PutChar('\n');
    }

/* Write the final line */   
    WriteIntermediateCalLine();
}

/***************************************************************/
/*                                                             */
/*  DoCalendarOneMonth                                         */
/*                                                             */
/*  Produce a calendar for the current month.                  */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void DoCalendarOneMonth(void)
#else
static void DoCalendarOneMonth()
#endif
{
    int y, m, d, mm, yy;

    if (!DoSimpleCalendar) WriteCalHeader();

    if (PsCal) {
	FromJulian(JulianToday, &y, &m, &d);
	printf("%s\n", PSBEGIN);
	printf("%s %d %d %d %d\n",
	       MonthName[m], y, DaysInMonth(m, y), (JulianToday+1) % 7,
	       MondayFirst);
	mm = m-1;
	if (mm<0) {
	    mm = 11; yy = y-1;
	} else yy=y;

	printf("%s %d\n", MonthName[mm], DaysInMonth(mm,yy));
	mm = m+1;
	if (mm>11) {
	    mm = 0; yy = y+1;
	} else yy=y;
	printf("%s %d\n", MonthName[mm], DaysInMonth(mm,yy));
    }
    while (WriteCalendarRow()) continue;

    if (PsCal) printf("%s\n", PSEND);
    if (!DoSimpleCalendar) WriteCalTrailer();
}   

/***************************************************************/
/*                                                             */
/*  WriteCalendarRow                                           */
/*                                                             */
/*  Write one row of the calendar                              */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE int WriteCalendarRow(void)
#else
static int WriteCalendarRow()
#endif
{
    int y, m, d, wd, i, l;
    int done;
    char buf[81];
    int OrigJul = JulianToday;
    int LinesWritten = 0;

/* Get the date of the first day */
    FromJulian(JulianToday, &y, &m, &d);
    if (!MondayFirst) wd = (JulianToday + 1) % 7;
    else		     wd = JulianToday % 7;

/* Fill in the column entries */
    for (i=wd; i<7; i++) {
	if (d+i-wd > DaysInMonth(m, y)) break;
	GenerateCalEntries(i);
	JulianToday++;
    }

/* Output the entries */

/* If it's "Simple Calendar" format, do it simply... */
    if (DoSimpleCalendar) {
	for (i=wd; i<7 && d+i-wd<=DaysInMonth(m, y); i++) {
	    WriteSimpleEntries(i, OrigJul+i-wd);
	}
	return (d+7-wd <= DaysInMonth(m, y));
    }
 

/* Here come the first few lines... */
    PutChar('|');
    for (i=0; i<7; i++) {
	if (i < wd || d+i-wd>DaysInMonth(m, y))
	    PrintLeft("", ColSpaces, ' ');
	else {
	    sprintf(buf, "%d", d+i-wd);
	    PrintLeft(buf, ColSpaces, ' ');
	}
	PutChar('|');
    }
    PutChar('\n');
    for (l=0; l<CalPad; l++) {
	PutChar('|');
	for (i=0; i<7; i++) {
	    PrintLeft("", ColSpaces, ' ');
	    PutChar('|');
	}
	PutChar('\n');
    }

/* Write the body lines */
    done = 0;
    while (!done) {
	done = WriteOneCalLine();
	LinesWritten++;
    }

/* Write any blank lines required */
    while (LinesWritten++ < CalLines) {
	PutChar('|');
	for (i=0; i<7; i++) {
	    PrintLeft("", ColSpaces, ' ');
	    PutChar('|');
	}
	PutChar('\n');
    }

    WriteIntermediateCalLine();

/* Return non-zero if we have not yet finished */
    return (d+7-wd <= DaysInMonth(m, y));
}

/***************************************************************/
/*                                                             */
/*  PrintLeft                                                  */
/*                                                             */
/*  Left-justify a piece of text.                              */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void PrintLeft(char *s, int width, char pad)
#else
static void PrintLeft(s, width, pad)
char *s;
int width;
char pad;
#endif
{
    int len = strlen(s);
    printf("%s", s);
    while (len++ < width) PutChar(pad);
}

/***************************************************************/
/*                                                             */
/*  PrintCentered                                              */
/*                                                             */
/*  Center a piec of text                                      */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void PrintCentered(char *s, int width, char pad)
#else
static void PrintCentered(s, width, pad)
char *s;
int width;
char pad;
#endif
{
    int len = strlen(s);
    int d = (width - len) / 2;
    int i;

    for (i=0; i<d; i++) PutChar(pad);
    for (i=0; i<width; i++) {
	if (*s) PutChar(*s++); else break;
    }
    for (i=d+len; i<width; i++) PutChar(pad);
}

/***************************************************************/
/*                                                             */
/*  WriteOneCalLine                                            */
/*                                                             */
/*  Write a single line.                                       */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE int WriteOneCalLine(void)
#else
static int WriteOneCalLine()
#endif
{
    int done = 1, i;

    PutChar('|');
    for (i=0; i<7; i++) {
	if (CalColumn[i]) {
	    if (WriteOneColLine(i)) done = 0;
	} else {
	    PrintCentered("", ColSpaces, ' ');
	}
	PutChar('|');
    }
    PutChar('\n');

    return done;
}

     
/***************************************************************/
/*                                                             */
/*  WriteOneColLine                                            */
/*                                                             */
/*  Write a single line for a specified column.  Return 1 if   */
/*  the column still has entries; 0 otherwise.                 */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE int WriteOneColLine(int col)
#else
static int WriteOneColLine(col)
int col;
#endif
{
    CalEntry *e = CalColumn[col];
    char *s;
    char *space;
    int numwritten = 0;

/* Print as many characters as possible within the column */
    space = NULL;
    s = e->pos;

/* If we're at the end, and there's another entry, do a blank line and move
   to next entry. */
    if (!*s && e->next) {
	PrintLeft("", ColSpaces, ' ');
	CalColumn[col] = e->next;
	free(e->text);
	free(e);
	return 1;
    }

/* Find the last space char within the column. */
    while (s - e->pos <= ColSpaces) {
	if (!*s) {space = s; break;}
	if (*s == ' ') space = s;
	s++;
    }

/* If we couldn't find a space char, print what we have. */
    if (!space) {
	for (s = e->pos; s - e->pos < ColSpaces; s++) {
	    if (!*s) break;
	    numwritten++;
	    PutChar(*s);
	}
	e->pos = s;
    } else {

/* We found a space - print everything before it. */
	for (s = e->pos; s<space; s++) {
	    if (!*s) break;
	    numwritten++;
	    PutChar(*s);
	}
    }

/* Flesh out the rest of the column */
    while(numwritten++ < ColSpaces) PutChar(' ');

/* Skip any spaces before next word */
    while (*s == ' ') s++;

/* If done, free memory if no next entry. */
    if (!*s && !e->next) {
	CalColumn[col] = e->next;
	free(e->text);
	free(e);
    } else {
	e->pos = s;
    }
    if (CalColumn[col]) return 1; else return 0;
}

/***************************************************************/
/*                                                             */
/*  GenerateCalEntries                                         */
/*                                                             */
/*  Generate the calendar entries for the ith column           */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void GenerateCalEntries(int col)
#else
static void GenerateCalEntries(col)
int col;
#endif
{
    int r;
    Token tok;
    char *s;
    Parser p;

/* Do some initialization first... */
    ClearGlobalOmits();
    DestroyOmitContexts();
    DestroyVars(0);
    NumTriggered = 0;

    r=OpenFile(InitialFile);
    if (r) {
	fprintf(ErrFp, "%s %s: %s\n", ErrMsg[E_ERR_READING], InitialFile, ErrMsg[r]);
	exit(1);
    }

    while(1) {
	r = ReadLine();
	if (r == E_EOF) return;
	if (r) {
	    Eprint("%s: %s", ErrMsg[E_ERR_READING], ErrMsg[r]);
	    exit(1);
	}
	s = FindInitialToken(&tok, CurLine);

	/* Should we ignore it? */
	if (NumIfs &&
	    tok.type != T_If &&
	    tok.type != T_Else &&
	    tok.type != T_EndIf &&
	    tok.type != T_IfTrig &&
	    ShouldIgnoreLine())
	{
	    /* DO NOTHING */
	}
	else {
	    /* Create a parser to parse the line */
	    CreateParser(s, &p);

	    switch(tok.type) {

            case T_Empty:
	    case T_Comment:
		break;

	    case T_ErrMsg:  r=DoErrMsg(&p);  break;
	    case T_Rem:     r=DoCalRem(&p, col); break;
	    case T_If:      r=DoIf(&p);      break;
	    case T_IfTrig:  r=DoIfTrig(&p);  break;
	    case T_Else:    r=DoElse(&p);    break;
	    case T_EndIf:   r=DoEndif(&p);   break;
	    case T_Include: r=DoInclude(&p); break;
	    case T_Exit:    DoExit(&p);	     break;
	    case T_Set:     r=DoSet(&p);     break;
	    case T_Fset:    r=DoFset(&p);    break;
	    case T_UnSet:   r=DoUnset(&p);   break;
	    case T_Clr:     r=DoClear(&p);   break;
	    case T_Flush:   r=DoFlush(&p);   break;
            case T_Debug:   break;  /* IGNORE DEBUG CMD */
	    case T_Dumpvars: break; /* IGNORE DUMPVARS CMD */
	    case T_Banner:  break;  /* IGNORE BANNER CMD */
	    case T_Omit:    r=DoOmit(&p);
		if (r == E_PARSE_AS_REM) {
		    DestroyParser(&p);
		    CreateParser(s, &p);
		    r=DoCalRem(&p, col);
		}
		break;
	    case T_Pop:     r=PopOmitContext(&p);     break;
	    case T_Push:    r=PushOmitContext(&p);    break;
            case T_Preserve: r=DoPreserve(&p);        break;
	    case T_RemType: if (tok.val == RUN_TYPE) {
		r=DoRun(&p);
		break;
	    } else {
		CreateParser(CurLine, &p);
		r=DoCalRem(&p, col);
		break;
	    }

	    /* If we don't recognize the command, do a REM by default */
	    /* Note:  Since the parser hasn't been used yet, we don't */
	    /* need to destroy it here. */

	    default:        CreateParser(CurLine, &p);
		r=DoCalRem(&p, col);
		break;
	    }
	    if (r && (!Hush || r != E_RUN_DISABLED)) Eprint("%s", ErrMsg[r]);

	    /* Destroy the parser - free up resources it may be tying up */
	    DestroyParser(&p);
	}
    }
}


/***************************************************************/
/*                                                             */
/*  WriteCalHeader                                             */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void WriteCalHeader(void)
#else
static void WriteCalHeader()
#endif
{
    char buf[80];
    int y, m, d;

    FromJulian(JulianToday, &y, &m, &d);
    sprintf(buf, "%s %d", MonthName[m], y);

    WriteSolidCalLine();

    PutChar('|');
    PrintCentered(buf, CalWidth-2, ' ');
    PutChar('|');
    PutChar('\n');

    WriteIntermediateCalLine();
    WriteCalDays();
    WriteIntermediateCalLine();
}

/***************************************************************/
/*                                                             */
/*  WriteCalTrailer                                            */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void WriteCalTrailer(void)
#else
static void WriteCalTrailer()
#endif
{
    PutChar('\f');
}

/***************************************************************/
/*                                                             */
/*  DoCalRem                                                   */
/*                                                             */
/*  Do the REM command in the context of a calendar.           */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE int DoCalRem(ParsePtr p, int col)
#else
static int DoCalRem(p, col)
ParsePtr p;
int col;
#endif
{

    Trigger trig;
    TimeTrig tim;
    Value v;
    int r;
    int jul;
    CalEntry *CurCol = CalColumn[col];
    CalEntry *CurPs = CalPs[col];
    CalEntry *e;
    char *s, *s2;
    static char buf[TOKSIZE];
    static char obuf[LINELEN];
    Token tok;

    /* Parse the trigger date and time */
    if ( (r=ParseRem(p, &trig, &tim)) ) return r;

/* Don't include timed reminders in calendar if -a option supplied. */
#ifdef HAVE_QUEUED
    if (DontIssueAts && tim.ttime != NO_TIME) return OK;
#endif
    if (trig.typ == NO_TYPE) return E_EOLN;
    if (trig.typ == SAT_TYPE) {
	r=DoSatRemind(&trig, &tim, p);
	if (r) return r;
	r=ParseToken(p, buf);
	if (r) return r;
	FindToken(buf, &tok);
	if (tok.type == T_Empty || tok.type == T_Comment) return OK;
	if (tok.type != T_RemType || tok.val == SAT_TYPE) return E_PARSE_ERR;
	trig.typ = tok.val;
	jul = LastTriggerDate;
	if (!LastTrigValid) return OK;
    } else {
	/* Calculate the trigger date */
	jul = ComputeTrigger(trig.scanfrom, &trig, &r);
	if (r) return r;
    }

    if (!PsCal && (trig.typ == PS_TYPE || trig.typ == PSF_TYPE)) return OK;

    /* Remove any "at" times from PS or PSFILE reminders */
    if (trig.typ == PS_TYPE || trig.typ == PSF_TYPE) tim.ttime = NO_TIME;

    /* If trigger date == today, add it to the current entry */   
    if (jul == JulianToday) {
	NumTriggered++;
	s = obuf;
	*s = 0;
	if (DoSimpleCalendar || tim.ttime != NO_TIME)
	    s += strlen(SimpleTime(tim.ttime, s));
	if (trig.typ != PS_TYPE && trig.typ != PSF_TYPE &&
	    UserFuncExists("calprefix")==1) {
	    sprintf(buf, "calprefix(%d)", trig.priority);
	    s2 = buf;
	    r = EvalExpr(&s2, &v);
	    if (!r) {
		if (!DoCoerce(STR_TYPE, &v)) {
		    strcat(s, v.v.str);
		    s += strlen(s);
		}
		DestroyValue(v);
	    }
	}
	if ( (r=DoSubst(p, s, &trig, &tim, jul, CAL_MODE)) ) return r;
	if (!*s) return OK;
	if (trig.typ != PS_TYPE && trig.typ != PSF_TYPE &&
	    UserFuncExists("calsuffix")==1) {
	    sprintf(buf, "calsuffix(%d)", trig.priority);
	    s2 = buf;
	    r = EvalExpr(&s2, &v);
	    if (!r) {
		if (!DoCoerce(STR_TYPE, &v)) {
		    strcat(s, v.v.str);
		    s += strlen(s);
		}
		DestroyValue(v);
	    }
	}
	s = obuf;
	if (!DoSimpleCalendar) while (isspace(*s)) s++;
	e = NEW(CalEntry);
	if (!e) return E_NO_MEM;
	e->text = StrDup(s);
	if (!e->text) {
	    free(e);
	    return E_NO_MEM;
	}
	e->priority = trig.priority;
	if (trig.typ == PS_TYPE || trig.typ == PSF_TYPE) {
	    e->pos = (trig.typ == PS_TYPE) ? "P" : "F";
	    e->time = NO_TIME;
	    e->next = CurPs;
	    CalPs[col] = e;
	    SortCol(&CalPs[col]);
	} else {
	    e->pos = e->text;
	    e->time = tim.ttime;
	    e->next = CurCol;
	    CalColumn[col] = e;
	    SortCol(&CalColumn[col]);
	}
    }
    return OK;
}

/***************************************************************/
/*                                                             */
/*  WriteSimpleEntries                                         */
/*                                                             */
/*  Write entries in 'simple calendar' format.                 */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void WriteSimpleEntries(int col, int jul)
#else
static void WriteSimpleEntries(col, jul)
int col, jul;
#endif
{
    CalEntry *e = CalPs[col];
    CalEntry *n;
    int y, m, d;

/* Do all the PostScript entries first, if any */
    FromJulian(jul, &y, &m, &d);
    while(e) {
	printf("%c%c%c%c%c%02d%c%02d ", *(e->pos), *(e->pos),
	       *(e->pos), *(e->pos), DATESEP,
	       m+1, DATESEP, d);
	printf("%s\n", e->text);
	free(e->text);
	n = e->next;
	free(e);
	e = n;
    }
    CalPs[col] = NULL;

    e = CalColumn[col];				     
    while(e) {
	printf("%04d%c%02d%c%02d ", y, DATESEP, m+1, DATESEP, d);
	printf("%s\n", e->text);
	free(e->text);
	n = e->next;
	free(e);
	e = n;
    }
    CalColumn[col] = NULL;
}

/***************************************************************/
/*                                                             */
/*  Various functions for writing different types of lines.    */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void WriteSolidCalLine(void)
#else
static void WriteSolidCalLine()
#endif
{
    PutChar('+');
    PrintCentered("", CalWidth-2, '-');
    PutChar('+');
    PutChar('\n');
}

#ifdef HAVE_PROTOS
PRIVATE void WriteIntermediateCalLine(void)
#else
static void WriteIntermediateCalLine()
#endif
{
    int i;

    PutChar('+');
    for (i=0; i<7; i++) {
	PrintCentered("", ColSpaces, '-');
	PutChar('+');
    }
    PutChar('\n');
}

#ifdef HAVE_PROTOS
PRIVATE void WriteCalDays(void)
#else
static void WriteCalDays()
#endif
{
    int i;
    PutChar('|');
    for (i=0; i<7; i++) {
	if (!MondayFirst)
	    PrintCentered(DayName[(i+6)%7], ColSpaces, ' ');
	else
	    PrintCentered(DayName[i%7], ColSpaces, ' ');
	PutChar('|');
    }
    PutChar('\n');
}

/***************************************************************/
/*                                                             */
/*  SimpleTime                                                 */
/*                                                             */
/*  Format the time according to simple time format.           */
/*  If out is NULL, result placed in internal static buffer.   */
/*  A trailing space is always added.                          */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PUBLIC char *SimpleTime(int tim, char *out)
#else
char *SimpleTime(tim, out)
int tim;
char *out;
#endif
{
    static buf[9];
    int h, min, hh;
   
    if (!out) out = (char *) buf;

    *out = 0;
   
    switch(ScFormat) {

    case SC_AMPM:
	if (tim == NO_TIME) sprintf(out, "        ");
	else {
	    h = tim / 60;
	    min = tim % 60;
	    if (h == 0) hh=12;
	    else if (h > 12) hh=h-12;
	    else hh=h;
	    sprintf(out, "%2d%c%02d%s ", hh, TIMESEP, min, (h>=12) ? L_PM : L_AM);
	}
	break;

    case SC_MIL:
	if (tim == NO_TIME) sprintf(out, "      ");
	else {
	    h = tim / 60;
	    min = tim % 60;
	    sprintf(out, "%02d%c%02d ", h, TIMESEP, min);
	}
	break;
    }
    return out;
}

/***************************************************************/
/*                                                             */
/*  SortCol                                                    */
/*                                                             */
/*  Sort the calendar entries in a column by time and priority */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void SortCol(CalEntry **col)
#else
static void SortCol(col)
CalEntry **col;
#endif
{
    CalEntry *cur, *prev, *next;

    cur = *col;
    prev = NULL;

/* Note that we use <= comparison rather than > comparison to preserve the
   file order of reminders which have the same time and priority */

    while (cur->next &&
	   CompareRems(0, cur->time, cur->priority,
		       0, cur->next->time, cur->next->priority,
		       SortByDate, SortByTime, SortByPrio) <= 0) {
	next = cur->next;
	/* Swap cur and next */
	if (!prev) {
	    *col = next;
	    cur->next = next->next;
	    next->next = cur;
	    prev = next;
	} else {
	    prev->next = next;
	    cur->next = next->next;
	    next->next = cur;
	    prev = next;
	}
    }
}
