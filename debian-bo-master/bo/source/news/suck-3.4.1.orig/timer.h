#ifndef _SUCK_TIMER_H
#define _SUCK_TIMER_H 1

void TimerFunc(int, long, FILE *);

enum  timer_funcs { TIMER_START, TIMER_ADDBYTES, TIMER_DISPLAY, TIMER_TOTALS, TIMER_TIMEONLY };

#endif /* _SUCK_TIMER_H */
