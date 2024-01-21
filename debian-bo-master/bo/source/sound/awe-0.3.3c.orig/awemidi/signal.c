/*
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

typedef struct HandlerList {
	void (*handler)();
	struct HandlerList *next;
} HandlerList;

static HandlerList *hlist[NSIG];

void call_handlers(int sig)
{
	HandlerList *p;
	for (p = hlist[sig]; p; p = p->next)
		p->handler(sig);
}

void exit_handlers(int sig)
{
	call_handlers(sig);
	exit(0);
}

void add_signal(int sig, void (*handler)(), int exit_after)
{
	HandlerList *rec = (HandlerList*)malloc(sizeof(HandlerList));
	if (rec == NULL) {
		fprintf(stderr, "can't malloc handler\n");
		exit(1);
	}
	rec->handler = handler;
	rec->next = hlist[sig];
	hlist[sig] = rec;
	if (exit_after)
		signal(sig, exit_handlers);
	else
		signal(sig, call_handlers);
}
