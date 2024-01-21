/* $Id: hash.h,v 1.2 1996/04/19 09:16:51 torsten Exp $ */

/* hash.c */
void *HashCreate(int (*cmp) (void *, void *), void (*free) (void *), int nelem);
void HashDestroy(void *t);
void *HashFind(void *t, int value, void *val);
int HashAdd(void *t, int value, void *val);
int HashRemove(void *t, int value, void *elem);
