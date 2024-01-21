/*
 * wmslib/src/but/textin.h, part of wmslib (Library functions)
 * Copyright (C) 1995 William Shubert.
 * See "configure.h.in" for more copyright information.
 *
 * Includes for textin.c
 */

#ifndef  _BUT_TEXTIN_H_
#define  _BUT_TEXTIN_H_  1

/**********************************************************************
 * Functions
 **********************************************************************/
extern But  *butTextin_create(ButOut (*func)(But *but, const char *value),
			      void *packet, ButWin *win, int layer,
			      int flags, const char *text, int maxlen);
extern void  butTextin_setCallback(But *but,
				   ButOut (*func)(But *but, const char *value),
				   void *packet);
extern void  butTextin_set(But *but, const char *str);
extern const char  *butTextin_get(But *but);
/*
 * If you set hidden to TRUE, then all characters appear as only "*"s.  This
 *   is nice if you are asking for a password.
 */
extern void  butTextin_setHidden(But *but, bool hidden);

#endif  /* _BUT_TEXTIN_H_ */
