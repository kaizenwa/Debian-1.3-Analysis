/*
 * tab.h - declarations for tab handling
 * Lars Wirzenius
 *
 * Note: The first column is number 0.
 */

void tab_set_width(long);
long tab_width(void);
long tab_next(long);
long tab_next_distance(long);
