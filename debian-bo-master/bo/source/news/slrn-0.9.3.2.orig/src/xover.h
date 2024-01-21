#ifndef _SLRN_XOVER_H
#define _SLRN_XOVER_H
/* In this structure, only subject_malloced will be malloced.  All other 
 * pointers point to a location in that space.  It is done this way because
 * art.c uses this convention and the pointer can just be passed to it.
 */

typedef struct
{
   int id;
   char *subject_malloced;
   char *from;
   char *date;
   char *message_id;
   char *references;
   char *xref;
   int bytes;
   int lines;
}
Slrn_XOver_Type;

extern char *slrn_get_extra_xover_header (char *);
extern void slrn_map_xover_to_header (Slrn_XOver_Type *, Slrn_Header_Type *);

#ifndef SLRNPULL_CODE
extern int slrn_xover_for_msgid (char *, Slrn_XOver_Type *);
extern int slrn_open_xover (int, int);
extern int slrn_read_xover (Slrn_XOver_Type *);
extern void slrn_close_xover (void);

extern void slrn_open_suspend_xover (void);
extern void slrn_close_suspend_xover (void);
#endif

#endif				       /* _SLRN_XOVER_H */
