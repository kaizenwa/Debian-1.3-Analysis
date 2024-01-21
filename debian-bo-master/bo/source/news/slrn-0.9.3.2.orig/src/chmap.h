#ifndef _SLRN_CHMAP_H
#define _SLRN_CHMAP_H

extern int slrn_set_charset (char *);
extern int slrn_chmap_fix_file (char *);
extern void slrn_chmap_fix_body (void);
extern void slrn_chmap_fix_headers (void);

#if SLRN_HAS_CHARACTER_MAP
extern char *Slrn_Charset;
#endif

#endif
