#ifndef _SLRN_GRPLENS_H
#define _SLRN_GRPLENS_H

extern int Slrn_Use_Group_Lens;
extern int slrn_init_grouplens (void);
extern void slrn_close_grouplens (void);
extern int slrn_put_grouplens_scores (void);
extern int slrn_get_grouplens_scores (void);
extern void slrn_group_lens_rate_article (Slrn_Header_Type *, int, int);

extern char *Slrn_GroupLens_Pseudoname;
extern char *Slrn_GroupLens_Host;
extern int slrn_grouplens_add_group (char *);

extern int Slrn_GroupLens_Port;

#endif


