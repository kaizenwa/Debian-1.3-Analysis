extern int Slrn_Apply_Score;
extern int slrn_score_header (Slrn_Header_Type *, char *);
extern int slrn_read_score_file (char *);
extern void slrn_close_score (void);
extern int slrn_open_score (char *);
#define SLRN_XOVER_SCORING 1
#define SLRN_EXPENSIVE_SCORING 2
extern int Slrn_Perform_Scoring;
