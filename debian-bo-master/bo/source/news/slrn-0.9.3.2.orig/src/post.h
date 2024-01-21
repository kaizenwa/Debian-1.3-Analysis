extern void slrn_add_signature (FILE *);
extern int slrn_add_custom_headers (FILE *, char *, int (*)(char *, FILE *));
extern int slrn_post (char *, char *, char *);
extern int slrn_post_file (char *, char *);
extern char *Slrn_Courtesy_CC_Message;
extern char *Slrn_Save_Posts_File;
extern char *Slrn_Last_Message_Id;
extern char *Slrn_Post_Custom_Headers;
extern int Slrn_Reject_Long_Lines;
