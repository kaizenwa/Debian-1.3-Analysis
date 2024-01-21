extern int slrn_rfc1522_decode_string (char *);

extern FILE *slrn_mime_encode (FILE *);
extern void slrn_mime_header_encode (char *, unsigned int);
extern void slrn_mime_process_article (void);
extern void slrn_mime_add_headers (FILE *);
extern int slrn_mime_scan_file (FILE *);
extern void slrn_mime_article_init (void);
extern int slrn_mime_call_metamail (void);
extern int slrn_check_rfc1522 (char *);

extern char *Slrn_Mime_Display_Charset;
extern int Slrn_Use_Mime;
extern int Slrn_Mime_Was_Modified;
extern int Slrn_Mime_Needs_Metamail;
extern int Slrn_Mime_Was_Parsed;
extern int Slrn_Use_Meta_Mail;
extern char *Slrn_MetaMail_Cmd;
