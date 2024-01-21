#ifndef _SLRN_SERVER_H
#define _SLRN_SERVER_H

#include "nntpcodes.h"

typedef struct 
{
   int (*po_start)(void);
   int (*po_end)(void);
   int (*po_printf)(char *, ...);
   int (*po_puts)(char *);
   int po_can_post;
} Slrn_Post_Obj_Type;

typedef struct 
{
   int (*sv_select_group) (char *, int *, int *);
   char *(*sv_read_line) (char *, unsigned int);
   void (*sv_close) (void);
   int (*sv_initialize) (void);
   int (*sv_select_article) (int, char *);
   int (*sv_put_server_cmd) (char *, char *, unsigned int);
   int (*sv_xpat_cmd) (char *, int, int, char *);

   int (*sv_xhdr_command) (char *, int, char *, unsigned int);
   int (*sv_xgtitle_cmd) (char *);

   int (*sv_has_cmd) (char *);
   int (*sv_list_newsgroups) (void);
   int (*sv_list_active) (void);

   int sv_has_xover;
   int (*sv_nntp_xover) (int, int);
   int (*sv_nntp_head) (int, char *, int *);
   int (*sv_nntp_next) (int *);
   char *sv_name;
   
} 
Slrn_Server_Obj_Type;

extern Slrn_Server_Obj_Type *Slrn_Server_Obj;
extern Slrn_Post_Obj_Type *Slrn_Post_Obj;

extern int Slrn_Server_Min, Slrn_Server_Max;
extern char *Slrn_Current_Group_Name;

#if SLRN_HAS_NNTP_SUPPORT
extern int Slrn_Query_Reconnect;
extern char *Slrn_NNTP_Server_Name;
#endif

#if SLRN_HAS_INEWS_SUPPORT
extern char *Slrn_Inews_Pgm;
#endif

extern int slrn_init_objects (void);
extern int slrn_select_post_object (int);
extern int slrn_select_server_object (int);
extern int slrn_parse_object_args (char *, char **, int);

extern char *slrn_getserverbyfile(char *);

#if SLRN_HAS_SPOOL_SUPPORT
extern char *Slrn_Inn_Root;
extern char *Slrn_Spool_Root;
extern char *Slrn_Nov_Root;
extern char *Slrn_Nov_File;
extern char *Slrn_Active_File;
extern char *Slrn_ActiveTimes_File;
extern char *Slrn_Newsgroups_File;
#endif

#if SLRN_HAS_PULL_SUPPORT
extern int Slrn_Use_Pull_Post;
#endif

extern int Slrn_Server_Id;
extern int Slrn_Post_Id;

#endif				       /* SLRN_SERVER_H */
