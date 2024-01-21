extern int slrn_select_group_mode (void);
extern void slrn_group_quit (void);
extern void slrn_select_next_group (void);
extern void slrn_select_prev_group (void);
extern int slrn_group_select_group (void);
extern int slrn_read_newsrc (int);
extern int slrn_write_newsrc (void);
extern void slrn_mark_article_as_read (char *, long);
extern void slrn_check_new_groups (int);
extern void slrn_init_group_mode (void);
extern void slrn_get_group_descriptions (void);
extern int slrn_read_group_descriptions (void);
extern SLKeyMap_List_Type *Slrn_Group_Keymap;
extern void slrn_post_cmd (void);
extern unsigned int slrn_group_up_n (unsigned int);
extern unsigned int slrn_group_down_n (unsigned int);
extern int slrn_group_search (char *);
extern void slrn_catchup_group (void);
extern void slrn_uncatchup_group (void);

typedef struct Slrn_Range_Type 
{
   struct Slrn_Range_Type *next;
   struct Slrn_Range_Type *prev;
   int min, max;
} Slrn_Range_Type;

typedef struct Slrn_Group_Type
{
   struct Slrn_Group_Type *next;
   struct Slrn_Group_Type *prev;
   unsigned int flags;
#define GROUP_UNSUBSCRIBED		0x001
#define GROUP_NEW_GROUP_FLAG		0x002
#define GROUP_HARMLESS_FLAGS_MASK 	0x0FF
#define GROUP_HIDDEN			0x100
#define GROUP_TOUCHED			0x200
#define GROUP_PROCESSED			0x400

#define MAX_GROUP_NAME_LEN 80
   char name[MAX_GROUP_NAME_LEN + 1];
   unsigned long hash;
   struct Slrn_Group_Type *hash_next;
   
   Slrn_Range_Type range;		       /* the first range corresponds to
						* what the server has.  next ranges
						* correspond to what has been read.
						*/
   int unread;
   char *descript;		       /* description of the group */
}
Slrn_Group_Type;

extern Slrn_Group_Type *Slrn_Group_Current_Group;
/* See important comment in group.c about this. */
extern void slrn_add_group_ranges (Slrn_Group_Type *, int, int);

extern char *Slrn_Group_Help_Line;

extern int Slrn_Groups_Dirty;
extern int Slrn_Group_Display_Descriptions;
extern int Slrn_Group_Description_Column;
extern int *Slrn_Prefix_Arg_Ptr;
extern int Slrn_No_Backups;
extern int Slrn_Unsubscribe_New_Groups;
extern int Slrn_List_Active_File;
extern int Slrn_Query_Group_Cutoff;
extern int Slrn_Prompt_Next_Group;
extern int Slrn_Use_Xgtitle;
extern int Slrn_Write_Newsrc_Flags;

extern SLscroll_Window_Type Slrn_Group_Window;
