#ifndef _SLRN_ART_H
#define _SLRN_ART_H

#ifndef SLRNPULL_CODE
extern int slrn_select_article_mode (Slrn_Group_Type *, int, int);
extern void slrn_init_article_mode (void);
extern SLKeyMap_List_Type *Slrn_Article_Keymap;
extern char *Slrn_Quote_String;
extern char *Slrn_Save_Directory;
extern char *Slrn_Header_Help_Line;
extern char *Slrn_Art_Help_Line;
extern char *Slrn_Followup_Custom_Headers;
extern char *Slrn_Reply_Custom_Headers;
extern int Slrn_Show_Author_Realname;

extern int Slrn_Show_Author;
extern int Slrn_Startup_With_Article;
extern int Slrn_Show_Thread_Subject;
extern int Slrn_Query_Next_Article;
extern int Slrn_Query_Next_Group;
extern int Slrn_Auto_CC_To_Poster;
extern int Slrn_Score_After_XOver;
extern int Slrn_Use_Tmpdir;
extern int Slrn_Sorting_Mode;
extern int Slrn_Threads_Visible;
extern int Slrn_Wrap_Mode;
extern int Slrn_Use_Header_Numbers;
extern int Slrn_Reads_Per_Update;
extern int Slrn_High_Score_Min;
extern int Slrn_Low_Score_Max;
extern int Slrn_Kill_Score_Max;
extern char *Slrn_X_Browser;
extern char *Slrn_NonX_Browser;
#if SLRN_HAS_SPOILERS
extern int Slrn_Spoiler_Char;
extern int Slrn_Spoiler_Display_Mode;
#endif
#if SLRN_HAS_TILDE_FEATURE
extern int Slrn_Use_Tildes;
#endif
extern int Slrn_New_Subject_Breaks_Threads;
extern int Slrn_Sig_Is_End_Of_Article;

#endif				       /* NOT SLRNPULL_CODE */

typedef struct Slrn_Header_Type
{
   struct Slrn_Header_Type *next, *prev;  /* threaded next/prev */
   unsigned int flags;
#define HEADER_READ			0x0001
#define HEADER_TAGGED			0x0004
#define HEADER_HIGH_SCORE		0x0008
#define HEADER_LOW_SCORE		0x0010
#define HEADER_HARMLESS_FLAGS_MASK	0x00FF 
#define HEADER_HIDDEN			0x0100
#define HEADER_NTAGGED			0x0200
#define FAKE_PARENT			0x0400
#define FAKE_CHILDREN			0x0800
#define FAKE_HEADER_HIGH_SCORE		0x1000
#define HEADER_CHMAP_PROCESSED		0x2000

   struct Slrn_Header_Type *real_next, *real_prev;
   struct Slrn_Header_Type *parent, *child, *sister;  /* threaded relatives */
   struct Slrn_Header_Type *hash_next;  /* next in hash table */
   unsigned int num_children;
   unsigned long hash;		       /* based on msgid */
   int number;			       /* server number */
   int lines;
   char *subject;			       /* malloced */
   char *from;			       /* pointers to above space */
   char *date;
   char *msgid;
   char *refs;
   char *xref;
   char *realname;		       /* NOT Null terminated */
   unsigned int realname_len;
   unsigned int tag_number;
#define MAX_TREE_SIZE 24
   unsigned char tree[MAX_TREE_SIZE];
#if SLRN_HAS_GROUPLENS
   int gl_rating;
   int gl_pred;
#endif
#if SLRN_HAS_SORT_BY_SCORE
   int score;
   int thread_score;
#endif
} Slrn_Header_Type;

extern Slrn_Header_Type *Slrn_First_Header;
extern Slrn_Header_Type *Slrn_Current_Header;

extern int slrn_goto_header (Slrn_Header_Type *, int);


typedef struct Slrn_Article_Line_Type
{
   struct Slrn_Article_Line_Type *next, *prev;
   unsigned int flags;
#define HEADER_LINE	1
#define HIDDEN_LINE	2
#define QUOTE_LINE	4
#define SIGNATURE_LINE	8
#define WRAPPED_LINE	0x10
#if SLRN_HAS_SPOILERS
# define SPOILER_LINE	0x20
#endif
   char *buf;
}
Slrn_Article_Line_Type;

extern Slrn_Header_Type *slrn_find_header_with_msgid (char *);

extern Slrn_Article_Line_Type *Slrn_Article_Lines;

extern SLRegexp_Type *Slrn_Ignore_Quote_Regexp[];

extern int slrn_pipe_article_to_cmd (char *);
extern int slrn_save_current_article (char *);

extern Slrn_Article_Line_Type *slrn_search_article (char *, char **, int, int);
extern unsigned int slrn_header_down_n (unsigned int, int);
extern unsigned int slrn_header_up_n (unsigned int, int);

extern void slrn_collapse_this_thread (Slrn_Header_Type *, int);
extern void slrn_collapse_threads (int);
extern void slrn_uncollapse_threads (int);
extern void slrn_uncollapse_this_thread (Slrn_Header_Type *, int);
extern unsigned int slrn_thread_size (Slrn_Header_Type *);
extern int slrn_is_thread_collapsed (Slrn_Header_Type *);

extern int slrn_next_unread_header (void);
extern int slrn_goto_num_tagged_header (int *);
extern int slrn_next_tagged_header (void);
extern int slrn_prev_tagged_header (void);
extern void slrn_set_article_window_size (int);
extern char *slrn_extract_header (char *, unsigned int);

/* Third argument must be zero unless caller deals with Slrn_Current_Header. */
extern Slrn_Header_Type *slrn_set_header_score (Slrn_Header_Type *, int, int);

extern int slrn_is_article_visible (void);

extern int slrn_edit_score (Slrn_Header_Type *, char *);

#if SLRN_HAS_SORT_BY_SCORE
extern int Slrn_Display_Score;
#endif


#endif				       /* _SLRN_ART_H */
