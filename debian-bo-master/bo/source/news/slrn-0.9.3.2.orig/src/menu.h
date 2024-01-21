extern void slrn_update_article_menu (void);
extern void slrn_update_group_menu (void);
extern int slrn_execute_menu (int);
extern int slrn_sbox_sorting_method (void);


typedef struct
{
   char *title;
   char **lines;
}
Slrn_Select_Box_Type;

extern int slrn_select_box (Slrn_Select_Box_Type *);


