#if JED_HAS_BUFFER_LOCAL_VARS

typedef struct
{				      
   char name[32];		       /* first char is len of name */
   unsigned int type;
   union
     {
	char *s;
	int i;
     }
   v;
}
Jed_BLocal_Type;

#define MAX_BLOCAL_VARS_PER_TABLE	10
typedef struct _Jed_BLocal_Table_Type
{
   unsigned int num;
   Jed_BLocal_Type local_vars[MAX_BLOCAL_VARS_PER_TABLE];
   struct _Jed_BLocal_Table_Type *next;
}
Jed_BLocal_Table_Type;

extern void jed_delete_blocal_vars (Jed_BLocal_Table_Type *);
extern void jed_make_blocal_var (void);
extern void jed_set_blocal_var (void);
extern void jed_get_blocal_var (char *);

#endif				       /* JED_HAS_BUFFER_LOCAL_VARS */
