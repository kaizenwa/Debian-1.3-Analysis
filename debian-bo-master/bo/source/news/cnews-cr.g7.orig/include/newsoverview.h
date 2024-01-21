/* newsreader interface to news overview data */
struct novgroup {
	char *g_dir;
	FILE *g_stream;
	struct novart *g_first;
	struct novart *g_curr;
	HASHTABLE *g_msgids;
	HASHTABLE *g_roots;
};
struct novart {
	char *a_num;
	char *a_subj;
	char *a_from;
	char *a_date;
	char *a_msgid;
	char *a_refs;
	char *a_bytes;
	char *a_lines;		/* a waste of bits */
	char *a_others;
	/* these members are message-ids, filled in by novthread() */
	char *a_parent;
	char *a_child1;		/* first child of a chain */
	char *a_sibling;	/* next sibling in this chain */
	/* end message-ids */
	struct novart *a_nxtnum;	/* next in numeric order */
};

struct novgroup *novopen(), *novstream();	/* open a new group */
struct novart *novall(), *novnext(); /* return overview data for a group */
/* novthread(ngroup);		built threads linkage; see g_roots */
/* novclose(ngroup);		frees resources associated with ngroup */
