typedef struct mk_pair {
	struct mk_pair *next;
	unsigned char upper;
	unsigned char lower;
} mk_pair_t;

extern mk_pair_t *blanks;
extern mk_pair_t *cntrls;
extern mk_pair_t *digits;
extern mk_pair_t *hexs;
extern mk_pair_t *lowers;
extern mk_pair_t *nographs;
extern mk_pair_t *puncts;
extern mk_pair_t *spaces;
extern mk_pair_t *uppers;

extern mk_pair_t *ul_pairs;
