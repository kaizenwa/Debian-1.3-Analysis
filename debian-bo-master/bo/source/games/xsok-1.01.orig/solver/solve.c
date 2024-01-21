#include <time.h>
#include <assert.h>
#include "BFS.h"

#ifdef HP
#define HASHBITS 24		/* 16 M entries => 48 MB RAM */
#define MEM	201326592	/* 192 MB total */
#else
#define HASHBITS 20		/* 1 M entries => 3 MB RAM */
#define MEM	(13152*1024)	/* 14 MB total */
#endif

#define MAXBOXES 32
typedef unsigned char zchar;
int xlevel;

void fatal(const char *msg, ...) {
    va_list args;

    va_start(args, msg);
    vfprintf(stderr, msg, args);
    fprintf (stderr, "\n");
    exit (1);
}

void *malloc_(size_t n) {
    void *p;
    if (!n)
	return NULL;	/* since malloc(0) may return NULL */
    p = malloc(n);
    if (!p)
	fatal("out of memory");
    return p;
}


/* const char *xsokdir = "/usr/games/lib/xsok/Sokoban"; */
const char *xsokdir = "../lib/Sokoban";
#define MAXROW	22
#define MAXCOL	32

int numrows, numcols, plx, ply, orgplx, orgply;
unsigned char map[MAXROW][MAXCOL], map2[MAXROW][MAXCOL], dist[MAXROW][MAXCOL];

static void dfs(int x, int y) {
    if (x < 0 || y < 0 || x >= numcols || y >= numrows)
	return;
    if (!(map[y][x] & 4) || (map[y][x] & 0x10))
	return;
    map[y][x] |= 0x10;
    dfs(x-1, y);
    dfs(x, y-1);
    dfs(x+1, y);
    dfs(x, y+1);
}

/* Bits in map: 0 = box there, 1 = target square, 2 = player allowed, 3 = box allowed */
#define BOX	1
#define TARGET	2
#define MAYGO	4
#define MAYBOX	8
#define DONE	0x10

#define MAXILL 2000
int pairs = 0;
int mbx[256], mby[256];
struct illegal {
   int x1, y1, x2, y2;
} illpairs[MAXILL];

static void add_illegal(int x1, int y1, int x2, int y2) {
    int i;
    struct illegal *ip;
    if (pairs == MAXILL)
	return;		/* cannot store more illegal pairs */
    if (!(map[y1][x1] & map[y2][x2] & MAYBOX))
	return;		/* cannot happen at all */
    ip = illpairs;
    for (i = 0; i < pairs; ++i, ++ip) {
	if (ip->x1 == x1 && ip->y1 == y1 && ip->x2 == x2 && ip->y2 == y2)
	    return;
	if (ip->x1 == x2 && ip->y1 == y2 && ip->x2 == x1 && ip->y2 == y1)
	    return;
    }
    ip->x1 = x1;
    ip->y1 = y1;
    ip->x2 = x2;
    ip->y2 = y2;
    printf("illegal pair: (%d,%d) with (%d,%d)\n", x1, y1, x2, y2);
    ++pairs;
}

static int check_pairs(void) {
    int i;
    struct illegal *ip;
    ip = illpairs;
    for (i = 0; i < pairs; ++i, ++ip)
	if (map[ip->y1][ip->x1] & map[ip->y2][ip->x2] & BOX)
	    return 0;
    return 1;	/* cannot see a reason to reject */
}

void collect_illegal_pairs(void) {
    int x, y;
    for (y = 1; y < numrows-1; ++y)
	for (x = 1; x < numcols-1; ++x)
	    if (!(map[y][x] & MAYGO)) {	/* there is a wall */
		if (map[y+1][x] & MAYGO) {
		    if (x > 1 &&
			(map[y][x-2] & MAYGO) && !(map[y+1][x-2] & MAYGO))
			    add_illegal(x-1, y, x-1, y+1);
		    if (x < numcols-2 &&
			(map[y][x+2] & MAYGO) && !(map[y+1][x+2] & MAYGO))
			    add_illegal(x+1, y, x+1, y+1);
		}
		if (y < numcols-2 && (map[y+2][x] & MAYGO)) {
		    if (!(map[y+2][x-1] & MAYGO) && (map[y][x-1] & MAYGO))
			add_illegal(x-1, y+1, x, y+1);
		    if (!(map[y+2][x+1] & MAYGO) && (map[y][x+1] & MAYGO))
			add_illegal(x+1, y+1, x, y+1);
		}
	    }
}

void ParseMapFile(int level) {
    FILE *fp;
    int x, y;
    char s[256];

    sprintf(s, "%s/screen.%02d", xsokdir, level);
    if (!(fp = fopen(s, "r")))
	fatal("cannot open level\n");

    memset(map, 0, sizeof(map));

    for (y = 1; fgets(s, sizeof(s), fp); ++y) {
	/* parse s to map */
	int c;
	if (*s == ';') {
	    --y;
	    continue;
	}
	if (y == MAXROW-1)
	    fatal("Level is too big\n");
	for (x = 1; (c=s[x-1]) && c != '\n'; ++x) {
	    if (x == MAXCOL-1)
		fatal("Level is too wide\n");
	    if (x > numcols)
		numcols = x;
	    switch (c) {
	    case '#':
		map[y][x] = 0;	/* no go */
		break;
	    case '.':
		map[y][x] = 0x0f-BOX;	/* no go */
		break;
	    case '*':
		map[y][x] = 0x0f;	/* target + box! */
		break;
	    case '$':
		map[y][x] = 0x0f-TARGET;	/* no go */
		break;
	    case '@':
		orgplx = plx = x;
		orgply = ply = y;
	    case ' ':
		map[y][x] = 0x0f-TARGET-BOX;	/* player & box allowed */
		break;
	    default:
		fatal("Cannot handle character 0x%02x\n", c);
	    }
	}
    }
    fclose(fp);
    numrows = y+1;
    numcols += 2;
    /* change floor to void */
    dfs(plx, ply);
    for (x = 0; x < MAXCOL; ++x)
	for (y = 0; y < MAXROW; ++y) {
	    if (!(map[y][x] & 0x10))
		map[y][x] = 0;
	    else
		map[y][x] &= 0x0f;
	}
    
}


void mask_map(void) {
    int x, y;
    for (y = 2; y < numrows-2; ++y)
	for (x = 2; x < numcols-2; ++x)
	    if ((map[y][x] & 6) == MAYGO) { /* not dest, player allowed */
		if (!(MAYGO & (map[y-1][x]|map[y][x-1])))
		    map[y][x] &= ~8;	/* no box allowed here */
		if (!(MAYGO & (map[y+1][x]|map[y][x-1])))
		    map[y][x] &= ~8;	/* no box allowed here */
		if (!(MAYGO & (map[y-1][x]|map[y][x+1])))
		    map[y][x] &= ~8;	/* no box allowed here */
		if (!(MAYGO & (map[y+1][x]|map[y][x+1])))
		    map[y][x] &= ~8;	/* no box allowed here */
	    }
}

void print_map(void) {
    int x, y;
    for (y = 1; y < numrows-1; ++y) {
	for (x = 1; x < numcols-1; ++x)
	    if (map[y][x] & MAYBOX) {
		if (map[y][x] & BOX)
		    putchar('$');
		else
		    if (map[y][x] & DONE)
			putchar(':');
		    else
			putchar(' ');
	    } else  {
		if (map[y][x] & MAYGO)
		    putchar('.');
		else
		    putchar('#');
	    }
	putchar('\n');
    }
}
int maygo = 0, maybox = 0, boxes = 0;

void statistics(void) {
    int x, y;
    for (y = 1; y < numrows-1; ++y)
	for (x = 1; x < numcols-1; ++x) {
	    if (map[y][x] & MAYBOX) {
		mbx[maybox] = x;
		mby[maybox] = y;
		++maybox;
	    } else
		if (map[y][x] & MAYGO)
		    ++maygo;
	    if (map[y][x] & BOX)
		++boxes;
	}
    maygo += maybox;
    printf("%d squares for boxes, %d boxes, %d total squares\n",
	   maybox, boxes, maygo);
    {   double num;
	num = maybox;
	for (x = 2; x <= boxes; ++x) {
	    num *= maybox - (x-1);
	    num /= x;
	}
	printf("%e possibilities\n", num);
    }
    if (boxes > MAXBOXES || maybox > 256)
	fatal("Level too large");
}
zchar bitpos[2+MAXBOXES];
zchar solpos[MAXBOXES];
int level = 1;

static void reset_done(void) {
    int x, y;
    for (y = 1; y < numrows-1; ++y)
	for (x = 1; x < numcols-1; ++x)
	    map[y][x] &= ~DONE;
}

static void pldfs(int x, int y) {
    if (!(map[y][x] & MAYGO) || (map[y][x] & BOX) || (map[y][x] & DONE))
	return;
    map[y][x] |= DONE;
    pldfs(x-1, y);
    pldfs(x, y-1);
    pldfs(x+1, y);
    pldfs(x, y+1);
}

static void norm_playerpos(void) {
    int x, y;
    reset_done();
    pldfs(plx, ply);
    for (y = 1; y < numrows-1; ++y)
	for (x = 1; x < numcols-1; ++x)
	    if (map[y][x] & DONE) {
		plx = x;
		ply = y;
		return;
	    }
}

void sol_to_bits(void) {
    int i, j, x, y;
    j = 0;
    for (i = 0; i < maybox; ++i) {
	x = mbx[i];
	y = mby[i];
	if (map[y][x] & TARGET)
	    solpos[j++] = i;
    }
    printf("solpos has %d targets:", j);
    for (i = 0; i < j; ++i)
	printf(" %2d", solpos[i]);
    printf("\n");
    if (j != boxes)
	fatal("different tnumber of targets and boxes!\n");
}
void pos_to_bits(void) {
    int i, j, x, y;
    norm_playerpos();
    memset(bitpos, 0, sizeof(bitpos));
    bitpos[0] = plx;
    bitpos[1] = ply;
    j = 2;
    for (i = 0; i < maybox; ++i) {
	x = mbx[i];
	y = mby[i];
	if (map[y][x] & BOX)
	    bitpos[j++] = i;
    }
}
void bits_to_pos(void) {
    int i, j, x, y;
    for (y = 1; y < numrows-1; ++y)
	for (x = 1; x < numcols-1; ++x)
	    map[y][x] &= ~BOX;
    plx = bitpos[0];
    ply = bitpos[1];
    for (j = 2; j < boxes + 2; ++j) {
	i = bitpos[j];
	map[mby[i]][mbx[i]] |= BOX;
    }
    reset_done();
    pldfs(plx, ply);
}

unsigned char smap[MAXROW][MAXCOL];
#if 0
int best = 0;
static int count_good(void) {
    int x, y, good;
    good = 0;
    for (y = 2; y < numrows-2; ++y)
	for (x = 2; x < numcols-2; ++x)
	    if ((map[y][x] & (BOX|TARGET)) == (BOX|TARGET))
		++good;
    if (good <= best)
	return good;
    best = good;
    printf("%d boxes good!\n", best);

    good = 0;
    for (y = 2; y < numrows-2; ++y)
	for (x = 2; x < numcols-2; ++x)
	    if ((map[y][x] & (BOX)) == (BOX))
		++good;
    printf("%d boxes total!\n", good);
    good = 0;
    for (y = 2; y < numrows-2; ++y)
	for (x = 2; x < numcols-2; ++x)
	    if ((map[y][x] & TARGET))
		++good;
    printf("%d targets total!\n", good);
}
#else
#define count_good()
#endif

#define NOT(c)	(!(c & MAYGO) || (c & BOX))

#define TEST(dx,dy,dir) 	\
		    if (!(map2[y+dy][x+dx]&BOX) && (map[y+dy][x+dx]&BOX)) { \
		       /* d+dx,y+dy is the new posn. */ \
		       move_player_to(x-(dx),y-(dy), fp); \
		       putc(dir,fp); orgplx = x; orgply=y; goto naus;}

#define STEP(dx,dy) if ((map2[y+dy][x+dx] & (MAYGO|BOX)) == MAYGO && dist[y+dy][x+dx] > d) \
   (dist[remy[wr] = y+dy][remx[wr]= x+dx] = d), ++wr;

void move_player_to(int xx, int yy, FILE *fp) {
    unsigned char rem[100];
    int length, i;
    int rd = 0, wr = 1, remx[1000], remy[1000];
    memset(dist, 0xff, sizeof(dist));
    dist[remy[0] = ply][remx[0] = plx] = 0;
    while (rd < wr) {
	int x, y, d;
	x = remx[rd];
	y = remy[rd++];
	d = 1 + dist[y][x];
	STEP(1,0);
	STEP(0,1);
	STEP(-1,0);
	STEP(0,-1);
    }
    if (dist[yy][xx] == 0xff) {
	fclose(fp);
	fatal("ile #1\n");
    }
    length = dist[yy][xx];
    for (i = length-1; i>= 0; --i) {
	     if (dist[yy-1][xx] == i) { --yy; rem[i] = 2; }
	else if (dist[yy][xx-1] == i) { --xx; rem[i] = 3; }
	else if (dist[yy+1][xx] == i) { ++yy; rem[i] = 0; }
	else if (dist[yy][xx+1] == i) { ++xx; rem[i] = 1; }
	else fatal("ile #2");
    }
    for (i = 0; i < length; ++i)
	putc(rem[i], fp);
}

void write_solution(long entrynr) {
    long *positions;
    char fname[100];
    FILE *fp;
    int i;
    positions = malloc_(sizeof(long) * (level+1));
    for (i = level; i >= 0; --i) {
	positions[i] = entrynr;
	entrynr = BFS_getentry(entrynr, bitpos);
	bits_to_pos();
	printf("Position after %d pushes: player at %d,%d\n", i, plx, ply);
	print_map();
	fflush(stdout);
	if (i > 0)
	    assert(entrynr >= 0);
	else
	    assert(entrynr < 0);
    }
    sprintf(fname, "Sokoban.%02d.sol", xlevel);
    fp = fopen(fname, "wb");
    /* write the "simple" magic */
    putc(0, fp);
    putc(0, fp);
    putc(0x74, fp);
    putc(0x1c, fp);
    
    plx = orgplx;
    ply = orgply;
    BFS_getentry(positions[0], &bitpos);
    bits_to_pos();
    for (i = 1; i <= level; ++i) {
	int x, y;
	memcpy(map2, map, sizeof(map));	/* the current state */
	BFS_getentry(positions[i], &bitpos);
	bits_to_pos();
	/* make a diff map2 => map and move the player */
	for (y = 2; y < numrows-2; ++y)
	    for (x = 2; x < numcols-2; ++x)
		/* map2 is old and map is new */
		if ((map2[y][x]&BOX) && !(map[y][x]&BOX)) {
		    /* there are 4 possibilities... (x,y) is the new player pos */
		    plx = orgplx;
		    ply = orgply;
		    TEST( 0,-1,0);
		    TEST(-1, 0,1);
		    TEST( 0, 1,2);
		    TEST( 1, 0,3);
		}
	fatal("ile 3");
    naus:
	;
    }
    fclose(fp);
}


static void printtime(void) {
    fflush(stdout);
    system("date");
}

#ifndef HP
inline
#endif
static void try_possible_state(x, y, dx, dy) {
    if (!(map[y][x] & TARGET)) {
	if (!dx) {
	    if (NOT(map[y+dy][x]) && (
				      (NOT(map[y][x-1]) && NOT(map[y+dy][x-1])) ||
				      (NOT(map[y][x+1]) && NOT(map[y+dy][x+1]))
				      ))
		return;
	    /* no new forbidden square */
	} else {
	    if (NOT(map[y][x+dx]) && (
				      (NOT(map[y-1][x]) && NOT(map[y-1][x+dx])) ||
				      (NOT(map[y+1][x]) && NOT(map[y+1][x+dx]))
				      ))
		return;
	    /* no new forbidden square */
	}
    }
    if (!check_pairs())	/* is this position stuck? */
	return;
    pos_to_bits();
#if 0
    if (level < 3) {
	printf("Storing new pos:\n");
	print_map();
	printf("%04lx %04lx\n", ((long *)bitpos)[1], ((long *)bitpos)[0]);
    }
#endif
    if (!memcmp(solpos, bitpos+2, boxes)) {
	printf("found the solution with %d pushes!\n", level);
	printtime();
	write_solution(BFS_store(bitpos));
	exit(0);
    }
    count_good();
    BFS_store(bitpos);
}

#define TRY(dx,dy) { \
    if ((map[y+dy][x+dx] & BOX) && (map[y+dy+dy][x+dx+dx] & (MAYBOX|BOX)) == MAYBOX) { \
	map[y+dy][x+dx] &= ~BOX; map[y+dy+dy][x+dx+dx] |= BOX;	\
        try_possible_state(x+dx+dx,y+dy+dy, dx, dy);		\
   	memcpy(map, smap, sizeof(map));				\
    }}

static void try_all_pushes(void) {
    int x, y;
    memcpy(smap, map, sizeof(map));
    for (y = 2; y < numrows-2; ++y)
	for (x = 2; x < numcols-2; ++x)
	    if (map[y][x] & DONE) {
		/* we may push from here */
		plx = x;
		ply = y;
		TRY(-1,0);
		TRY(1,0);
		TRY(0,1);
		TRY(0,-1);
	    }
}


int main(int argc, char *argv[]) {
    size_t ndata;

    if (argc == 2)
	level = atoi(argv[1]);
    else
	level =1;
    xlevel = level;

    ParseMapFile(level);
    /* mask out squares not allowed for a box */
    system("uname -a");
    printtime();
    mask_map();
    print_map();
    statistics();
    collect_illegal_pairs();
    sol_to_bits();

    printf("mem = %ld, mem for hash = %ld\n", MEM, (3UL << HASHBITS));
    ndata = (MEM - (3UL << HASHBITS)) / (boxes + 5);
    if (ndata >= (3UL << (HASHBITS-2)))
	ndata = (3UL << (HASHBITS-2));
    printf("Using %d hashbits and %ld data entries (%d boxes)\n", HASHBITS,
	   ndata, boxes);
    BFS_init(2 + boxes, 2 + boxes, HASHBITS, ndata, 1);	/* set keylength */
    pos_to_bits();
    BFS_store(bitpos);
    level = 0;
    while (BFS_newlevel()) {	/* there are some entries */
	++level;
	/* fprintf(stderr, "searching at level %d\n", level); */
	while (BFS_retrieve(bitpos)) {
	    bits_to_pos();
	    try_all_pushes();
	}
    }
    printf("no solution found\n");
    printtime();
    return 0;
}
