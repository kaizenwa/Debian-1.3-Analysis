     /* Screen cell types */
#define AT_DIR     0x1
#define AT_REG     0x2
#define AT_BAD     0x4
#define AT_GROUP   0x8
#define AT_SUPER   0x10
#define AT_KERNEL  0x20
#define AT_BITMAP  0x40
#define AT_INODE   0x80
#define AT_DATA    0x100
#define AT_READ    0x200
#define AT_WRITE   0x400

#define AT_FRAG     0x1000
#define AT_SELECTED 0x2000

extern int voyer_mode;

void init_screen(ulong blocks);
void done_screen(int wait_key);   

void set_attr(ulong block,ushort attr);
void clear_attr(ushort attr);
void display_map(void);
void update_display(void); /* Only map part is updated */

void display_legend(ushort attr);

void add_comment(char *text);
void display_comments(char *title);
void clear_comments(void);

void stat_line(const char *s, ...);
 

