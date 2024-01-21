
/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 */

extern int dds_bsr(void);
extern void dds_read_block(void);
extern void dds_read_next_block(void);
extern int dds_getpos(int const dev);
extern int dds_seek(int const dev ,int const blkno);

#define DDSCM_NULL -1
#define DDSCM_OFF 0
#define DDSCM_ON 1
#define DDSCM_QUERY 2
#define DDSCM_LOG 3
#define DDSCM_LOAD 4
#define DDSCM_UNLOAD 5
#define DDSCM_FILENO 6

extern void set_compression_mode(int _device, const int _mode);

