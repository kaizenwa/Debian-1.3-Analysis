/*
   Time-stamp: <96/07/19 20:18:37 yusuf>

   $Id: tapeio.h,v 1.7 1996/07/27 20:42:16 yusuf Exp $
*/


/* Functions in tapeio.h */

extern char *make_tt(char tt);
extern _errstat tape_readheader(struct tape_header *t, _s8 allowz);
extern _errstat check_tape(WINDOW *mes, int line, _s32 tape_required);
extern _errstat tape_rewind(void);
extern _errstat tape_seek(int toblock);
extern _errstat tape_eom(int allowerr);
extern _errstat tape_fsf(int count, int allowerr);
extern _errstat tape_erase(void);
extern _errstat tape_goto_block(WINDOW *mes, int line, _s32 block);
extern _errstat ntape_open(int mode);
extern _errstat tape_open(int mode);
extern _errstat write_tape_header(struct tape_header *tdh);
extern _errstat wait_finish_writing(void);
extern _errstat flush_buffers(void);
extern int tape_read(_vptr buf, size_t count);
extern int tape_write(_vptr buf, size_t count);
extern _errstat tape_read_s32(_s32 *x);
extern _errstat tape_read_namelen(char *s);
extern _errstat tape_read_fi(struct file_info *fi);
extern _errstat tape_read_volheader(struct volume_header *vh, int allower);
extern _errstat tape_write_fi(struct file_info *fi);
extern _errstat tape_write_namelen(char *s);
extern _errstat tape_write_volheader(struct volume_header *vh);
extern _errstat tape_close(void);
extern _errstat get_tape_header(WINDOW *mess, int line, struct tape_header *tdh);
extern _errstat is_regfile(int d);
extern _errstat tape_get_blk_size(int);
extern  void tape_set_blk_size(void);
extern _errstat read_volheader(struct volume_header *vh, _s8 read_vh, _s8 into_mem);
extern _errstat goto_end_vol(WINDOW *mes, int line, _s32 vol, _s32 at_vol, _s8 get_th, _s8 to_end);
