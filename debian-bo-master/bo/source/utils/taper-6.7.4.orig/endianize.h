/*
   Time-stamp: <96/07/19 20:16:33 yusuf>

   $Id: endianize.h,v 1.2 1996/07/27 20:42:09 yusuf Exp $
*/


/* Routines for endianizing things */


extern _time_t little2machtime(_time_t *s);
extern _u32 mach2littleu32(_u32 *s);
extern _u32 little2machu32(_u32 *s);
extern _u16 mach2littleu16(_u16 *s);
extern _u16 little2machu16(_u16 *s);
extern _s32 mach2littles32(_s32 *s);
extern _s32 little2machs32(_s32 *s);
extern _s16 mach2littles16(_s16 *s);
extern _s16 little2machs16(_s16 *s);
extern void tape_header_endianize2mach(struct tape_header *t);
extern void tape_header_endianize2little(struct tape_header *t);
extern void fi_endianize2mach(struct file_info *fi, struct file_info *fi1);
extern void fi_endianize2little(struct file_info *fi, struct file_info *f1);
extern void ifd_endianize2little(struct info_file_header *ifd, struct info_file_header *ifd1);
extern void ifd_endianize2mach(struct info_file_header *ifd, struct info_file_header *ifd1);
extern void volheader_endianize2little(struct volume_header orgvh, 
				struct volume_header *newvh);
extern void volheader_endianize2mach(struct volume_header orgvh, 
			      struct volume_header *newvh);
extern void vti_endianize2little(struct volume_tape_info *vti, struct volume_tape_info *vti1);
extern void vti_endianize2mach(struct volume_tape_info *vti, struct volume_tape_info *vti1);
extern void tsi_endianize2little(struct tape_size_info *tsi, struct tape_size_info *tsi1);
extern void tsi_endianize2mach(struct tape_size_info *tsi, struct tape_size_info *tsi1);
