/*
   Time-stamp: <96/07/19 20:12:52 yusuf>

   $Id: endianize.c,v 1.2 1996/07/27 20:42:08 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: endianize.c,v 1.2 1996/07/27 20:42:08 yusuf Exp $";
#endif /* lint */



/* Routines for endianizing things */


#include "taper.h"


 _time_t mach2littletime(_time_t *s)
{
/* Makes the time of this processor into little endian for
 * the tape.
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_time_t)];
    int  c;
    
    for (c=0; c<sizeof(_time_t); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_time_t)-c-1];
    return *((_time_t *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}
    

 _time_t little2machtime(_time_t *s)
{
/* Makes the time read from tape (which will be little endian)
 * into the endian required by this machine
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_time_t)];
    int  c;
    
    for (c=0; c<sizeof(_time_t); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_time_t)-c-1];
    return *((_time_t *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}

      
_s32 mach2littles32(_s32 *s)
{
/* Makes the 32 bits of this processor into little endian for
 * the tape.
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_s32)];
    int  c;
    
    for (c=0; c<sizeof(_s32); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_s32)-c-1];
    return *((_s32 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}
    

 _s32 little2machs32(_s32 *s)
{
/* Makes the 32 bits read from tape (which will be little endian)
 * into the endian required by this machine
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_s32)];
    int  c;
    
    for (c=0; c<sizeof(_s32); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_s32)-c-1];
    return *((_s32 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}

      
 _s16 mach2littles16(_s16 *s)
{
/* Makes the 16 bits of this processor into little endian for
 * the tape.
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_s16)];
    int  c;
    
    for (c=0; c<sizeof(_s16); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_s16)-c-1];
    return *((_s16 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}
    

 _s16 little2machs16(_s16 *s)
{
/* Makes the 16 bits read from tape (which will be little endian)
 * into the endian required by this machine
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_s16)];
    int  c;
    
    for (c=0; c<sizeof(_s16); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_s16)-c-1];
    return *((_s16 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}

      
 _u32 mach2littleu32(_u32 *s)
{
/* Makes the 32 bits of this processor into little endian for
 * the tape.
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_u32)];
    int  c;
    
    for (c=0; c<sizeof(_u32); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_u32)-c-1];
    return *((_u32 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}
    

 _u32 little2machu32(_u32 *s)
{
/* Makes the 32 bits read from tape (which will be little endian)
 * into the endian required by this machine
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_u32)];
    int  c;
    
    for (c=0; c<sizeof(_u32); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_u32)-c-1];
    return *((_u32 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}

      
 _u16 mach2littleu16(_u16 *s)
{
/* Makes the 16 bits of this processor into little endian for
 * the tape.
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_u16)];
    int  c;
    
    for (c=0; c<sizeof(_u16); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_u16)-c-1];
    return *((_u16 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}
    

 _u16 little2machu16(_u16 *s)
{
/* Makes the 16 bits read from tape (which will be little endian)
 * into the endian required by this machine
*/
#ifdef TAPER_BIG_ENDIAN
    char s1[sizeof(_u16)];
    int  c;
    
    for (c=0; c<sizeof(_u16); c++) 		 /* reverse byte orders */
      s1[c] = ((char *) s)[sizeof(_u16)-c-1];
    return *((_u16 *) s1);
#else
/* little endian machine - no need to convert */    
    return *s;
#endif    
}

      
void tape_header_endianize2mach(struct tape_header *t)
{
/* What a verb! */
/* Converts the struct tape_header *t read in from tape
 * to the appropriate form for the machine */
    
    struct tape_header t1;
    
    t1.magic = little2machu32(&t->magic);
    t1.archive_id = little2machu32(&t->archive_id);
    t1.tape_number = little2machs32(&t->tape_number);
    my_strcpy(t1.archive_title, t->archive_title, MAX_ARCHIVETITLE);
    *t = t1;
}

    
void tape_header_endianize2little(struct tape_header *t)
{
/* What a verb! */
/* Converts the struct tape_header *t to little endian
 * for the tape */
    
    struct tape_header t1;
    
    t1.magic = mach2littleu32(&t->magic);
    t1.archive_id = mach2littleu32(&t->archive_id);
    t1.tape_number = mach2littles32(&t->tape_number);
    my_strcpy(t1.archive_title, t->archive_title, MAX_ARCHIVETITLE);
    *t = t1;
}


void fi_endianize2mach(struct file_info *fi, struct file_info *fi1)
{
/* Converts little endian fi to machine endian fi */
    
    fi1->act_size = little2machu32(&fi->act_size);
    fi1->volume = little2machs32(&fi->volume);
    fi1->pos_in_archive = little2machs32(&fi->pos_in_archive);
    fi1->dev = little2machu16(&fi->dev);
    fi1->uid = little2machu16(&fi->uid);
    fi1->gid = little2machu16(&fi->gid);
    fi1->mode = little2machu16(&fi->mode);
    fi1->org_mode = little2machu16(&fi->org_mode);
    fi1->size = little2machu32(&fi->size);
    fi1->atime = little2machtime(&fi->atime);
    fi1->ctime = little2machtime(&fi->ctime);
    fi1->mtime = little2machtime(&fi->mtime);
    fi1->backup_time = little2machtime(&fi->backup_time);
    fi1->checksum = little2machs32(&fi->checksum);
    fi1->name_len = fi->name_len;
    fi1->compressed = fi->compressed;
}


void fi_endianize2little(struct file_info *fi, struct file_info *fi1)
{
/* Converts fi to a little endian for writing to tape/info */
    fi1->act_size = mach2littleu32(&fi->act_size);
    fi1->volume = mach2littles32(&fi->volume);
    fi1->pos_in_archive = mach2littles32(&fi->pos_in_archive);
    fi1->dev = mach2littleu16(&fi->dev);
    fi1->uid = mach2littleu16(&fi->uid);
    fi1->gid = mach2littleu16(&fi->gid);
    fi1->mode = mach2littleu16(&fi->mode);
    fi1->org_mode = mach2littleu16(&fi->org_mode);
    fi1->size =  mach2littleu32(&fi->size);
    fi1->atime =  mach2littletime(&fi->atime);
    fi1->ctime =  mach2littletime(&fi->ctime);
    fi1->mtime =  mach2littletime(&fi->mtime);
    fi1->backup_time = mach2littletime(&fi->backup_time);
    fi1->checksum = mach2littles32(&fi->checksum);
    fi1->name_len = fi->name_len;
    fi1->compressed = fi->compressed;
}
    
    
void ifd_endianize2mach(struct info_file_header *ifd, struct info_file_header *ifd1)
{
/* Makes info file header to machine endian */    
/* Header of info file */
    ifd1->magic = little2machu32(&ifd->magic);
    ifd1->archive_id = little2machu32(&ifd->archive_id);
    ifd1->number_tapes = little2machs32(&ifd->number_tapes);
    ifd1->info_file_size = little2machu32(&ifd->info_file_size);
    ifd1->number_volumes = little2machs32(&ifd->number_volumes);
    ifd1->size_volume_headers = little2machu32(&ifd->size_volume_headers);
    ifd1->no_in_archive = little2machs32(&ifd->no_in_archive);
    ifd1->number_tsi = little2machs32(&ifd->number_tsi);
    strcpy(ifd1->archive_title, ifd->archive_title);
}


void ifd_endianize2little(struct info_file_header *ifd, struct info_file_header *ifd1)
{
/* Makes info file header to little endian */    
/* Header of info file */
    ifd1->magic = mach2littleu32(&ifd->magic);
    ifd1->archive_id = mach2littleu32(&ifd->archive_id);
    ifd1->number_tapes = mach2littles32(&ifd->number_tapes);
    ifd1->info_file_size = mach2littleu32(&ifd->info_file_size);
    ifd1->number_volumes = mach2littles32(&ifd->number_volumes);
    ifd1->size_volume_headers = mach2littleu32(&ifd->size_volume_headers);
    ifd1->no_in_archive = mach2littles32(&ifd->no_in_archive);
    ifd1->number_tsi = mach2littles32(&ifd->number_tsi);
    strcpy(ifd1->archive_title, ifd->archive_title);
}


void volheader_endianize2little(struct volume_header orgvh, 
				struct volume_header *newvh)
{
/* Converts orgvh which is machine endian to newvh which
 * is little endian
 */
    newvh->volume_magic = mach2littles32(&orgvh.volume_magic);
    newvh->no_in_volume = mach2littles32(&orgvh.no_in_volume);
    my_strcpy(newvh->volume_title, orgvh.volume_title, MAX_ARCHIVETITLE);
    newvh->backup_time = mach2littletime(&orgvh.backup_time);
    newvh->no_sels = mach2littles32(&orgvh.no_sels);
    newvh->size_header = mach2littleu32(&orgvh.size_header);
}

void volheader_endianize2mach(struct volume_header orgvh, 
			      struct volume_header *newvh)
{
/* Converts orgvh which is little endian to newvh which
 * is machine endian
 */
    newvh->volume_magic = little2machs32(&orgvh.volume_magic);
    newvh->no_in_volume = little2machs32(&orgvh.no_in_volume);
    my_strcpy(newvh->volume_title, orgvh.volume_title, MAX_ARCHIVETITLE);
    newvh->backup_time = little2machtime(&orgvh.backup_time);
    newvh->no_sels = little2machs32(&orgvh.no_sels);
    newvh->size_header = little2machu32(&orgvh.size_header);
}


void vti_endianize2little(struct volume_tape_info *vti, struct volume_tape_info *vti1)
{
    vti1->volume = mach2littles32(&vti->volume);
    vti1->start_tape = mach2littles32(&vti->start_tape);
    vti1->end_tape = mach2littles32(&vti->end_tape);
    vti1->blocks_on_last_tape = mach2littleu32(&vti->blocks_on_last_tape);
}


void vti_endianize2mach(struct volume_tape_info *vti, struct volume_tape_info *vti1)
{
    vti1->volume = little2machs32(&vti->volume);
    vti1->start_tape = little2machs32(&vti->start_tape);
    vti1->end_tape = little2machs32(&vti->end_tape);
    vti1->blocks_on_last_tape = little2machu32(&vti->blocks_on_last_tape);
}


void tsi_endianize2little(struct tape_size_info *tsi, struct tape_size_info *tsi1)
{
    tsi1->tape_number = mach2littles32(&tsi->tape_number);
    tsi1->volume = mach2littles32(&tsi->volume);
    tsi1->blocks = mach2littleu32(&tsi->blocks);
    tsi1->lb_bytes_short = mach2littleu32(&tsi->lb_bytes_short);
}


void tsi_endianize2mach(struct tape_size_info *tsi, struct tape_size_info *tsi1)
{
    tsi1->tape_number = little2machs32(&tsi->tape_number);
    tsi1->volume = little2machs32(&tsi->volume);
    tsi1->blocks = little2machu32(&tsi->blocks);
    tsi1->lb_bytes_short = little2machu32(&tsi->lb_bytes_short);
}
