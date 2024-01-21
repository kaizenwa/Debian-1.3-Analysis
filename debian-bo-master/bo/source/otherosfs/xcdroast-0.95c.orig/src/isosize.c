#include <stdio.h>
#include <termio.h>
#include <signal.h>

FILE * infile;
int file_addr;

#define ISODCL(from, to) (to - from + 1)

int
isonum_731 (char * p)
{
	return ((p[0] & 0xff)
		| ((p[1] & 0xff) << 8)
		| ((p[2] & 0xff) << 16)
		| ((p[3] & 0xff) << 24));
}


int
isonum_733 (char * p)
{
	return (isonum_731 (p));
}

struct iso_primary_descriptor {
	unsigned char type			[ISODCL (  1,   1)]; /* 711 */
	unsigned char id				[ISODCL (  2,   6)];
	unsigned char version			[ISODCL (  7,   7)]; /* 711 */
	unsigned char unused1			[ISODCL (  8,   8)];
	unsigned char system_id			[ISODCL (  9,  40)]; /* aunsigned chars */
	unsigned char volume_id			[ISODCL ( 41,  72)]; /* dunsigned chars */
	unsigned char unused2			[ISODCL ( 73,  80)];
	unsigned char volume_space_size		[ISODCL ( 81,  88)]; /* 733 */
	unsigned char unused3			[ISODCL ( 89, 120)];
	unsigned char volume_set_size		[ISODCL (121, 124)]; /* 723 */
	unsigned char volume_sequence_number	[ISODCL (125, 128)]; /* 723 */
	unsigned char logical_block_size		[ISODCL (129, 132)]; /* 723 */
	unsigned char path_table_size		[ISODCL (133, 140)]; /* 733 */
	unsigned char type_l_path_table		[ISODCL (141, 144)]; /* 731 */
	unsigned char opt_type_l_path_table	[ISODCL (145, 148)]; /* 731 */
	unsigned char type_m_path_table		[ISODCL (149, 152)]; /* 732 */
	unsigned char opt_type_m_path_table	[ISODCL (153, 156)]; /* 732 */
	unsigned char root_directory_record	[ISODCL (157, 190)]; /* 9.1 */
	unsigned char volume_set_id		[ISODCL (191, 318)]; /* dunsigned chars */
	unsigned char publisher_id		[ISODCL (319, 446)]; /* achars */
	unsigned char preparer_id		[ISODCL (447, 574)]; /* achars */
	unsigned char application_id		[ISODCL (575, 702)]; /* achars */
	unsigned char copyright_file_id		[ISODCL (703, 739)]; /* 7.5 dchars */
	unsigned char abstract_file_id		[ISODCL (740, 776)]; /* 7.5 dchars */
	unsigned char bibliographic_file_id	[ISODCL (777, 813)]; /* 7.5 dchars */
	unsigned char creation_date		[ISODCL (814, 830)]; /* 8.4.26.1 */
	unsigned char modification_date		[ISODCL (831, 847)]; /* 8.4.26.1 */
	unsigned char expiration_date		[ISODCL (848, 864)]; /* 8.4.26.1 */
	unsigned char effective_date		[ISODCL (865, 881)]; /* 8.4.26.1 */
	unsigned char file_structure_version	[ISODCL (882, 882)]; /* 711 */
	unsigned char unused4			[ISODCL (883, 883)];
	unsigned char application_data		[ISODCL (884, 1395)];
	unsigned char unused5			[ISODCL (1396, 2048)];
};

main(int argc, char * argv[]){
  char c;
  int nbyte;
  int i,j;
  struct iso_primary_descriptor ipd;
  struct iso_directory_record * idr;

  if(argc < 2) return 0;
  infile = fopen(argv[1],"rb");

  file_addr = 16 << 11;
  lseek(fileno(infile), file_addr, 0);
  read(fileno(infile), &ipd, sizeof(ipd));

  idr = (struct iso_directory_record *) &ipd.root_directory_record;
#if 1
  printf ("%d\n", isonum_733(ipd.volume_space_size) * 2048);
#else
  printf ("%d\n", isonum_733(ipd.volume_space_size) *
	  isonum_733(ipd.logical_block_size);
#endif

  exit(0);
}




