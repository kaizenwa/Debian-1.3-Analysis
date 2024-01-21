/*================================================================
 * guspatch.h
 *	patch structure of GUS compatible patch file
 *================================================================*/

#ifndef GUSPATCH_H_DEF
#define GUSPATCH_H_DEF

#define GUS_ENVELOPES	6	
 
/* This is the definition for what FORTE's patch format is. All .PAT */
/* files will have this format. */
 
#define GUS_HEADER_SIZE		12
#define GUS_ID_SIZE		10
#define GUS_DESC_SIZE		60
#define GUS_RESERVED_SIZE	40
#define GUS_PATCH_HEADER_RESERVED_SIZE 36
#define GUS_LAYER_RESERVED_SIZE	40
#define GUS_PATCH_DATA_RESERVED_SIZE	36
#define GUS_GF1_HEADER_TEXT	"GF1PATCH110"
 
typedef struct
{
	char		header[ GUS_HEADER_SIZE ];	
	char		gravis_id[ GUS_ID_SIZE ];	/* Id = "ID#000002" */
	char		description[ GUS_DESC_SIZE ];
	unsigned char	instruments;
	char		voices;
	char		channels;
	unsigned short	wave_forms;
	unsigned short	master_volume;
	unsigned long	data_size;
	char		reserved[ GUS_PATCH_HEADER_RESERVED_SIZE ];
} GusPatchHeader;
 
typedef struct
{
	unsigned short	instrument;
	char		instrument_name[ 16 ];
	long		instrument_size;
	char		layers;
	char		reserved[ GUS_RESERVED_SIZE ];	
} GusInstrument;
 
typedef struct
{
	char		layer_duplicate;
	char		layer;
	long		layer_size;
	char		samples;
	char		reserved[ GUS_LAYER_RESERVED_SIZE ];	
} GusLayerData;
 
typedef struct
{
	char		wave_name[7];
 
	unsigned char	fractions;
	long		wave_size;
	long		start_loop;
	long		end_loop;
 
	unsigned short	sample_rate;
	long		low_frequency;
	long		high_frequency;
	long		root_frequency;
	short		tune;
	
	unsigned char	balance;
 
	unsigned char	envelope_rate[ GUS_ENVELOPES ];
	unsigned char	envelope_offset[ GUS_ENVELOPES ];	
 
	unsigned char	tremolo_sweep;
	unsigned char	tremolo_rate;
	unsigned char	tremolo_depth;
	
	unsigned char	vibrato_sweep;
	unsigned char	vibrato_rate;
	unsigned char	vibrato_depth;
	
	char		modes;
	#define GUS_MODE_16BIT		1
	#define GUS_MODE_UNSIGNED	2
	#define GUS_MODE_LOOP		4
	#define GUS_MODE_LOOP_BIDIR	8
	#define GUS_MODE_LOOP_BACK	16
	#define GUS_MODE_SUSTAIN	32
	#define GUS_MODE_ENVELOPE	64
 
	short		scale_frequency;
	unsigned short	scale_factor;		/* from 0 to 2048 or 0 to 2 */
	
	char		reserved[ GUS_PATCH_DATA_RESERVED_SIZE ];
} GusPatchData;


#endif
