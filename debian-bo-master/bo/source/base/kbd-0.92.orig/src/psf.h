#define PSF_MAGIC      0x0436
#define PSF_MODE512    0x01
#define PSF_MODEHASTAB 0x02
#define PSF_MAXMODE    0x03
#define PSF_SEPARATOR  0xFFFF

struct psf_header
{
  unsigned short magic;		/* Magic number */
  unsigned char mode;		/* PSF font mode */
  unsigned char charsize;	/* Character size */
};
