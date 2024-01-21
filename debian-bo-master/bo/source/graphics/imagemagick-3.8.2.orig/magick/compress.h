/*
  Compress utility routines.
*/
extern unsigned int
  BMPDecodeImage(unsigned char *,unsigned char *,const unsigned int,
    const unsigned int,const unsigned int),
  BMPEncodeImage(unsigned char *,unsigned char *,const unsigned int,
    const unsigned int),
  HuffmanDecodeImage(Image *),
  HuffmanEncodeImage(const ImageInfo *image_info,Image *),
  GIFDecodeImage(Image *),
  GIFEncodeImage(Image *,const unsigned int),
  LZWEncodeImage(FILE *,unsigned char *,const unsigned int),
  PackbitsEncodeImage(FILE *,unsigned char *,unsigned int),
  PCDDecodeImage(Image *,unsigned char *,unsigned char *,unsigned char *),
  PICTEncodeImage(Image *,unsigned char *,unsigned char *),
  RunlengthDecodeImage(Image *),
  RunlengthEncodeImage(Image *),
  SUNDecodeImage(unsigned char *,unsigned char *,const unsigned int,
    const unsigned int);

extern void
  Ascii85Encode(const unsigned int,FILE *),
  Ascii85Flush(FILE *),
  Ascii85Initialize(void);
