/*
  Image define declarations.
*/
#define ColorMatch(color,target,delta) \
  ((((int) ((color).red)-delta) <= (int) ((target).red)) && \
    ((int) ((target).red) <= ((int) ((color).red)+delta)) && \
   (((int) ((color).green)-delta) <= (int) ((target).green)) && \
    ((int) ((target).green) <= ((int) ((color).green)+delta)) && \
   (((int) ((color).blue)-delta) <= (int) ((target).blue)) && \
    ((int) ((target).blue) <= ((int) ((color).blue)+delta)))
#define DegreesToRadians(x) ((x)*M_PI/180.0)
#define Intensity(color)  \
  ((unsigned int) ((color).red*77+(color).green*150+(color).blue*29) >> 8)
#define IsGray(color)  \
  (((color).red == (color).green) && ((color).green == (color).blue))
#define IsMonochromeImage(image) (IsGrayImage(image) && ((image)->colors <= 2))
#define MatteMatch(color,target,delta) \
  (ColorMatch(color,target,delta) && ((color).index == (target).index))
#define MaxColormapSize  65535L
#define MaxStacksize  (1 << 15)
#define MaxTextExtent  2048
#define PixelOffset(x,y) image->pixels+((y)*image->columns+(x))
#define Push(up,left,right,delta) \
  if ((p < (segment_stack+MaxStacksize)) && (((up)+(delta)) >= 0) && \
      (((up)+(delta)) < image->rows)) \
    { \
      p->y1=(up); \
      p->x1=(left); \
      p->x2=(right); \
      p->y2=(delta); \
      p++; \
    }
#define RadiansToDegrees(x) ((x)*180/M_PI)
#define ReadQuantum(quantum,p)  \
{  \
  if (image->depth == 8) \
    quantum=UpScale(*p++); \
  else \
    { \
      value=(*p++) << 8;  \
      value|=(*p++);  \
      quantum=value >> (image->depth-QuantumDepth); \
    } \
}
#define ReadQuantumFile(quantum)  \
{  \
  if (image->depth == 8) \
    quantum=UpScale(fgetc(image->file)); \
  else \
    quantum=MSBFirstReadShort(image->file) >> (image->depth-QuantumDepth); \
}
#define SharpenFactor  70.0
#define Transparent  0
#define WriteQuantum(quantum,q)  \
{  \
  if (image->depth == 8) \
    *q++=DownScale(quantum); \
  else \
    { \
      value=(quantum); \
      if ((QuantumDepth-image->depth) > 0) \
        value*=257; \
      *q++=value >> 8; \
      *q++=value; \
    } \
}
#define WriteQuantumFile(quantum)  \
{  \
  if (image->depth == 8) \
    (void) fputc(DownScale(quantum),image->file); \
  else \
    if ((QuantumDepth-image->depth) > 0) \
      MSBFirstWriteShort((quantum)*257,image->file); \
    else \
      MSBFirstWriteShort(quantum,image->file); \
}

#ifdef QuantumLeap
/*
  Color quantum is [0..65535].
*/
#define DownScale(quantum)  (((unsigned int) (quantum)) >> 8)
#define Opaque  65535L
#define MaxRGB  65535L
#define MaxRunlength  65535L
#define QuantumDepth  16
#define UpScale(quantum)  (((unsigned int) (quantum))*257)
#define XDownScale(color)  ((unsigned int) (color))
#define XUpScale(color)  ((unsigned int) (color))

typedef unsigned short Quantum;
#else
/*
  Color quantum is [0..255].
*/
#define DownScale(quantum)  ((unsigned int) (quantum))
#define Opaque  255
#define MaxRGB  255
#define MaxRunlength  255
#define QuantumDepth  8
#define UpScale(quantum)  ((unsigned int) (quantum))
#define XDownScale(color)  (((unsigned int) (color)) >> 8)
#define XUpScale(color)  (((unsigned int) (color))*257)

typedef unsigned char Quantum;
#endif

/*
  Enumeration declarations.
*/
typedef enum
{
  UndefinedClass,
  DirectClass,
  PseudoClass
} ClassType;

typedef enum
{
  UndefinedColorspace,
  RGBColorspace,
  GRAYColorspace,
  TransparentColorspace,
  OHTAColorspace,
  XYZColorspace,
  YCbCrColorspace,
  YCCColorspace,
  YIQColorspace,
  YPbPrColorspace,
  YUVColorspace
} ColorspaceType;

typedef enum
{
  UndefinedCompositeOp = 0,
  OverCompositeOp,
  InCompositeOp,
  OutCompositeOp,
  AtopCompositeOp,
  XorCompositeOp,
  PlusCompositeOp,
  MinusCompositeOp,
  AddCompositeOp,
  SubtractCompositeOp,
  DifferenceCompositeOp,
  BumpmapCompositeOp,
  ReplaceCompositeOp,
  MatteReplaceCompositeOp,
  AddMaskCompositeOp,
  BlendCompositeOp,
  DisplaceCompositeOp
} CompositeOperator;

typedef enum
{
  UndefinedCompression,
  NoCompression,
  RunlengthEncodedCompression,
  ZipCompression
} CompressionType;

typedef enum
{
  BoxFilter,
  MitchellFilter,
  TriangleFilter
} FilterType;

typedef enum
{
  UndefinedId,
  ImageMagickId
} IdType;

typedef enum
{
  UndefinedInterlace,
  NoneInterlace,
  LineInterlace,
  PlaneInterlace,
  PartitionInterlace
} InterlaceType;

typedef enum
{
  UniformNoise,
  GaussianNoise,
  MultiplicativeGaussianNoise,
  ImpulseNoise,
  LaplacianNoise,
  PoissonNoise
} NoiseType;

typedef enum
{
  PointMethod = 0,
  ReplaceMethod,
  FloodfillMethod,
  ResetMethod
} PaintMethod;

typedef enum
{
  RotatePreview = 0,
  ShearPreview,
  RollPreview,
  HuePreview,
  SaturationPreview,
  BrightnessPreview,
  GammaPreview,
  SpiffPreview,
  DullPreview,
  GrayscalePreview,
  QuantizePreview,
  DespecklePreview,
  ReduceNoisePreview,
  AddNoisePreview,
  SharpenPreview,
  BlurPreview,
  ThresholdPreview,
  EdgeDetectPreview,
  SpreadPreview,
  SolarizePreview,
  ShadePreview,
  RaisePreview,
  SegmentPreview,
  SwirlPreview,
  ImplodePreview,
  OilPaintPreview,
  CharcoalDrawingPreview
} PreviewType;

typedef enum
{
  UndefinedPrimitive = 0,
  PointPrimitive,
  LinePrimitive,
  RectanglePrimitive,
  FillRectanglePrimitive,
  EllipsePrimitive,
  FillEllipsePrimitive,
  PolygonPrimitive,
  FillPolygonPrimitive,
  ColorPrimitive,
  MattePrimitive,
  TextPrimitive,
  ImagePrimitive
} PrimitiveType;

typedef enum
{
  UndefinedResolution,
  PixelsPerInchResolution,
  PixelsPerCentimeterResolution
} ResolutionType;

/*
  Typedef declarations.
*/
typedef struct _AnnotateInfo
{
  char
    *server_name,
    *font;

  unsigned int
    pointsize;

  char
    *box,
    *pen,
    *geometry,
    *text,
    *primitive;

  unsigned int
    linewidth,
    center;
} AnnotateInfo;

typedef struct _ColorPacket
{
  Quantum
    red,
    green,
    blue;

  unsigned char
    flags;

  char
    key[3];

  unsigned short
    index;
} ColorPacket;

typedef struct _ContributionInfo
{
  int
    pixel;

  long
    weight;
} ContributionInfo;

typedef struct _FrameInfo
{
  int
    x,
    y;

  unsigned int
    width,
    height;

  int
    inner_bevel,
    outer_bevel;
} FrameInfo;

typedef struct _ImageInfo
{
  char
    *filename,
    magick[MaxTextExtent];

  unsigned int
    affirm,
    subimage,
    subrange;

  char
    *server_name,
    *font,
    *size,
    *tile,
    *density,
    *page,
    *dispose,
    *delay,
    *iterations,
    *texture;

  unsigned int
    adjoin;

  CompressionType
    compression;

  unsigned int
    dither;

  InterlaceType
    interlace;

  unsigned int
    monochrome,
    pointsize,
    quality,
    verbose;

  PreviewType
    preview_type;

  char
    *undercolor;
} ImageInfo;

typedef struct _PrimitiveInfo
{
  PrimitiveType
    primitive;

  unsigned int
    coordinates;

  int
    x,
    y;

  PaintMethod
    method;

  char
    *text;
} PrimitiveInfo;

typedef struct _QuantizeInfo
{
  unsigned int
    number_colors,
    tree_depth,
    dither;

  ColorspaceType
    colorspace;
} QuantizeInfo;

typedef struct _RectangleInfo
{
  unsigned int
    width,
    height;

  int
    x,
    y;
} RectangleInfo;

typedef struct _RunlengthPacket
{
  Quantum
    red,
    green,
    blue,
    length;

  unsigned short
    index;
} RunlengthPacket;

typedef struct _SegmentInfo
{
  int
    x1,
    y1,
    x2,
    y2;
} SegmentInfo;

typedef struct _Image
{
  FILE
    *file;

  int
    status,
    temporary;

  char
    filename[MaxTextExtent];

  long int
    filesize;

  int
    pipe;

  char
    magick[MaxTextExtent],
    *comments,
    *label,
    *text;

  IdType
    id;

  ClassType
#if defined(__cplusplus) || defined(c_plusplus)
    c_class;
#else
    class;
#endif

  unsigned int
    matte;

  CompressionType
    compression;

  unsigned int
    columns,
    rows,
    depth;

  InterlaceType
    interlace;

  unsigned int
    scene;

  char
    *montage,
    *directory;

  ColorPacket
    *colormap;

  ColorspaceType
    colorspace;

  unsigned int
    colors;

  double
    gamma;

  ResolutionType
    units;

  float
    x_resolution,
    y_resolution;

  unsigned int
    mean_error_per_pixel;

  double
    normalized_mean_error,
    normalized_maximum_error;

  unsigned long
    total_colors;

  char
    *signature;

  RunlengthPacket
    *pixels,
    *packet;

  unsigned int
    packets,
    runlength,
    packet_size;

  unsigned char
    *packed_pixels;

  ColorPacket
    background_color,
    border_color,
    matte_color;

  long int
    magick_time;

  char
    magick_filename[MaxTextExtent];

  unsigned int
    magick_columns,
    magick_rows;

  char
    *geometry,
    *page;

  unsigned int
    dispose,
    delay,
    iterations;

  unsigned int
    orphan;

  struct _Image
    *previous,
    *list,
    *next;
} Image;

/*
  Image utilities routines.
*/
extern void
  CommentImage(Image *,char *),
  LabelImage(Image *,char *);

extern Image
  *AddNoiseImage(Image *,NoiseType),
  *AllocateImage(const ImageInfo *),
  *AverageImages(Image *),
  *BorderImage(Image *,RectangleInfo *),
  *BlurImage(Image *,double),
  *ChopImage(Image *,RectangleInfo *),
  *CopyImage(Image *,const unsigned int,const unsigned int,const unsigned int),
  *CropImage(Image *,RectangleInfo *),
  *DespeckleImage(Image *),
  *EdgeImage(Image *,double),
  *EmbossImage(Image *),
  *EnhanceImage(Image *),
  *FlipImage(Image *),
  *FlopImage(Image *),
  *FrameImage(Image *,FrameInfo *),
  *ImplodeImage(Image *,double),
  **ListToGroupImage(Image *,unsigned int *),
  *MagnifyImage(Image *),
  *MinifyImage(Image *),
  *OilPaintImage(Image *,const unsigned int),
  *ReadImage(ImageInfo *),
  *ReduceNoiseImage(Image *),
  *RollImage(Image *,int,int),
  *RotateImage(Image *,double,const unsigned int,const unsigned int),
  *SampleImage(Image *,unsigned int,unsigned int),
  *ScaleImage(Image *,const unsigned int,const unsigned int),
  *ShadeImage(Image *,unsigned int,double,double),
  *SharpenImage(Image *,double),
  *ShearImage(Image *,double,double,const unsigned int),
  *SpreadImage(Image *,unsigned int),
  *StereoImage(Image *,Image *),
  *SwirlImage(Image *,double),
  *ZoomImage(Image *,const unsigned int,const unsigned int,const FilterType);

extern int
  ParseImageGeometry(char *,int *,int *,unsigned int *,unsigned int *);

extern unsigned int
  IsGeometry(char *),
  IsGrayImage(Image *),
  IsPseudoClass(Image *),
  PlasmaImage(Image *,SegmentInfo *,int,int),
  UncompressImage(Image *),
  WriteImage(ImageInfo *,Image *);

extern void
  AnnotateImage(Image *,AnnotateInfo *),
  CloseImage(Image *),
  ColorFloodfillImage(Image *,int,int,const ColorPacket *,const int),
  ColorizeImage(Image *,char *,char *),
  CompositeImage(Image *,const CompositeOperator,Image *,const int,const int),
  CompressColormap(Image *),
  CompressImage(Image *),
  ContrastImage(Image *,const unsigned int),
  CycleColormapImage(Image *,int),
  DescribeImage(Image *,FILE *,const unsigned int),
  DestroyImage(Image *),
  DestroyImages(Image *),
  DrawImage(Image *,AnnotateInfo *),
  EqualizeImage(Image *),
  GammaImage(Image *,char *),
  GetAnnotateInfo(AnnotateInfo *),
  GetImageInfo(ImageInfo *),
  GetQuantizeInfo(QuantizeInfo *),
  HSLTransform(double,const double,const double,Quantum *,Quantum *,Quantum *),
  MapImage(Image *,Image *,const unsigned int),
  MapImages(Image *,Image *,const unsigned int),
  MatteFloodfillImage(Image *,int,int,const unsigned int,const int),
  ModulateImage(Image *,char *),
  MogrifyImage(ImageInfo *,int,char **,Image **),
  MogrifyImages(ImageInfo *,int,char **,Image **),
  NegateImage(Image *,unsigned int),
  NormalizeImage(Image *),
  NumberColors(Image *,FILE *),
  OpaqueImage(Image *,char *,char *),
  OpenImage(const ImageInfo *,Image *,const char *),
  QuantizationError(Image *),
  QuantizeImage(QuantizeInfo *,Image *),
  QuantizeImages(QuantizeInfo *,Image *),
  RaiseImage(Image *,RectangleInfo *,const int),
  RGBTransformImage(Image *,const unsigned int),
  SegmentImage(Image *,const unsigned int,const unsigned int,const double,
    const double),
  SetImageInfo(ImageInfo *,unsigned int),
  SignatureImage(Image *),
  SolarizeImage(Image *,const double),
  SortColormapByIntensity(Image *),
  SyncImage(Image *),
  TextureImage(Image *,char *),
  ThresholdImage(Image *,char *),
  TransformHSL(const Quantum,const Quantum,const Quantum,double *,double *,
    double *),
  TransformImage(Image **,char *,char *),
  TransformRGBImage(Image *,const unsigned int),
  TransparentImage(Image *,char *);
