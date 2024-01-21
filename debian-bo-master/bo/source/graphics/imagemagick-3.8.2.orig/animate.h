/*
  State declarations.
*/
#define AutoReverseAnimationState 0x0001
#define DefaultState  0x0000
#define ExitState  0x0002
#define ForwardAnimationState 0x0004
#define HighlightState  0x0008
#define PlayAnimationState 0x0010
#define RepeatAnimationState 0x0020
#define StepAnimationState 0x0040

/*
  Static declarations.
*/
static char
  *ImageMagickHelp[]=
  {
    "BUTTONS",
    "",
    "  Press any button to map or unmap the Command widget.",
    "",
    "COMMAND WIDGET",
    "  The Command widget lists a number of sub-menus and commands.",
    "  They are",
    "",
    "    Animate",
    "      Open",
    "      Play",
    "      Step",
    "      Repeat",
    "      Auto Reverse",
    "    Speed",
    "      Slower",
    "      Faster",
    "    Direction",
    "      Forward",
    "      Reverse",
    "    Image Info",
    "    Help",
    "    Quit",
    "",
    "  Menu items with a indented triangle have a sub-menu.  They",
    "  are represented above as the indented items.  To access a",
    "  sub-menu item, move the pointer to the appropriate menu and",
    "  press a button and drag.  When you find the desired sub-menu",
    "  item, release the button and the command is executed.  Move",
    "  the pointer away from the sub-menu if you decide not to",
    "  execute a particular command.",
    "",
    "KEYBOARD ACCELERATORS",
    "  Accelerators are one or two key presses that effect a",
    "  particular command.  The keyboard accelerators that",
    "  animate(1) understands is:",
    "",
    "  Ctl+O  Press to open an image from a file.",
    "",
    "  space  Press to display the next image in the sequence.",
    "",
    "  <      Press to speed-up the display of the images.  Refer to",
    "         -delay for more information.",
    "",
    "  >      Press to slow the display of the images.  Refer to",
    "         -delay for more information.",
    "",
    "  ?      Press to display information about the image.  Press",
    "         any key or button to erase the information.",
    "",
    "         This information is printed: image name;  image size;",
    "         and the total number of unique colors in the image.",
    "",
    "  h      Press to display helpful information about animate(1).",
    "",
    "  Ctl-q  Press to discard all images and exit program.",
    (char *) NULL
  };

/*
  Enumeration declarations.
*/
typedef enum
{
  OpenCommand,
  PlayCommand,
  StepCommand,
  RepeatCommand,
  AutoReverseCommand,
  SlowerCommand,
  FasterCommand,
  ForwardCommand,
  ReverseCommand,
  InfoCommand,
  HelpCommand,
  QuitCommand,
  NullCommand
} CommandType;

/*
  Stipples.
*/
#define BricksWidth  20
#define BricksHeight  20
#define DiagonalWidth  16
#define DiagonalHeight  16
#define HighlightWidth  8
#define HighlightHeight  8
#define ScalesWidth  16
#define ScalesHeight  16
#define ShadowWidth  8
#define ShadowHeight  8
#define VerticalWidth  16
#define VerticalHeight  16
#define WavyWidth  16
#define WavyHeight  16

static unsigned char
  BricksBitmap[] =
  {
    0xff, 0xff, 0x0f, 0x03, 0x0c, 0x00, 0x03, 0x0c, 0x00, 0x03, 0x0c, 0x00,
    0x03, 0x0c, 0x00, 0xff, 0xff, 0x0f, 0x60, 0x80, 0x01, 0x60, 0x80, 0x01,
    0x60, 0x80, 0x01, 0x60, 0x80, 0x01, 0xff, 0xff, 0x0f, 0x03, 0x0c, 0x00,
    0x03, 0x0c, 0x00, 0x03, 0x0c, 0x00, 0x03, 0x0c, 0x00, 0xff, 0xff, 0x0f,
    0x60, 0x80, 0x01, 0x60, 0x80, 0x01, 0x60, 0x80, 0x01, 0x60, 0x80, 0x01
  },
  DiagonalBitmap[] =
  {
    0x44, 0x44, 0x88, 0x88, 0x11, 0x11, 0x22, 0x22, 0x44, 0x44, 0x88, 0x88,
    0x11, 0x11, 0x22, 0x22, 0x44, 0x44, 0x88, 0x88, 0x11, 0x11, 0x22, 0x22,
    0x44, 0x44, 0x88, 0x88, 0x11, 0x11, 0x22, 0x22
  },
  HighlightBitmap[] =
  {
    0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55
  },
  ScalesBitmap[] =
  {
    0x08, 0x08, 0x08, 0x08, 0x14, 0x14, 0xe3, 0xe3, 0x80, 0x80, 0x80, 0x80,
    0x41, 0x41, 0x3e, 0x3e, 0x08, 0x08, 0x08, 0x08, 0x14, 0x14, 0xe3, 0xe3,
    0x80, 0x80, 0x80, 0x80, 0x41, 0x41, 0x3e, 0x3e
  },
  ShadowBitmap[] =
  {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  },
  VerticalBitmap[] =
  {
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11
  },
  WavyBitmap[] =
  {
    0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfd, 0xff, 0xfd, 0xff, 0xfb, 0xff,
    0xe7, 0xff, 0x1f, 0xff, 0xff, 0xf8, 0xff, 0xe7, 0xff, 0xdf, 0xff, 0xbf,
    0xff, 0xbf, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f
  };

/*
  Global variable declarations.
*/
static Display
  *display;
 
static Image
  *copy_image = (Image *) NULL;
 
static XWindows
  *windows;

/*
  Function prototypes.
*/
static Image
  *XAnimateImages(Display *,XResourceInfo *,char **,const int,Image *),
  *XMagickCommand(Display *,XResourceInfo *,XWindows *,const CommandType,
    Image **,unsigned int *);

static int
  SceneCompare(const void *,const void *);

static void
  XAnimateBackgroundImage (Display *,XResourceInfo *,Image *),
  XConfigureImageColormap(Display *,XResourceInfo *,XWindows *,Image *),
  XProgressMonitor(char *,const unsigned int,const unsigned int),
  XWarning(const char *,const char *);
