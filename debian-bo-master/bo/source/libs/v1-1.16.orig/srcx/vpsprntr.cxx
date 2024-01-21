//===============================================================
// vpsprntr.cxx - a PostScript Printer class - X11
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vapp.h>
#include <v/vpsprntr.h>
#include <v/vicon.h>
#include <v/vfilesel.h>

#define prompt_width 32
#define prompt_height 32
static unsigned char prompt_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0xf8, 0xff, 0xff, 0x1f, 0x04, 0x00, 0x00, 0x20,
   0xe4, 0xff, 0xff, 0x27, 0x14, 0x00, 0x00, 0x28, 0x14, 0xc0, 0x01, 0x28,
   0x14, 0xe0, 0x03, 0x28, 0x14, 0x30, 0x06, 0x28, 0x14, 0x30, 0x06, 0x28,
   0x14, 0x00, 0x03, 0x28, 0x14, 0x80, 0x01, 0x28, 0x14, 0xc0, 0x00, 0x28,
   0x14, 0xc0, 0x00, 0x28, 0x14, 0xc0, 0x00, 0x28, 0x14, 0xc0, 0x00, 0x28,
   0x14, 0x00, 0x00, 0x28, 0x14, 0xc0, 0x00, 0x28, 0x14, 0x00, 0x00, 0x28,
   0xe4, 0xff, 0xff, 0x27, 0x04, 0x00, 0x00, 0x20, 0xf8, 0xff, 0xff, 0x1f,
   0xfe, 0xff, 0xff, 0x7f, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
   0x02, 0x00, 0xe0, 0x47, 0x02, 0x00, 0x00, 0x40, 0xfa, 0xff, 0xff, 0x5f,
   0xae, 0xaa, 0xaa, 0x6a, 0x56, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0x6a,
   0xfe, 0xff, 0xff, 0x7f, 0x00, 0x00, 0x00, 0x00};

    static vIcon prompt(&prompt_bits[0], prompt_height, prompt_width);

    // Standard PostScript Papers

    static char* PaperList[] =
      {
	"Letter (8.5x11)", "Legal",	"Tabloid",	"Ledger",
	"Statement",	"Executive",	"A3",		"A4",
	"A5",		"B4",		"B5",		"Folio",
	"Quarto",	"10x14",	0
      };           

    static int PaperWidths[] =
      {
	612,		612,		792,		1224,
	396,		540,		842,		595,
	420,		729,		516,		612,	
	610,		720,		0
      };           

    static int PaperHeights[] =
      {
	792,		1008,		1224,		792,
	612,		720,		1190,		842,
	595,		1032,		729,		936,
	780,		1008,		0
      };           

    static CommandObject PrintDialog[] =
      {
	{C_Icon,  10, 0, "Print -", (void*)&prompt, CA_None, isSens,NoFrame,0,0},
	{C_Label, 11, 0, "PostScript Print To File",
		 NoList, CA_None, isSens,NoFrame, 10,0},

	{C_Label, 20, 0, "Save in: ",NoList, CA_None,isSens,NoFrame,0,10},
	{C_TextIn,21, 0, "",NoList,CA_None, isSens,NoFrame,20,10},
	{C_Button,22, 0, "Browse",NoList,CA_None,isSens,NoFrame,21,10},

	{C_RadioButton,30, 1, "Portrait",NoList,CA_None,isSens,NoFrame,0,21},
	{C_RadioButton,31, 0, "Landscape",NoList,CA_None,isSens,NoFrame,30,21},

	{C_Label,40,0,"Paper:",NoList,CA_None,isSens,NoFrame,0,30},
	{C_ComboBox, 41,vPaperDefault, "",(void*)PaperList,CA_None,isSens,NoFrame,40,30},
	{C_Blank, 49, 0, "  ", NoList, CA_None,isSens,NoFrame, 41, 21},
	{C_CheckBox, 33, 0, "Color Printer",NoList,CA_None,isSens,NoFrame,49,21},
	{C_Button, M_Cancel, M_Cancel, " Cancel ", NoList,
		CA_None,isSens,NoFrame, 49, 30},
	{C_Button, M_OK, M_OK, " OK ", NoList,
		CA_DefaultButton, isSens, NoFrame, M_Cancel, 30},
	{C_EndOfList,0,0,0,0,CA_None,0,0,0,0}
      };

   int vPSPrinter::_instances = 0;	// reference counter for copy / assignment

//================>>> vPSPrinter::vPSPrinter <<<========================
  vPSPrinter::vPSPrinter() : vModalDialog(theApp,"PostScript Printer Setup")
  {
    _cmdsAdded = 0;
    _name = 0;
    _paperType = vPaperDefault;
    _copies = 1;
    _toFile = 1;
    _width = PaperWidths[_paperType];
    _height = PaperHeights[_paperType];
    _portrait = 1;
    _useColor = 0;
    _pstream = 0;

    ++_instances;		// bump reference counter
  }

//================>>> vPSPrinter::= <<<========================
  vPSPrinter& vPSPrinter::operator =(const vPSPrinter& pr)
  {
    if (this == &pr)	// to self?
	return *this;

    if (_name)
	delete [] _name;	// free space before overwrite
    if (_pstream)
	delete _pstream;

    _cmdsAdded = pr._cmdsAdded;
    _name = pr._name;
    _pstream = pr._pstream;
    _width = pr._width;
    _height = pr._height;
    _portrait = pr._portrait;
    _useColor = pr._useColor;
    _paperType = pr._paperType;
    _copies = pr._copies;
    _toFile = pr._toFile;

    return *this;
  }

//================>>> vPSPrinter::~vPSPrinter <<<========================
  vPSPrinter::~vPSPrinter()
  {
    // We are using a reference count to track instances of printers.
    // We want to allow the user to pass copies of the single printer
    // around, but there should only be one name and one stream;

    --_instances;		// not reference any more

    if (_instances > 0)
	return;			// don't do anything else

    if (_name)
	delete [] _name;
    if (_pstream)
	delete _pstream;
  }

//================>>> vPSPrinter::GetPaperName <<<========================
  char* vPSPrinter::GetPaperName()
  {
    if (_paperType == vPaperLetter)	// we have (8.5x11) added...
	return "Letter";
    else
	return PaperList[_paperType];
  }

//================>>> vPSPrinter::OpenPrinter <<<========================
  ofstream* vPSPrinter::OpenPrinter()
  {
    if (!_name)
      {
	char *name = "printer.ps";
	_name = new char[strlen(name)+1];
	strcpy(_name, name);
      }

    if (_pstream)
	_pstream->close();

    _pstream = new ofstream(_name);	// open output stream
    return _pstream;
  }
 
//================>>> vPSPrinter::ClosePrinter <<<========================
  void vPSPrinter::ClosePrinter()
  {
    if (_pstream)
	_pstream->close();
    delete _pstream;
    _pstream = 0;
  }

//================>>> vPSPrinter::Setup <<<========================
  int vPSPrinter::Setup(char* fn)
  {
    // fn is an override default name

    int id, val;

    if (!_cmdsAdded)                    // only add commands once
      {
        AddDialogCmds(PrintDialog);	// Set up standard dialog
        _cmdsAdded = 1;
      }

    // set string of textin control to override name if there
    if (fn && *fn)
	SetString(21, fn);

    id = ShowModalDialog("Printer Setup", val);     // show and wait
    switch (id)                         // convert to our system
      {
        case M_OK:
	  {
	    char name[256];		// a buffer to retrieve name
	    GetTextIn(21,name,255);	// fetch the string user entered
	    if (!*name)			// Make sure gave a name
		return 0;
	    else			// copy user name to private copy
	      {
		if (_name)		// free old name
		    delete [] _name;
		_name = new char[strlen(name)+1];  // allocate new
		strcpy(_name,name);	// copy it
		return 1;
	      }
	  }

        case M_Cancel:
            return 0;
      };
    return 0;
  }

//================>>> vPSPrinter::DialogCommand <<<==================
  void vPSPrinter::DialogCommand(ItemVal id, ItemVal val, CmdType ctype)
  {

    switch (id)
      {

	case 22:	// "File Dialog"
	  {
	    char fname[256];
	    int fi = 0;

	    vFileSelect fsel(theApp);

	    fname[0] = 0;
	    if (fsel.FileSelectSave("Print to file", fname, 255, 0,fi))
	      {
		if (_name)		// free old name
		    delete [] _name;
		_name = new char[strlen(fname)+1];  // allocate new
		strcpy(_name,fname);	// copy it
		SetString(21, _name);
	      }
	    break;
	  }

	case 30:	// portrait
	    _portrait = val;
	    break;

	case 31:	// landscape
	    _portrait = !val;
	    break;

	case 33:	//"Color Printer"
	    _useColor = val;
	    break;

	case 41:	// Paper type
	    _paperType = val;
	    _width = PaperWidths[_paperType];
	    _height = PaperHeights[_paperType];
	    break;

	default:
	    vModalDialog::DialogCommand(id,val,ctype);
	    break;
      }

    if (id == M_Yes || id == M_Cancel)
        CloseDialog();
  }
