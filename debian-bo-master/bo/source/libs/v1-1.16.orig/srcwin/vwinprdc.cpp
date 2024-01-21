//===============================================================
// vWinPrinterC - a basic canvas for drawing
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================


#include <v/vwin32.h>		// for Win 32 stuff

#include <math.h>


#include <v/vwinprdc.h>
#include <v/vapp.h>		// need access to the app

//-----------------------------------------------------------------------

//================>>> vWinPrinterDC::vWinPrinterDC <<<========================
  vWinPrinterDC::vWinPrinterDC() : vWinDC()
  {
    SysDebug(Constructor,"vWinPrinterDC::vWinPrinterDC() constructor\n")
    _isPrinterDC = 1;
  }

//================>>> vWinPrinterDC::~vWinPrinterDC <<<========================
  vWinPrinterDC::~vWinPrinterDC()
  {

    SysDebug(Destructor,"vWinPrinterDC::~vWinPrinterDC() destructor\n")

  }

//================>>> vWinPrinterDC::SetBackground <<<==========================
  void vWinPrinterDC::SetBackground(vColor& color)
  {

   _canvasBG = color.pixel();		// retrieve X pixel value

    GetHDC();
    ::SetBkColor(_hdc, _canvasBG);
    ReleaseHDC();
  }

//======================>>> vWinPrinterDC::SetFont <<<===========================
  void vWinPrinterDC::SetFont(vFont& vf)
  {
    // Change the font associated with this window.

    _font = vf;
  }

//================>>> vWinPrinterDC::BeginPrinting <<<========================
  int vWinPrinterDC::BeginPrinting()
  {

    DOCINFO docinfo;

    if (!_printer.GetHDC())		// should not happen...
      {
	return 0;
      }

    _pages = 0;

    docinfo.cbSize = sizeof(DOCINFO);	// set this size...
    docinfo.lpszDocName = "VPrintJob";
    docinfo.lpszOutput = 0;		// no file output...

    if (::StartDoc(_printer.GetHDC(), &docinfo) < 0)
	return 0;


    BeginPage();

    return 1;

  }

//================>>> vWinPrinterDC::BeginPage <<<========================
  void vWinPrinterDC::BeginPage()
  {
    HDC pHDC = _printer.GetHDC();
    if (!pHDC)
	return;
    ++_pages;		// bump number of pages so far

    // I'm not sure exactly why, but the printer DC seems
    // to have all this set again after an EndPage has been
    // done, so I will always do it now at a begin page.

    int pixw = ::GetDeviceCaps(pHDC, HORZRES);
    int pixh = ::GetDeviceCaps(pHDC, VERTRES);
    // This scaling gives about the same proportions as the X version
    int width = ::GetDeviceCaps(pHDC, HORZSIZE) * 3;
    int height = ::GetDeviceCaps(pHDC,VERTSIZE) * 3;

    ::SetMapMode(pHDC, MM_ISOTROPIC);
    ::SetWindowExt(pHDC,width,height);
    ::SetViewportExt(pHDC,pixw,pixh);

    ::StartPage(_printer.GetHDC());
  }

//================>>> vWinPrinterDC::EndPage <<<========================
  void vWinPrinterDC::EndPage()
  {
    if (!_printer.GetHDC())
	return;
    ::EndPage(_printer.GetHDC());
  }

//================>>> vWinPrinterDC::EndPrinting <<<========================
  void vWinPrinterDC::EndPrinting()
  {
    if (!_printer.GetHDC())
	return;

    EndPage();

    ::EndDoc(_printer.GetHDC());

  }

//=====================>>> vWinPrinterDC::SetPrinter <<<============================
 void vWinPrinterDC::SetPrinter(vPrinter& printer)
  {
    _printer = printer;
    _physHeight = _printer.GetHeight();
    _physWidth = _printer.GetWidth();

  }


