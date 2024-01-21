//===============================================================
// vwinprtr.h: Windows Printer class .h file - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VWINPRTR_H
#define VWINPRTR_H

#include <v/v_defs.h>
#include <v/vmodald.h>

#define vPaperLetter 0
#define vPaperLegal 1
#define vPaperTabloid 2
#define vPaperLedger 3
#define vPaperStatement 4
#define vPaperExecutive 5
#define vPaperA3 6
#define vPaperA4 7
#define vPaperA5 8
#define vPaperB4 9
#define vPaperB5 10
#define vPaperFolio 11
#define vPaperQuarto 12
#define vPaper10x14 13

#define vPaperDefault vPaperLetter              // Index to default paper

    class vWinPrinter
      {
      public:           //---------------------------------------- public
        vWinPrinter();
        virtual ~vWinPrinter();
        vWinPrinter(const vWinPrinter& pr);

        vWinPrinter& operator =(const vWinPrinter& pr);

        int GetHeight() {return _height;}
        int GetWidth() {return _width;}

        char* GetDocName() {return _name;}

        int GetPortrait(){return _portrait;}
        void SetPortrait(int p) {_portrait = p;}

        int GetUseColors() {return _useColor;}
        void SetUseColors(int c) {_useColor = c;}

        int GetPaper() {return _paperType;}
        char* GetPaperName();

        int GetCopies() {return _copies;}
        void SetCopies(int s) {_copies = s;}

        int GetToFile() {return _toFile;}

        int Setup(char* fn = 0);

        HDC GetHDC() { return _printhDC; }

      protected:        //--------------------------------------- protected

      private:          //--------------------------------------- private
        
        static int _instances;
        HDC _printhDC;

        // Printer attributes
        char* _name;            // name of stream

        int _width;             // width of printer
        int _height;            // height of printer
        int _portrait;  // true if portrait, else landscape
        int _useColor;  // true if printer supports colors

        int _paperType;
        int _copies;
        int _toFile;
      };

#endif
