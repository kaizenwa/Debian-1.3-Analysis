/*    e_print.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

int EBuffer::BlockPrint() {
    static char cr = 13;
    static char lf = 10;
    EPoint B, E;
    int L;
    int A, Z;
    PELine LL;
    int fd;
    int bc = 0, lc = 0;
    
    AutoExtend = 0;
    if (CheckBlock() == 0) return 0;
    if (RCount == 0) return 0;
    B = BB;
    E = BE;
    Msg(INFO, "Printing to %s...", PrintDevice);
    fd = open(PrintDevice, O_RDWR | O_BINARY | O_CREAT | O_TRUNC, 0666);
    if (fd == -1) goto erroropen;
    for (L = B.Row; L <= E.Row; L++) {
        A = -1;
        Z = -1;
        LL = RLine(L);
        switch (BlockMode) {
          case bmLine:
            if (L < E.Row) {
                A = 0;
                Z = LL->Count;
            }
            break;
          case bmColumn:
            if (L < E.Row) {
                A = CharOffset(LL, B.Col);
                Z = CharOffset(LL, E.Col);
            }
            break;
          case bmStream:
            if (B.Row == E.Row) {
                A = CharOffset(LL, B.Col);
                Z = CharOffset(LL, E.Col);
            } else if (L == B.Row) {
                A = CharOffset(LL, B.Col);
                Z = LL->Count;
            } else if (L < E.Row) {
                A = 0;
                Z = LL->Count;
            } else if (L == E.Row) {
                A = 0;
                Z = CharOffset(LL, E.Col);
            }
            break;
        }
        if (A != -1 && Z != -1) {
            if (A < LL->Count) {
                if (Z > LL->Count)
                    Z = LL->Count;
                if (Z > A) {
                    if (write(fd, LL->Chars + A, Z - A) != Z - A) {
                        goto error;
                    } else
                        bc += Z - A;
                }
            }
            if (BFI(this, BFI_AddCR) == 1)
                if (write(fd, &cr, 1) != 1)
                    goto error;
                else
                    bc++;
            if (BFI(this, BFI_AddLF) == 1)
                if (write(fd, &lf, 1) != 1)
                    goto error;
                else {
                    bc++;
                    lc++;
                }
            if ((lc % 200) == 0)
                Msg(INFO, "Printing, %d lines, %d bytes.", lc, bc);

        }
    }
    close(fd);
    Msg(INFO, "Printing %d lines, %d bytes.", lc, bc);
    return 1;
error:
    close(fd);
erroropen:
    Msg(INFO, "Failed to write to %s", PrintDevice);
    return 0;
}


int EBuffer::FilePrint() {
    static char cr = 13;
    static char lf = 10;
    int l;
    int fd;
    long ByteCount = 0;
    int BChars;
  
    Msg(INFO, "Printing %s to %s...", FileName, PrintDevice);
    fd = open(PrintDevice, O_RDWR | O_BINARY | O_CREAT | O_TRUNC, 0666);
    if (fd == -1) goto erroropen;
    BChars = 0;
    for (l = 0; l < RCount; l++) {
        if ((int) sizeof(FileBuffer) - (BChars + 2) < RLine(l)->Count) {
            if (BChars) {
                ByteCount += BChars;
                Msg(INFO, "Printing: %d lines, %d bytes.", l, ByteCount);
                if (write(fd, FileBuffer, BChars) != BChars) goto fail;
                BChars = 0;
            }
        }
        if (RLine(l)->Count > sizeof(FileBuffer) - 2) {
            assert(BChars == 0);
            ByteCount += RLine(l)->Count;
            Msg(INFO, "Printing: %d lines, %d bytes.", l, ByteCount);
            if (write(fd, RLine(l)->Chars, RLine(l)->Count) != RLine(l)->Count) goto fail;
        } else {
            memcpy(FileBuffer + BChars, RLine(l)->Chars, RLine(l)->Count);
            BChars += RLine(l)->Count;
        }
        if ((l < RCount - 1) || BFI(this, BFI_ForceNewLine)) {
            assert(sizeof(FileBuffer) >= BChars + 2);
            if (BFI(this, BFI_AddCR) == 1) FileBuffer[BChars++] = cr;
            if (BFI(this, BFI_AddLF) == 1) FileBuffer[BChars++] = lf;
        }
    }
    if (BChars) {
        ByteCount += BChars;
        Msg(INFO, "Printing: %d lines, %d bytes.", l, ByteCount);
        if (write(fd, FileBuffer, BChars) != BChars) goto fail;
    }
    BChars = 0;
    if (close(fd) != 0) goto fail;
    Msg(INFO, "Printed %s.", FileName);
    return 1;
fail:
    close(fd);
    return 0;
erroropen:
    Msg(ERROR, "Error printing %s to %s.", FileName, PrintDevice);
    return 0;
}
