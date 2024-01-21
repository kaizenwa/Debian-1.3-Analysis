/*
 * hfsutils - tools for reading and writing Macintosh HFS volumes
 * Copyright (C) 1996, 1997 Robert Leslie
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

# include <stdio.h>

# include "hfsck.h"
# include "util.h"

# include "ck_mdb.h"

/*
 * NAME:	ck_mdb()
 * DESCRIPTION:	verify/repair the master directory block
 */
int ck_mdb(hfsvol *vol)
{
  MDB *mdb = &vol->mdb;

  printf("*** Checking volume MDB\n");

  if (VERBOSE)
    {
      int flag;

      printf("  drSigWord  = 0x%04x\n", mdb->drSigWord);

      printf("  drCrDate   = %s\n", mctime(mdb->drCrDate));
      printf("  drLsMod    = %s\n", mctime(mdb->drLsMod));

      printf("  drAtrb     =");
      flag = 0;
      if (mdb->drAtrb & HFS_ATRB_BUSY)
	{
	  printf("%s BUSY", flag ? " |" : "");
	  flag = 1;
	}
      if (mdb->drAtrb & HFS_ATRB_HLOCKED)
	{
	  printf("%s HLOCKED", flag ? " |" : "");
	  flag = 1;
	}
      if (mdb->drAtrb & HFS_ATRB_UMOUNTED)
	{
	  printf("%s UMOUNTED", flag ? " |" : "");
	  flag = 1;
	}
      if (mdb->drAtrb & HFS_ATRB_BBSPARED)
	{
	  printf("%s BBSPARED", flag ? " |" : "");
	  flag = 1;
	}
      if (mdb->drAtrb & HFS_ATRB_COPYPROT)
	{
	  printf("%s COPYPROT", flag ? " |" : "");
	  flag = 1;
	}
      if (mdb->drAtrb & HFS_ATRB_SLOCKED)
	{
	  printf("%s SLOCKED", flag ? " |" : "");
	  flag = 1;
	}
      printf("\n");

      printf("  drNmFls    = %u\n", mdb->drNmFls);
      printf("  drVBMSt    = %u\n", mdb->drVBMSt);
      printf("  drAllocPtr = %u\n", mdb->drAllocPtr);

      printf("  drNmAlBlks = %u\n", mdb->drNmAlBlks);
      printf("  drAlBlkSiz = %lu\n", mdb->drAlBlkSiz);
      printf("  drClpSiz   = %lu\n", mdb->drClpSiz);
      printf("  drAlBlSt   = %u\n", mdb->drAlBlSt);

      printf("  drNxtCNID  = %lu\n", mdb->drNxtCNID);
      printf("  drFreeBks  = %u\n", mdb->drFreeBks);
      printf("  drVN       = \"%s\"\n", mdb->drVN);

      printf("  drVolBkUp  = %s\n", mctime(mdb->drVolBkUp));
      printf("  drVSeqNum  = %u\n", mdb->drVSeqNum);
      printf("  drWrCnt    = %lu\n", mdb->drWrCnt);

      printf("  drXTClpSiz = %lu\n", mdb->drXTClpSiz);
      printf("  drCTClpSiz = %lu\n", mdb->drCTClpSiz);

      printf("  drNmRtDirs = %u\n", mdb->drNmRtDirs);
      printf("  drFilCnt   = %lu\n", mdb->drFilCnt);
      printf("  drDirCnt   = %lu\n", mdb->drDirCnt);

      /* drFndrInfo */

      printf("  drVCSize   = %u\n", mdb->drVCSize);
      printf("  drVBMCSize = %u\n", mdb->drVBMCSize);
      printf("  drCtlCSize = %u\n", mdb->drCtlCSize);

      printf("  drXTFlSize = %lu\n", mdb->drXTFlSize);
      printf("  drXTExtRec = %s\n", extstr(&mdb->drXTExtRec));

      printf("  drCTFlSize = %lu\n", mdb->drCTFlSize);
      printf("  drCTExtRec = %s\n", extstr(&mdb->drCTExtRec));
    }

  if (mdb->drSigWord != 0x4244 &&
      ask("Bad volume signature (0x%04x); should be 0x4244", mdb->drSigWord))
    {
      mdb->drSigWord = 0x4244;
      vol->flags |= HFS_UPDATE_MDB;
    }

  return 0;
}
