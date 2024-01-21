/**
 *
 * $Id: AtomMgr.c,v 1.6 1996/11/28 09:20:43 u27113 Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static char rcsid[] = "$Id: AtomMgr.c,v 1.6 1996/11/28 09:20:43 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>
#include <Xm/AtomMgr.h>
#include <XmI/AtomMgrI.h>
#include <Xm/XmP.h>

#include <XmI/DebugUtil.h>

/*
 * Improved atom cache management for LessTif -- This implementation uses
 * an open hashing algorithm. Details about such algorithms can be found
 * in every large book about computer algorithms...
 *
 * 25-Sep-95 by Harald Albrecht.
 * 08-Sep-96 Multidisplay patch
 */

/*
 * Global variables for use with the AtomManager. The use of two hash
 * tables will allow us not only to lookup fast atom id's from their names
 * but also to query for the name of an atom.
 */
static Boolean        NeedInit = True;
static XmAtomCacheRec NamesHashTable; /* Names --> Atom values      */
static XmAtomCacheRec AtomsHashTable; /* Atom values --> Atom Names */

/*
 * DumpHashTable -- during testing this function dumps a hash table's 
 * contents to stdout.
 */
static void
DumpHashTable(XmAtomCache Table)
{
    int i;

    XdbDebug(__FILE__, NULL, "hash table size: %i entries used out of %i\n",
           (int) Table->InUse, (int) (Table->HashMask + 1));
    for ( i = 0; i <= Table->HashMask; i++ )
        if ( Table->Entries[i].AtomName != NULL )
            XdbDebug(__FILE__, NULL,
		     "entry %i: (name: %s, atom: %i, display %08X)\n",
                     i,
                     (char *) Table->Entries[i].AtomName,
                     (int)    Table->Entries[i].AtomID,
                     (int)    Table->Entries[i].Dsp);
    XdbDebug(__FILE__, NULL, "\n");
} /* DumpHashTable */

/*
 * StringToHashValue -- returns a hash value for a given string. As with
 * every hash value function, one can argue about it for a lifetime. This
 * one creates very easy hash values where all bits are used. In my (humble)
 * oppinion it is better to spread bits around the key value as fast as
 * possible while also regarding the trailing characters of the string --
 * most often identifier strings do have a leading suffix and only the last
 * few characters differ.
 */
static unsigned int
StringToHashValue(String id, Display *Dsp)
{
    unsigned int HashValue, DisplayHash;
    char         ch;
    int          i;
    
    HashValue = 0;
    while ( (ch = *id++) )
        HashValue += (HashValue << 3) + ch;
    DisplayHash = (unsigned int) Dsp;
    for ( i = 0; i < 4; i++ ) {
        HashValue += DisplayHash;
        DisplayHash >>= 4;
    }
    return HashValue;
} /* StringToHashValue */

/*
 * AtomToHashValue -- returns a hash value for a given atom. As with String-
 * ToHashValue the display is also mangled and added to the hash value. This
 * should (hopefuly) be result in better spread hash values for multi display
 * applications.
 */
static unsigned int
AtomToHashValue(Atom AtomID, Display *Dsp)
{
    unsigned int HashValue, DisplayHash;
    int          i;
    
    HashValue = (unsigned int) AtomID;
    DisplayHash = (unsigned int) Dsp;
    for ( i = 0; i < 4; i++ ) {
        HashValue += DisplayHash;
        DisplayHash >>= 4;
    }
    return HashValue;
} /* AtomToHashValue */

/*
 * HashIndex() makes a valid index into the hash table out of a hash value,
 * whereas RehashOffset() calculates the delta offset for searching free
 * slots.
 */
#define HashIndex(Table, HashValue) \
    ((HashValue) & (Table->HashMask))
#define RehashOffset(Table, HashValue) \
    ((((HashValue) % (Table->RehashValue)) + 2) | 1)

#define NOT_FOUND ((unsigned int) ~0)

/*
 * InitHashTable -- initialize a hash table for further use. This is:
 * setting up the entries with NULL names and filling in the HashMask and
 * some other useful things.
 */
static void
InitHashTable(XmAtomCache Table)
{
    int              i;
    XmAtomCacheEntry Entry;
    
    Table->HashMask    = 0xFF; /* this MUST be always an ODD number, and
                                * all lower bits must be 1's!! 
                                */
    Table->RehashValue = Table->HashMask - 2;
    Table->InUse       = 0;
    Table->Entries     = (XmAtomCacheEntry)
                             XtMalloc(sizeof(XmAtomCacheEntryRec) *
                                                  (Table->HashMask + 1));
    Entry = Table->Entries;
    for ( i = 0; i <= Table->HashMask; i++ )
        Entry++->AtomName = NULL;
} /* InitHashTable */

/*
 * ExpandHashTable -- when a hash table gets crouded, this function makes
 * room for new entries. The old entries then will be spread around the
 * new table. To do this, the function must know whether it's working on
 * a hash table where the primary hashing key is the name (string) or the
 * atom identifier.
 */
static void
ExpandHashTable(XmAtomCache Table, Boolean StringIsPrimaryKey)
{
    unsigned int     OldHashMask;
    XmAtomCacheEntry OldEntries;
    XmAtomCacheEntry NewEntries;
    unsigned int     i, Index, HashValue, Rehash;
    
    OldHashMask = Table->HashMask;
    OldEntries  = Table->Entries;
    /*
     * Expand the hash table to twice the old size and allocate a 
     * memory block for the new table.
     */
    Table->HashMask    = (Table->HashMask << 1) + 1;
    Table->RehashValue = Table->HashMask - 2;
    Table->Entries     = (XmAtomCacheEntry) 
                             XtMalloc(sizeof(XmAtomCacheEntryRec) * 
                                                  (Table->HashMask + 1));
    /*
     * Now copy all entries into the new hash table. But first, we have
     * to initialize the new table -- otherwise we could stumble over
     * invalid entries with uninitialized atom names.
     */
    NewEntries = Table->Entries;
    for ( i = 0; i <= Table->HashMask; i++ )
        NewEntries++->AtomName = NULL;
    NewEntries = Table->Entries;
    for ( i = 0; i <= OldHashMask; i++ ) {
        if ( OldEntries[i].AtomName != NULL ) {
            HashValue = StringIsPrimaryKey ? 
                            StringToHashValue(OldEntries[i].AtomName,
                                              OldEntries[i].Dsp) :
                            AtomToHashValue  (OldEntries[i].AtomID,
                                              OldEntries[i].Dsp);
            Index     = HashIndex(Table, HashValue);
            if ( NewEntries[Index].AtomName != NULL ) {
                Rehash = RehashOffset(Table, HashValue);
                do {
                    Index = HashIndex(Table, Index + Rehash);
                } while ( NewEntries[Index].AtomName != NULL );
            }
            NewEntries[Index] = OldEntries[i];
        }
    }
    /*
     * do the clean up...
     */
    XtFree((char *) OldEntries);
} /* ExpandHashTable */

/*
 * Remove all entries for a given Display from the hash table. We won't
 * shrink the table at all but rearrange all entries within the table that
 * don't belong to the display, we just have cut the connection to.
 * NOTICE: The parameter StringIsPrimaryKey is also used to indicate when
 * to free the space for the names of the atoms. See also the note on
 * _XmFlushAtomsForDisplay().fh
 */
static void
FlushAtomsForDisplay(XmAtomCache Table, Display *Dsp,
                     Boolean StringIsPrimaryKey)
{
    unsigned int     HashMask;
    XmAtomCacheEntry OldEntries;
    XmAtomCacheEntry NewEntries;
    unsigned int     i, Index, HashValue, Rehash;
    
    HashMask       = Table->HashMask;
    OldEntries     = Table->Entries;
    Table->Entries = (XmAtomCacheEntry) 
                         XtMalloc(sizeof(XmAtomCacheEntryRec) * 
                                              (HashMask + 1));
    /*
     * Now copy all entries into the new hash table. Do not copy those
     * entries belonging to the display just closed.
     */
    NewEntries = Table->Entries;
    for ( i = 0; i <= HashMask; i++ )
        NewEntries++->AtomName = NULL;
    NewEntries = Table->Entries;
    for ( i = 0; i <= HashMask; i++ ) {
        if ( OldEntries[i].AtomName != NULL ) {
	    if ( OldEntries[i].Dsp != Dsp ) {
	        /*
	         * All entries not belonging to this particular display
		 * are copied into the new table at a rehashed position.
		 */
                HashValue = StringIsPrimaryKey ? 
                                StringToHashValue(OldEntries[i].AtomName,
                                                  OldEntries[i].Dsp) :
                                AtomToHashValue  (OldEntries[i].AtomID,
                                                  OldEntries[i].Dsp);
                Index     = HashIndex(Table, HashValue);
                if ( NewEntries[Index].AtomName != NULL ) {
                    Rehash = RehashOffset(Table, HashValue);
                    do {
                        Index = HashIndex(Table, Index + Rehash);
                    } while ( NewEntries[Index].AtomName != NULL );
                }
                NewEntries[Index] = OldEntries[i];
	    } else {
	        Table->InUse--;
	        if ( StringIsPrimaryKey ) {
		    XtFree(OldEntries[i].AtomName);
		}
	    }
        }
    }
    /*
     * do the clean up...
     */
    XtFree((char *) OldEntries);
} /* FlushAtomsForDisplay */

/*
 * GetIndexFromName -- returns the index for a given atom name. If the
 * atom name isn't in the given hash table, the function returns the
 * special index value NOT_FOUND.
 */
static unsigned int
GetIndexFromName(XmAtomCache Table, String AtomName, Display *Dsp)
{
    unsigned int Index, HashValue, Rehash;
    
    HashValue = StringToHashValue(AtomName, Dsp);
    Index     = HashIndex(Table, HashValue);
    if ( Table->Entries[Index].AtomName != NULL ) {
        Rehash = RehashOffset(Table, HashValue);
        do {
            if ( (strcmp(AtomName, Table->Entries[Index].AtomName) == 0)
                 && (Table->Entries[Index].Dsp == Dsp) )
                return Index;
            Index = HashIndex(Table, Index + Rehash);
        } while ( Table->Entries[Index].AtomName != NULL );
    }
    return NOT_FOUND;
} /* GetIndexFromName */

/*
 * GetIndexFromAtom -- look up an entry. If the appropiate entry can't
 * be found (from AtomID), the function returns the special index
 * NOT_FOUND to the caller.
 */
static unsigned int
GetIndexFromAtom(XmAtomCache Table, Atom AtomID, Display *Dsp)
{
    unsigned int Index, HashValue, Rehash;

    HashValue = AtomToHashValue(AtomID, Dsp);
    Index     = HashIndex(Table, HashValue);
    if ( Table->Entries[Index].AtomName != NULL ) {
        Rehash = RehashOffset(Table, HashValue);
        do {
            if ( (Table->Entries[Index].AtomID == AtomID)
                 && (Table->Entries[Index].Dsp == Dsp) )
                return Index;
            Index = HashIndex(Table, Index + Rehash);
        } while ( Table->Entries[Index].AtomName != NULL );
    }
    return NOT_FOUND;
} /* GetIndexFromAtom */

/*
 * Add -- add a new entry to a hash table. The hash table can be either
 * indexed by the atom's name or by its atom identifier. This functions
 * relies on the caller responsible for rejecting duplicate entries. In
 * addition, the caller must specify a copy of the atom's name. The
 * memory for that string then belongs to the cache.
 */
static void
AddToHashTable(XmAtomCache Table,
               String AtomName, Atom AtomID, Display *Dsp,
               Boolean StringIsPrimaryKey)
{
    unsigned int Index, HashValue, Rehash;
    
    /*
     * If the hash table is getting crowed, we'll allocate here new
     * space for the entries.
     */
    if ( Table->InUse + (Table->InUse >> 2) >= Table->HashMask ) {
        ExpandHashTable(Table, StringIsPrimaryKey);
    }
    /*
     * Now get the hash value and look for a free place in the hash
     * table.
     */
    HashValue = StringIsPrimaryKey ? StringToHashValue(AtomName, Dsp) :
                                     AtomToHashValue(AtomID, Dsp);
    Index = HashIndex(Table, HashValue);
    if ( Table->Entries[Index].AtomName != NULL ) {
        Rehash = RehashOffset(Table, HashValue);
        do {
            Index = HashIndex(Table, Index + Rehash);
        } while ( Table->Entries[Index].AtomName != NULL );
    }
    /*
     * Finaly add the new entry.
     */
    Table->InUse++;
    Table->Entries[Index].AtomName = AtomName;
    Table->Entries[Index].AtomID   = AtomID;
    Table->Entries[Index].Dsp      = Dsp;
} /* AddToHashTable */

/*
 * _XmInternAtomAndName -- As this is an undocumented function, I'm not
 * going to describe it ;-)
 */
void
_XmInternAtomAndName(Display *display, Atom atom, String name)
{
    unsigned int Index;
    
    if ( NeedInit ) {
        NeedInit = False;
        InitHashTable(&NamesHashTable); InitHashTable(&AtomsHashTable);
    }
    /*
     * Search a free slot for the new atom name. If the atom is not already
     * in the cache then use the slot returned by GetIndexFromName(). Don't
     * forget to add the atom id to the `atomic hash table' too.
     */
    Index = GetIndexFromName(&NamesHashTable, name, display);
    if ( Index == NOT_FOUND ) {
        name = XtNewString(name);
        AddToHashTable(&NamesHashTable, name, atom, display, True);
        AddToHashTable(&AtomsHashTable, name, atom, display, False);
	if (XdbInDebug(__FILE__, NULL)) {
            DumpHashTable(&NamesHashTable);
            DumpHashTable(&AtomsHashTable);
	}
    }
} /* _XmInternAtomAndName */

/*
 * XmInternAtom -- same as XInternAtom(), but with a cache. This cache
 * (hopefully) improves the performance, if an application has to look
 * up atoms (probably the same) many times.
 */
Atom
XmInternAtom(Display *display, String name, Boolean only_if_exists)
{
    unsigned Index;
    Atom     AtomID;
    
    if ( NeedInit ) {
        NeedInit = False;
        InitHashTable(&NamesHashTable); InitHashTable(&AtomsHashTable);
    }
    /*
     * Try to find the atom id in the cache first.
     */
    Index = GetIndexFromName(&NamesHashTable, name, display);
    if ( Index == NOT_FOUND ) {
        /*
         * The atom is not in the cache (yet). Therefore ask the X server
         * for it. And if the server doesn't know anything about that atom,
         * and the caller doesn't want to create it, we just return without
         * doing anything more.
         */
        AtomID = XInternAtom(display, name, only_if_exists);
        if ( AtomID == (Atom) None )
            return (Atom) None;
        /*
         * The user wants an atom, so we have to give him one. Hey, no
         * neutrinos allowed! Do not make a call here to _XmInternAtom-
         * AndName() as this would result in another (failing) look up
         * attempt in the cache. Because we know here, that the atom isn't
         * in the cache the should avoid a second look up.
         */
        name = XtNewString(name);
        AddToHashTable(&NamesHashTable, name, AtomID, display, True);
        AddToHashTable(&AtomsHashTable, name, AtomID, display, False);
	if (XdbInDebug(__FILE__, NULL)) {
            DumpHashTable(&NamesHashTable);
            DumpHashTable(&AtomsHashTable);
	}
        return AtomID;
    } else {
        /*
         * The atom is already registered, thus we can return it's atom
         * identifier immediatly.
         */
        return NamesHashTable.Entries[Index].AtomID;
    }
} /* XmInternAtom */

/*
 * XmGetAtomName -- get the name of an already registered atom. If the
 * atom isn't in the cache we'll query the X server. If this fails, the
 * server will bounce back a BadAtom error.
 */
String
XmGetAtomName(Display *display, Atom atom)
{
    unsigned int Index;
    String       AtomName, name;
    
    if ( NeedInit ) {
        NeedInit = False;
        InitHashTable(&NamesHashTable); InitHashTable(&AtomsHashTable);
    }
    /*
     * If the atom is not yet in the cache, we'll have to make a round
     * trip to the server and ask it for the name of the atom. To speed
     * up further requests for the same atom's name, we put the name into
     * the cache.
     */
    Index = GetIndexFromAtom(&AtomsHashTable, atom, display);
    if ( Index == NOT_FOUND ) {
        AtomName = XGetAtomName(display, atom);
        if ( AtomName != NULL ) {
            name = XtNewString(AtomName);
            AddToHashTable(&NamesHashTable, name, atom, display, True);
            AddToHashTable(&AtomsHashTable, name, atom, display, False);
	    if (XdbInDebug(__FILE__, NULL)) {
		DumpHashTable(&NamesHashTable);
		DumpHashTable(&AtomsHashTable);
	    }
        }
        return AtomName;
    } else {
        /*
         * This is like XGetAtomName()... return a copy of the atom's
         * name to the caller.
         */
        return XtNewString(AtomsHashTable.Entries[Index].AtomName);
    }
} /* XmGetAtomName */

/*
 * _XmFlushAtomsForDisplay -- LessTif specific helper function, to be used
 * within LessTif only. After a display connection has been closed, this
 * removes all entries for that display from both caches. Otherwise a new
 * display connection may return the same display pointer albeit representing
 * a connection to a different display (server) and thus causing trouble
 * because the client will get the wrong atom id's from the cache.
 */
void
_XmFlushAtomsForDisplay(Display *display)
{
    /*
     * Never, Never, NEVER(!), change the calling sequence below!!!
     * FlushAtomsForDisplay() relies on this as it must clean up the
     * memory occupied by the names of the atoms.
     */
    FlushAtomsForDisplay(&AtomsHashTable, display, False);
    FlushAtomsForDisplay(&NamesHashTable, display, True);
} /* _XmFlushAtomsForDisplay */
