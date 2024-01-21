/* 
 * tclMacFCmd.c --
 *
 * Implements the Macintosh specific portions of the file manipulation
 * subcommands of the "file" command.
 *
 * Copyright (c) 1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclMacFCmd.c 1.11 96/10/10 10:13:49
 */

#include "tclInt.h"
#include "tclMacInt.h"
#include "tclPort.h"
#include <FSpCompat.h>
#include <MoreFilesExtras.h>
#include <Strings.h>
#include <Errors.h>
#include <FileCopy.h>
#include <DirectoryCopy.h>
#include <Script.h>
#include <string.h>

/*
 * Prototypes for procedure only used in this file
 */

OSErr			FSpGetFLockCompat(const FSSpec *specPtr, 
			    Boolean *lockedPtr);

static OSErr		GenerateUniqueName(short vRefNum, long dirID1, 
			    long dirID2, Str31 uniqueName);
static OSErr		GetFileSpecs(char *path, FSSpec *pathSpecPtr,
			    FSSpec *dirSpecPtr,	Boolean *pathExistsPtr,	
			    Boolean *pathIsDirectoryPtr);
static OSErr		MoveRename(const FSSpec *srcSpecPtr, 
			    const FSSpec *dstSpecPtr, StringPtr copyName);
static int		Pstrequal(ConstStr255Param stringA, 
			    ConstStr255Param stringB);

static pascal Boolean CopyErrHandler(
    OSErr error,              /* Error that occured */
    short failedOperation,    /* operation that caused the error */
    short srcVRefNum,         /* volume ref number of source */
    long srcDirID,            /* directory id of source */
    StringPtr srcName,        /* name of source */
    short dstVRefNum,         /* volume ref number of dst */
    long dstDirID,            /* directory id of dst */
    StringPtr dstName         /* name of dst directory */
) ;
          
                 
/*
 *---------------------------------------------------------------------------
 *
 * TclpRenameFile --
 *
 *      Changes the name of an existing file or directory, from src to dst.
 *	If src and dst refer to the same file or directory, does nothing
 *	and returns success.  Otherwise if dst already exists, it will be
 *	deleted and replaced by src subject to the following conditions:
 *	    If src is a directory, dst may be an empty directory.
 *	    If src is a file, dst may be a file.
 *	In any other situation where dst already exists, the rename will
 *	fail.  
 *
 * Results:
 *	If the directory was successfully created, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR and errno is set to
 *	indicate the error.  Some possible values for errno are:
 *
 *	EACCES:     src or dst parent directory can't be read and/or written.
 *	EEXIST:	    dst is a non-empty directory.
 *	EINVAL:	    src is a root directory or dst is a subdirectory of src.
 *	EISDIR:	    dst is a directory, but src is not.
 *	ENOENT:	    src doesn't exist.  src or dst is "".
 *	ENOTDIR:    src is a directory, but dst is not.  
 *	EXDEV:	    src and dst are on different filesystems.
 *	
 * Side effects:
 *	The implementation of rename may allow cross-filesystem renames,
 *	but the caller should be prepared to emulate it with copy and
 *	delete if errno is EXDEV.
 *
 *---------------------------------------------------------------------------
 */

int
TclpRenameFile( 
    char *src,    		/* Pathname of file or dir to be renamed. */
    char *dst)     		/* New pathname for file or directory. */
{
    FSSpec srcFileSpec, dstFileSpec, dstDirSpec;
    OSErr err; 
    long srcID, dummy;
    Boolean srcIsDirectory, dstIsDirectory, dstExists, dstLocked;

    err = FSpLocationFromPath(strlen(src), src, &srcFileSpec);
    if (err == noErr) {
	FSpGetDirectoryID(&srcFileSpec, &srcID, &srcIsDirectory);
    }
    if (err == noErr) {
        err = GetFileSpecs(dst, &dstFileSpec, &dstDirSpec, &dstExists, 
        	&dstIsDirectory);
    }
    if (err == noErr) {
	if (dstExists == 0) {
            err = MoveRename(&srcFileSpec, &dstDirSpec, dstFileSpec.name);
            goto end;
        }
        err = FSpGetFLockCompat(&dstFileSpec, &dstLocked);
        if (dstLocked) {
            FSpRstFLockCompat(&dstFileSpec);
        }
    }
    if (err == noErr) {
        if (srcIsDirectory) {
	    if (dstIsDirectory) {
		/*
		 * The following call will remove an empty directory.  If it
		 * fails, it's because it wasn't empty.
		 */
		 
                if (TclpRemoveDirectory(dst, 0, NULL) != TCL_OK) {
                    return TCL_ERROR;
                }
                
                /*
		 * Now that that empty directory is gone, we can try
		 * renaming src.  If that fails, we'll put this empty
		 * directory back, for completeness.
		 */

		err = MoveRename(&srcFileSpec, &dstDirSpec, dstFileSpec.name);
                if (err != noErr) {
		    FSpDirCreateCompat(&dstFileSpec, smSystemScript, &dummy);
		    if (dstLocked) {
		        FSpSetFLockCompat(&dstFileSpec);
		    }
		}
	    } else {
	        errno = ENOTDIR;
	        return TCL_ERROR;
	    }
	} else {   
	    if (dstIsDirectory) {
		errno = EISDIR;
		return TCL_ERROR;
	    } else {                                
		/*
		 * Overwrite existing file by:
		 * 
		 * 1. Rename existing file to temp name.
		 * 2. Rename old file to new name.
		 * 3. If success, delete temp file.  If failure,
		 *    put temp file back to old name.
		 */

	        Str31 tmpName;
	        FSSpec tmpFileSpec;

	        err = GenerateUniqueName(dstFileSpec.vRefNum, 
	        	dstFileSpec.parID, dstFileSpec.parID, tmpName);
	        if (err == noErr) {
	            err = FSpRenameCompat(&dstFileSpec, tmpName);
	        }
	        if (err == noErr) {
	            err = FSMakeFSSpecCompat(dstFileSpec.vRefNum,
	            	    dstFileSpec.parID, tmpName, &tmpFileSpec);
	        }
	        if (err == noErr) {
	            err = MoveRename(&srcFileSpec, &dstDirSpec, 
	            	    dstFileSpec.name);
	        }
	        if (err == noErr) {
		    FSpDeleteCompat(&tmpFileSpec);
		} else {
		    FSpDeleteCompat(&dstFileSpec);
		    FSpRenameCompat(&tmpFileSpec, dstFileSpec.name);
	            if (dstLocked) {
	            	FSpSetFLockCompat(&dstFileSpec);
	            }
	        }
	    }
   	}
    }    

    end:    
    if (err != noErr) {
	errno = TclMacOSErrorToPosixError(err);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpCopyFile --
 *
 *      Copy a single file (not a directory).  If dst already exists and
 *	is not a directory, it is removed.
 *
 * Results:
 *	If the file was successfully copied, returns TCL_OK.  Otherwise
 *	the return value is TCL_ERROR and errno is set to indicate the
 *	error.  Some possible values for errno are:
 *
 *	EACCES:     src or dst parent directory can't be read and/or written.
 *	EISDIR:	    src or dst is a directory.
 *	ENOENT:	    src doesn't exist.  src or dst is "".
 *
 * Side effects:
 *      This procedure will also copy symbolic links, block, and
 *      character devices, and fifos.  For symbolic links, the links 
 *      themselves will be copied and not what they point to.  For the
 *	other special file types, the directory entry will be copied and
 *	not the contents of the device that it refers to.
 *
 *---------------------------------------------------------------------------
 */
 
int 
TclpCopyFile(
    char *src,			/* Pathname of file to be copied. */
    char *dst)			/* Pathname of file to copy to. */
{
    OSErr err, dstErr;
    Boolean dstExists, dstIsDirectory, dstLocked;
    FSSpec srcFileSpec, dstFileSpec, dstDirSpec, tmpFileSpec;
    Str31 tmpName;
	
    err = FSpLocationFromPath(strlen(src), src, &srcFileSpec);
    if (err == noErr) {
        err = GetFileSpecs(dst, &dstFileSpec, &dstDirSpec, &dstExists,
        	&dstIsDirectory);
    }
    if (dstExists) {
        if (dstIsDirectory) {
            errno = EISDIR;
            return TCL_ERROR;
        }
        err = FSpGetFLockCompat(&dstFileSpec, &dstLocked);
        if (dstLocked) {
            FSpRstFLockCompat(&dstFileSpec);
        }
        
        /*
         * Backup dest file.
         */
         
        dstErr = GenerateUniqueName(dstFileSpec.vRefNum, dstFileSpec.parID, 
    	        dstFileSpec.parID, tmpName);
        if (dstErr == noErr) {
            dstErr = FSpRenameCompat(&dstFileSpec, tmpName);
        }   
    }
    if (err == noErr) {
    	err = FSpFileCopy(&srcFileSpec, &dstDirSpec, 
    		(StringPtr) dstFileSpec.name, NULL, 0, true);
    }
    if ((dstExists != false) && (dstErr == noErr)) {
        FSMakeFSSpecCompat(dstFileSpec.vRefNum, dstFileSpec.parID,
        	tmpName, &tmpFileSpec);
	if (err == noErr) {
	    /* 
	     * Delete backup file. 
	     */
	     
	    FSpDeleteCompat(&tmpFileSpec);
	} else {
	
	    /* 
	     * Restore backup file.
	     */
	     
	    FSpDeleteCompat(&dstFileSpec);
	    FSpRenameCompat(&tmpFileSpec, dstFileSpec.name);
	    if (dstLocked) {
	        FSpSetFLockCompat(&dstFileSpec);
	    }
	}
    }
    
    if (err != noErr) {
	errno = TclMacOSErrorToPosixError(err);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpDeleteFile --
 *
 *      Removes a single file (not a directory).
 *
 * Results:
 *	If the file was successfully deleted, returns TCL_OK.  Otherwise
 *	the return value is TCL_ERROR and errno is set to indicate the
 *	error.  Some possible values for errno are:
 *
 *	EACCES:     a parent directory can't be read and/or written.
 *	EISDIR:	    path is a directory.
 *	ENOENT:	    path doesn't exist or is "".
 *
 * Side effects:
 *      The file is deleted, even if it is read-only.
 *
 *---------------------------------------------------------------------------
 */

int
TclpDeleteFile( 
    char *path)			/* Pathname of file to be removed. */
{
    OSErr err;
    FSSpec fileSpec;
    Boolean isDirectory;
    long dirID;
	
    err = FSpLocationFromPath(strlen(path), path, &fileSpec);
    if (err == noErr) {
	/*
     	 * Since FSpDeleteCompat will delete an empty directory, make sure
     	 * that this isn't a directory first.
         */
        
        FSpGetDirectoryID(&fileSpec, &dirID, &isDirectory);
	if (isDirectory == true) {
            errno = EISDIR;
            return TCL_ERROR;
        }
    }
    err = FSpDeleteCompat(&fileSpec);
    if (err == fLckdErr) {
    	FSpRstFLockCompat(&fileSpec);
    	err = FSpDeleteCompat(&fileSpec);
    	if (err != noErr) {
    	    FSpSetFLockCompat(&fileSpec);
    	}
    }
    if (err != noErr) {
	errno = TclMacOSErrorToPosixError(err);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpCreateDirectory --
 *
 *      Creates the specified directory.  All parent directories of the
 *	specified directory must already exist.  The directory is
 *	automatically created with permissions so that user can access
 *	the new directory and create new files or subdirectories in it.
 *
 * Results:
 *	If the directory was successfully created, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR and errno is set to
 *	indicate the error.  Some possible values for errno are:
 *
 *	EACCES:     a parent directory can't be read and/or written.
 *	EEXIST:	    path already exists.
 *	ENOENT:	    a parent directory doesn't exist.
 *
 * Side effects:
 *      A directory is created with the current umask, except that
 *	permission for u+rwx will always be added.
 *
 *---------------------------------------------------------------------------
 */

int
TclpCreateDirectory(
    char *path)			/* Pathname of directory to create. */
{
    OSErr err;
    FSSpec dirSpec;
    long outDirID;
	
    err = FSpLocationFromPath(strlen(path), path, &dirSpec);
    if (err == noErr) {
        err = dupFNErr;		/* EEXIST. */
    } else if (err == fnfErr) {
        err = FSpDirCreateCompat(&dirSpec, smSystemScript, &outDirID);
    } 
    
    if (err != noErr) {
	errno = TclMacOSErrorToPosixError(err);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpCopyDirectory --
 *
 *      Recursively copies a directory.  The target directory dst must
 *	not already exist.  Note that this function does not merge two
 *	directory hierarchies, even if the target directory is an an
 *	empty directory.
 *
 * Results:
 *	If the directory was successfully copied, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR, errno is set to indicate
 *	the error, and the pathname of the file that caused the error
 *	is stored in errorPtr.  See TclpCreateDirectory and TclpCopyFile
 *	for a description of possible values for errno.
 *
 * Side effects:
 *      An exact copy of the directory hierarchy src will be created
 *	with the name dst.  If an error occurs, the error will
 *      be returned immediately, and remaining files will not be
 *	processed.
 *
 *---------------------------------------------------------------------------
 */

int
TclpCopyDirectory(
    char *src,			/* Pathname of directory to be copied.  */
    char *dst,			/* Pathname of target directory. */
    Tcl_DString *errorPtr)	/* If non-NULL, initialized DString for
				 * error reporting. */
{
    OSErr err, saveErr;
    long srcID, tmpDirID;
    FSSpec srcFileSpec, dstFileSpec, dstDirSpec, tmpDirSpec, tmpFileSpec;
    Boolean srcIsDirectory, srcLocked;
    Boolean dstIsDirectory, dstExists;
    Str31 tmpName;

    err = FSpLocationFromPath(strlen(src), src, &srcFileSpec);
    if (err == noErr) {
    	err = FSpGetDirectoryID(&srcFileSpec, &srcID, &srcIsDirectory);
    }
    if (err == noErr) {
        if (srcIsDirectory == false) {
            err = afpObjectTypeErr;	/* ENOTDIR. */
        }
    }
    if (err == noErr) {
        err = GetFileSpecs(dst, &dstFileSpec, &dstDirSpec, &dstExists,
        	&dstIsDirectory);
    }
    if (dstExists) {
        if (dstIsDirectory == false) {
            err = afpObjectTypeErr;	/* ENOTDIR. */
        } else {
            err = dupFNErr;		/* EEXIST. */
        }
    }
    if (err != noErr) {
        goto done;
    }        
    if ((srcFileSpec.vRefNum == dstFileSpec.vRefNum) &&
    	    (srcFileSpec.parID == dstFileSpec.parID) &&
            (Pstrequal(srcFileSpec.name, dstFileSpec.name) != 0)) {
        /*
         * Copying on top of self.  No-op.
         */
                    
        goto done;
    }

    /*
     * This algorthm will work making a copy of the source directory in
     * the current directory with a new name, in a new directory with the
     * same name, and in a new directory with a new name:
     *
     * 1. Make dstDir/tmpDir.
     * 2. Copy srcDir/src to dstDir/tmpDir/src
     * 3. Rename dstDir/tmpDir/src to dstDir/tmpDir/dst (if necessary).
     * 4. CatMove dstDir/tmpDir/dst to dstDir/dst.
     * 5. Remove dstDir/tmpDir.
     */
                
    err = FSpGetFLockCompat(&srcFileSpec, &srcLocked);
    if (srcLocked) {
        FSpRstFLockCompat(&srcFileSpec);
    }
    if (err == noErr) {
        err = GenerateUniqueName(dstFileSpec.vRefNum, dstFileSpec.parID, 
    	        dstFileSpec.parID, tmpName);
    }
    if (err == noErr) {
        FSMakeFSSpecCompat(dstFileSpec.vRefNum, dstFileSpec.parID,
        	tmpName, &tmpDirSpec);
        err = FSpDirCreateCompat(&tmpDirSpec, smSystemScript, &tmpDirID);
    }
    if (err == noErr) {
	err = FSpDirectoryCopy(&srcFileSpec, &tmpDirSpec, NULL, 0, true,
	    	CopyErrHandler);
    }
    
    /* 
     * Even if the Copy failed, Rename/Move whatever did get copied to the
     * appropriate final destination, if possible.  
     */
     
    saveErr = err;
    err = noErr;
    if (Pstrequal(srcFileSpec.name, dstFileSpec.name) == 0) {
        err = FSMakeFSSpecCompat(tmpDirSpec.vRefNum, tmpDirID, 
        	srcFileSpec.name, &tmpFileSpec);
        if (err == noErr) {
            err = FSpRenameCompat(&tmpFileSpec, dstFileSpec.name);
        }
    }
    if (err == noErr) {
        err = FSMakeFSSpecCompat(tmpDirSpec.vRefNum, tmpDirID,
        	dstFileSpec.name, &tmpFileSpec);
    }
    if (err == noErr) {
        err = FSpCatMoveCompat(&tmpFileSpec, &dstDirSpec);
    }
    if (err == noErr) {
        if (srcLocked) {
            FSpSetFLockCompat(&dstFileSpec);
        }
    }
    
    FSpDeleteCompat(&tmpDirSpec);
    
    if (saveErr != noErr) {
        err = saveErr;
    }
    
    done:
    if (err != noErr) {
        errno = TclMacOSErrorToPosixError(err);
        if (errorPtr != NULL) {
            Tcl_DStringAppend(errorPtr, dst, -1);
        }
        return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CopyErrHandler --
 *
 *      This procedure is called from the MoreFiles procedure 
 *      FSpDirectoryCopy whenever an error occurs.
 *
 * Results:
 *      False if the condition should not be considered an error, true
 *      otherwise.
 *
 * Side effects:
 *      Since FSpDirectoryCopy() is called only after removing any 
 *      existing target directories, there shouldn't be any errors.
 *      
 *----------------------------------------------------------------------
 */
static pascal Boolean 
CopyErrHandler(
    OSErr error,              /* Error that occured */
    short failedOperation,    /* operation that caused the error */
    short srcVRefNum,         /* volume ref number of source */
    long srcDirID,            /* directory id of source */
    StringPtr srcName,        /* name of source */
    short dstVRefNum,         /* volume ref number of dst */
    long dstDirID,            /* directory id of dst */
    StringPtr dstName         /* name of dst directory */
) {
    return true;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpRemoveDirectory --
 *
 *	Removes directory (and its contents, if the recursive flag is set).
 *
 * Results:
 *	If the directory was successfully removed, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR, errno is set to indicate
 *	the error, and the pathname of the file that caused the error
 *	is stored in errorPtr.  Some possible values for errno are:
 *
 *	EACCES:     path directory can't be read and/or written.
 *	EEXIST:	    path is a non-empty directory.
 *	EINVAL:	    path is a root directory.
 *	ENOENT:	    path doesn't exist or is "".
 * 	ENOTDIR:    path is not a directory.
 *
 * Side effects:
 *	Directory removed.  If an error occurs, the error will be returned
 *	immediately, and remaining files will not be deleted.
 *
 *---------------------------------------------------------------------------
 */
 
int
TclpRemoveDirectory(
    char *path,			/* Pathname of directory to be removed. */
    int recursive,		/* If non-zero, removes directories that
				 * are nonempty.  Otherwise, will only remove
				 * empty directories. */
    Tcl_DString *errorPtr)	/* If non-NULL, initialized DString for
				 * error reporting. */
{				 
    OSErr err;
    FSSpec fileSpec;
    long dirID;
    int locked;
    Boolean isDirectory;
    CInfoPBRec pb;
    Str255 fileName;

    locked = 0;
    err = FSpLocationFromPath(strlen(path), path, &fileSpec);
    if (err != noErr) {
        goto done;
    }   

    /*
     * Since FSpDeleteCompat will delete a file, make sure this isn't
     * a file first.
     */
         
    isDirectory = 1;
    FSpGetDirectoryID(&fileSpec, &dirID, &isDirectory);
    if (isDirectory == 0) {
        errno = ENOTDIR;
        return TCL_ERROR;
    }
    
    err = FSpDeleteCompat(&fileSpec);
    if (err == fLckdErr) {
        locked = 1;
    	FSpRstFLockCompat(&fileSpec);
    	err = FSpDeleteCompat(&fileSpec);
    }
    if (err == noErr) {
	return TCL_OK;
    }
    if (err != fBsyErr) {
        goto done;
    }
     
    if (recursive == 0) {
	/*
	 * fBsyErr means one of three things: file busy, directory not empty, 
	 * or working directory control block open.  Determine if directory
	 * is empty. If directory is not empty, return EEXIST.
	 */

	pb.hFileInfo.ioVRefNum = fileSpec.vRefNum;
	pb.hFileInfo.ioDirID = dirID;
	pb.hFileInfo.ioNamePtr = (StringPtr) fileName;
	pb.hFileInfo.ioFDirIndex = 1;
	if (PBGetCatInfoSync(&pb) == noErr) {
	    err = dupFNErr;	/* EEXIST */
	    goto done;
	}
    }
	
    /*
     * DeleteDirectory removes a directory and all its contents, including
     * any locked files.  There is no interface to get the name of the 
     * file that caused the error, if an error occurs deleting this tree,
     * unless we rewrite DeleteDirectory ourselves.
     */
	 
    err = DeleteDirectory(fileSpec.vRefNum, dirID, NULL);

    done:
    if (err != noErr) {
	if (errorPtr != NULL) {
	    Tcl_DStringAppend(errorPtr, path, -1);
	}
        if (locked) {
            FSpSetFLockCompat(&fileSpec);
        }
    	errno = TclMacOSErrorToPosixError(err);
    	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------------------
 *
 * MoveRename --
 *
 *	Helper function for TclpRenameFile.  Renames a file or directory
 *	into the same directory or another directory.  The target name
 * 	must not already exist in the destination directory.
 *
 *	Don't use FSpMoveRenameCompat because it doesn't work with
 *	directories or with locked files. 
 *
 * Results:
 *	Returns a mac error indicating the cause of the failure.
 *
 * Side effects:
 *	Creates a temp file in the target directory to handle a rename
 *	between directories.
 *
 *--------------------------------------------------------------------------
 */
  
static OSErr		
MoveRename(
    const FSSpec *srcFileSpecPtr,   /* Source object. */
    const FSSpec *dstDirSpecPtr,    /* Destination directory. */
    StringPtr copyName)		    /* New name for object in destination 
    				     * directory. */
{
    OSErr err;
    long srcID, dstID;
    Boolean srcIsDir, dstIsDir;
    Str31 tmpName;
    FSSpec dstFileSpec, srcDirSpec, tmpSrcFileSpec, tmpDstFileSpec;
    Boolean locked;
    
    if (srcFileSpecPtr->parID == 1) {
        /*
         * Trying to rename a volume.
         */
          
        return badMovErr;
    }
    if (srcFileSpecPtr->vRefNum != dstDirSpecPtr->vRefNum) {
	/*
	 * Renaming across volumes.
	 */
	 
        return diffVolErr;
    }
    err = FSpGetFLockCompat(srcFileSpecPtr, &locked);
    if (locked) {
        FSpRstFLockCompat(srcFileSpecPtr);
    }
    if (err == noErr) {
	err = FSpGetDirectoryID(dstDirSpecPtr, &dstID, &dstIsDir);
    }
    if (err == noErr) {
        if (srcFileSpecPtr->parID == dstID) {
            /*
             * Renaming object within directory. 
             */
            
            err = FSpRenameCompat(srcFileSpecPtr, copyName);
            goto done; 
        }
        if (Pstrequal(srcFileSpecPtr->name, copyName)) {
	    /*
	     * Moving object to another directory (under same name). 
	     */
	 
	    err = FSpCatMoveCompat(srcFileSpecPtr, dstDirSpecPtr);
	    goto done; 
        } 
        err = FSpGetDirectoryID(srcFileSpecPtr, &srcID, &srcIsDir);
    } 
    if (err == noErr) {
        /*
         * Fullblown: rename source object to temp name, move temp to
         * dest directory, and rename temp to target.
         */
          
        err = GenerateUniqueName(srcFileSpecPtr->vRefNum, 
       		srcFileSpecPtr->parID, dstID, tmpName);
        FSMakeFSSpecCompat(srcFileSpecPtr->vRefNum, srcFileSpecPtr->parID,
         	tmpName, &tmpSrcFileSpec);
        FSMakeFSSpecCompat(dstDirSpecPtr->vRefNum, dstID, tmpName,
         	&tmpDstFileSpec);
    }
    if (err == noErr) {
        err = FSpRenameCompat(srcFileSpecPtr, tmpName);
    }
    if (err == noErr) {
        err = FSpCatMoveCompat(&tmpSrcFileSpec, dstDirSpecPtr);
        if (err == noErr) {
            err = FSpRenameCompat(&tmpDstFileSpec, copyName);
            if (err == noErr) {
                goto done;
            }
            FSMakeFSSpecCompat(srcFileSpecPtr->vRefNum, srcFileSpecPtr->parID,
             	    NULL, &srcDirSpec);
            FSpCatMoveCompat(&tmpDstFileSpec, &srcDirSpec);
        }                 
        FSpRenameCompat(&tmpSrcFileSpec, srcFileSpecPtr->name);
    }
    
    done:
    if (locked != false) {
    	if (err == noErr) {
	    FSMakeFSSpecCompat(dstDirSpecPtr->vRefNum, 
	    	    dstID, copyName, &dstFileSpec);
            FSpSetFLockCompat(&dstFileSpec);
        } else {
            FSpSetFLockCompat(srcFileSpecPtr);
        }
    }
    return err;
}     
			    
/*
 *---------------------------------------------------------------------------
 *
 * GetFileSpecs --
 *
 * 	Generate a filename that is not in either of the two specified
 *	directories (on the same volume). 
 *
 * Results:
 *	Standard macintosh error.  On success, uniqueName is filled with 
 *	the name of the temporary file.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */ 
 
static OSErr
GenerateUniqueName(
    short vRefNum,		/* Volume on which the following directories
    				 * are located. */		
    long dirID1,		/* ID of first directory. */
    long dirID2,		/* ID of second directory.  May be the same
    				 * as the first. */
    Str31 uniqueName)		/* Filled with filename for a file that is
    				 * not located in either of the above two
    				 * directories. */
{
    OSErr err;
    long i;
    CInfoPBRec pb;
    static unsigned char hexStr[16] = "0123456789ABCDEF";
    static long startSeed = 248923489;
    
    pb.hFileInfo.ioVRefNum = vRefNum;
    pb.hFileInfo.ioFDirIndex = 0;
    pb.hFileInfo.ioNamePtr = uniqueName;

    while (1) {
        startSeed++;		
	pb.hFileInfo.ioNamePtr[0] = 8;
	for (i = 1; i <= 8; i++) {
	    pb.hFileInfo.ioNamePtr[i] = hexStr[((startSeed >> ((8-i)*4)) & 0xf)];
	}
	pb.hFileInfo.ioDirID = dirID1;
	err = PBGetCatInfoSync(&pb);
	if (err == fnfErr) {
	    if (dirID1 != dirID2) {
		pb.hFileInfo.ioDirID = dirID2;
		err = PBGetCatInfoSync(&pb);
	    }
	    if (err == fnfErr) {
	        return noErr;
	    }
	}
	if (err == noErr) {
	    continue;
	} 
	return err;
    }
} 

/*
 *---------------------------------------------------------------------------
 *
 * GetFileSpecs --
 *
 *	Gets FSSpecs for the specified path and its parent directory.
 *
 * Results:
 *	The return value is noErr if there was no error getting FSSpecs,
 *	otherwise it is an error describing the problem.  Fills buffers 
 *	with information, as above.  
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */

static OSErr
GetFileSpecs(
    char *path,			/* The path to query. */
    FSSpec *pathSpecPtr,	/* Filled with information about path. */
    FSSpec *dirSpecPtr,		/* Filled with information about path's
    				 * parent directory. */
    Boolean *pathExistsPtr,	/* Set to true if path actually exists, 
    				 * false if it doesn't or there was an 
    				 * error reading the specified path. */
    Boolean *pathIsDirectoryPtr)/* Set to true if path is itself a directory,
    				 * otherwise false. */
{
    char *dirName;
    OSErr err;
    int argc;
    char **argv;
    long d;
    Tcl_DString buffer;
        
    *pathExistsPtr = false;
    *pathIsDirectoryPtr = false;
    
    Tcl_DStringInit(&buffer);
    Tcl_SplitPath(path, &argc, &argv);
    if (argc == 1) {
        dirName = ":";
    } else {
        dirName = Tcl_JoinPath(argc - 1, argv, &buffer);
    }
    err = FSpLocationFromPath(strlen(dirName), dirName, dirSpecPtr);
    Tcl_DStringFree(&buffer);
    ckfree((char *) argv);

    if (err == noErr) {
        err = FSpLocationFromPath(strlen(path), path, pathSpecPtr);
        if (err == noErr) {
            *pathExistsPtr = true;
            err = FSpGetDirectoryID(pathSpecPtr, &d, pathIsDirectoryPtr);
        } else if (err == fnfErr) {
            err = noErr;
        }
    }
    return err;
}

/*
 *-------------------------------------------------------------------------
 *
 * FSpGetFLockCompat --
 *
 *	Determines if there exists a software lock on the specified
 *	file.  The software lock could prevent the file from being 
 *	renamed or moved.
 *
 * Results:
 *	Standard macintosh error code.  
 *
 * Side effects:
 *	None.
 *
 *
 *-------------------------------------------------------------------------
 */
 
OSErr
FSpGetFLockCompat(
    const FSSpec *specPtr,	/* File to query. */
    Boolean *lockedPtr)		/* Set to true if file is locked, false
    				 * if it isn't or there was an error reading
    				 * specified file. */
{
    CInfoPBRec pb;
    OSErr err;
    
    pb.hFileInfo.ioVRefNum = specPtr->vRefNum;
    pb.hFileInfo.ioDirID = specPtr->parID;
    pb.hFileInfo.ioNamePtr = (StringPtr) specPtr->name;
    pb.hFileInfo.ioFDirIndex = 0;
    
    err = PBGetCatInfoSync(&pb);
    if ((err == noErr) && (pb.hFileInfo.ioFlAttrib & 0x01)) {
        *lockedPtr = true;
    } else {
        *lockedPtr = false;
    }
    return err;
}
    
/*
 *----------------------------------------------------------------------
 *
 * Pstrequal --
 *
 *      Pascal string compare. 
 *
 * Results:
 *      Returns 1 if strings equal, 0 otherwise.
 *
 * Side effects:
 *      None.
 *      
 *----------------------------------------------------------------------
 */
static int 
Pstrequal (
    ConstStr255Param stringA,	/* Pascal string A */
    ConstStr255Param stringB)    /* Pascal string B */
{
    int i, len;
    
    len = *stringA;
    for (i = 0; i <= len; i++) {
        if (*stringA++ != *stringB++) {
            return 0;
        }
    }
    return 1;
}


