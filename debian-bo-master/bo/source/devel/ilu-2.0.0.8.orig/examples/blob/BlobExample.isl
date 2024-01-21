INTERFACE BlobExample;

TYPE coord = SHORT INTEGER;

TYPE blobid = OBJECT
	METHODS
		ReqMove (x : coord, y : coord),
		ReqGrab (bv : blobvis) : BOOLEAN,
		ReqRelease (bv : blobvis)
	END;

TYPE blobvis = OBJECT
	METHODS
		Move(x : coord, y : coord),
		Grab (),
		Release ()
	END;

TYPE blobcli = OBJECT
	METHODS
		NewBlobvis (x : coord,
			    y : coord,
			    n : blobid) : blobvis,
		RemoveBlobvis(n : blobid)
	END;

TYPE blobserv = OBJECT
	METHODS
		RegisterClient(c : blobcli),
		RemoveClient(c : blobcli),
		NewBlobId(x : coord, y : coord),
		RemoveBlobId(n : blobid)
	END;
