/*
   FITS.I
   Routines to manipulate FITS files (IAU astronomical data format).
   Anonymous FTP site:    nssdc.gsfc.nasa.gov:/pub/fits

   $Id$
 */
/*    Copyright (c) 1996.  The Regents of the University of California.
                    All rights reserved.  */

/*----------------------------------------------------------------------
 * @(#) fits.i: manipulation of FITS files for Yorick by Eric THIEBAUT.
 */

require, "string.i";

/*----------------------------------------------------------------------*/

local fits_max_naxis;
/* DOCUMENT fits_max_naxis -- maximum number of allowed axis in FITS
	Its value should *NOT* be modified during a Yorick session.
*/
fits_max_naxis= 9;	/* if you need more axis, change this value in this
			 * file and source it again. */

struct FitsHeader {
  int		bitpix;		//   8	pixel values are unsigned bytes
				//  16	pixel values are signed 2-byte integers
				//  32	pixel values are signed 4-byte integers
				// -32	pixel values are 4-byte floating points
				// -64	pixel values are 8-byte floating points
				// -128	pixel values are 2x8-byte complex
  int		naxis;		// number of axis
  int		axis(fits_max_naxis);	// number of pixel along axis No. n
  // Pixel intensity / brighness:
  double	bscale;		// pixelValue = BZERO+BSCALE*fileValue
  double	bzero;		// pixelValue = BZERO+BSCALE*fileValue
  string	bunit;		// brightness unit
  double	datamax;	// maximum data value in the file
  double	datamin;	// minimum data value in the file
  // Miscellaneous information:
  string	object;		// image name
  string	date;		// date of file creation (dd/mm/yy)
  string	date_obs;	// date of data acquisition (dd/mm/yy)
  string	origin;		// institution
  string	instrume;	// data acquisition instrument
  string	telescop;	// data acquisition telescope
  string	observer;	// observer name/identification
  string	history;	// newline separated history lines
  string	comment;	// newline separated comment lines
  // coordinate system for each axis:
  double	epoch;		// epoch of coordinate system (year)
  double	crval(fits_max_naxis);	// coord = CRVAL+(pixel-CRPIX)*CDELT
  double	crpix(fits_max_naxis);	// coord = CRVAL+(pixel-CRPIX)*CDELT
  double	cdelt(fits_max_naxis);	// coord = CRVAL+(pixel-CRPIX)*CDELT
  string	ctype(fits_max_naxis);	// type of physical coordinate
  double	crota(fits_max_naxis);	// rotation angle of axis No. #
}

/*----------------------------------------------------------------------*/

func fitsHeader(&header)
/* DOCUMENT header= fitsHeader();
            fitsHeader, header;
     creates a structure FitsHeader with defaults.  Equivalent to:
         header= FitsHeader(bscale= 1., bzero= 0.);

   SEE ALSO: fitsRead, fitsWrite.
*/
{
  return (header= FitsHeader(bscale= 1., bzero= 0.));
}

func fitsFixHeader(&header)
/* DOCUMENT fitsFixHeader, header
	check consistency of FITS header.
   SEE ALSO: fitsRead, fitsWrite.
*/
{
  if (structof(header) != FitsHeader)
    error, "bad type for HEADER (not FitsHeader)";
  if (header.datamin > header.datamax)
    error, "DATAMIN greater than DATAMAX in FITS header!";
  if (header.bscale == 0. && header.bzero != 0.) {
    write, "WARNING: BZERO ignored because BSCALE is zero";
    header.bzero= 0.;
  }
}

func fitsAddComment(&header, str)
/* DOCUMENT fitsAddComment, header, string
	Add STRING as a comment in HEADER (a FitsHeader structure). If
	STRING is too long, it is broken in pieces.
   SEE ALSO: fitsRead, fitsWrite.
*/
{
  //if (structof(header) != FitsHeader)
  //	error, "bad type for HEADER (not FitsHeader)";
  str= strtrim(str);	// remove trailing/leading blanks
  while (strlen(str)) {
    header.comment+= strpart(str, :69)+"\n";
    str= strtrim(strpart(str, 70:), 1);
  }
}

func fitsAddHistory(&header, str, stamp=)
/* DOCUMENT fitsAddHistory, header, string
	Add STRING as an history card in HEADER (a FitsHeader structure).
	If STRING is too long, it is broken in pieces.
	Keyword STAMP may be used with a zero value to avoid the
	additional time stamp (default is to add a time stamp).
   SEE ALSO: fitsRead, fitsWrite.
*/
{
  //if (structof(header) != FitsHeader)
  //	error, "bad type for HEADER (not FitsHeader)";
  str= strtrim(str);	// remove trailing/leading blanks
  if (is_void(stamp))
    stamp= 1N;	// default is to add a time stamp
  if (stamp) {
    str= getdate()+" "+gettime()+": "+str;
    indent= "                   ";
  } else {
    indent= "";
  }
  len= 69 - strlen(indent);
  header.history+= strpart(str, :69)+"\n";
  str= strtrim(strpart(str, 70:), 1);
  while (strlen(str)) {
    header.history+= indent + strpart(str, :len) + "\n";
    str= strtrim(strpart(str, len+1:), 1);
  }
}

/*----------------------------------------------------------------------*/

func fitsRescale(data, bitpix, &bscale, &bzero, data_min=, data_max=)
/* DOCUMENT rescaled_data= fitsRescale(data, bitpix, bscale, bzero)

	Linearly rescale the values of input array DATA to fit into
	integers with BITPIX bits per value (i.e., `char', `short' or
	`long' for BITPIX being 8, 16 and 32 respectively).

	Arguments BSCALE and BZERO are optional and purely outputs passed
	by address.  Their value will be set so that:
		DATA(i) = BZERO + BSCALE * RESCALED_DATA(i)
	where the equality may not be exact due to rounding errors.  The
	difference is however the smallest possible, i.e., less than
	BSCALE / 2.

	Keywords DATA_MIN and DATA_MAX may be used to supply the maximum
	and minimum data values or to set brightness cutoffs.

   SEE ALSO: fitsRead, fitsWrite.
*/
{
  if (bitpix == 8) {
    // assume ``char'' is 8 bits unsigned
    file_type= char;
    file_unsigned= 1N;
  } else if (bitpix == 16) {
    // assume ``short'' is 16 bits signed
    file_type= short;
    file_unsigned= 0N;
  } else if (bitpix == 32) {
    // assume ``long'' is 32 bits signed
    file_type= long;
    file_unsigned= 0N;
  } else {
    error, "bad BITPIX (should be 8, 16 or 32)";
  }

  data_min= double((is_void(data_min)? min(data) : data_min));
  data_max= double((is_void(data_max)? max(data) : data_max));
  if (data_max < data_min) error, "bad DATA_MAX and DATA_MIN";

  if (file_unsigned) {
    file_min= 0.;
    file_max= 2.^bitpix - 1.;
  } else {
    file_min= -2.^(bitpix - 1);
    file_max=  2.^(bitpix - 1) - 1.;
  }

  if (data_max == data_min) {
    bzero= double(data_min);
    return array(file_type(0), dimsof(data));
  }

  bscale= (data_max - data_min) / (file_max - file_min);
  bzero= data_min - bscale * file_min;
  return file_type(min(file_max, max(file_min, (data - bzero) / bscale)));
}

/*----------------------------------------------------------------------*/

func fitsWrite(name, data, header, rescale=, pack=)
/* DOCUMENT fitsWrite, filename, data, header
	Write DATA in file FILENAME using FITS format.  If present, the
	information of the optional argument HEADER  (a FitsHeader structure)
	will be used to write the FITS file header.

	Keyword PACK, if non-nil and non-zero, indicates that axis whith
	unit dimension should be ignored.  The default is to ignore only
	zero length axis.

	Keyword RESCALE, if non-nil and zero, indicates that input data
	values should not be rescaled into integers according to FITS
	standard.  The default, is to recode doubles as 32 bit integers,
	floats as 16 bit integers and integers into smallest integers
	possible without loss of dynamic range (e.g., an array of long
	integers ranging from -31 to +130 will be recoded as chars).

  SEE ALSO: fitsRead, fitsRescale, fitsAddHistory, fitsAddComment.
*/
{
  local bitpix, bzero, bscale;

  if (is_void(data)) error, "empty DATA array!!!";
  if (dimsof(data)(1) > fits_max_naxis) {
    write, swrite(format="*** Currently, number of dimensions for "+
		  "FITS cannot exceed %d\n*** If you really need "+
		  "more dimensions, you'll have to modify the\n"+
		  "*** source file...", int(fits_max_naxis));
    error, "too many dimensions";
  }

  if (is_void(header)) {
    fitsHeader, header;	// create a FITS header with defaults
  } else {
    fitsFixHeader, header;	// just check header
  }
  if (strlen(header.date) == 0) {
    header.date= getdate()+" "+gettime();
  }

  // rescale brightness to fit standards
  if (is_void(rescale) || rescale) {
    if (header.bscale != 1. || header.bzero != 0.)
      write, "WARNING: initial [BSCALE,BZERO] not equal to [1,0]";

    if (structof(data) == float || structof(data) == double) {
      if (structof(data) == float) {
	bitpix= 16;	// float  -> short
      } else {
	bitpix= 32;	// double -> long
      }
      data= fitsRescale(data, bitpix, bscale, bzero);
      header.bitpix= bitpix;
      header.bzero+= header.bscale * bzero;
      header.bscale*= bscale;
    } else if (structof(data) == short ||
	       structof(data) == int ||
	       structof(data) == long) {
      // Integers: check if data fit into smaller data type
      if (structof(data) == short) bitpix= 16;
      else if (structof(data) == int) bitpix= 32;
      else bitpix= 32;
      data_max= double(max(data));
      data_min= double(min(data));
      data_range= 1.+data_max-data_min;
      if (data_range<=256.) {
	file_type= char;
	bzero= double(data_min);
	bitpix= 8;
      } else if (bitpix==32 && data_range<=65536.) {
	file_type= short;
	bzero= data_min + 32768.;
	bitpix= 16;
      } else {
	file_type= bitpix==32? long : short;
	bzero= data_min + 2.^(bitpix - 1);
      }
      data= file_type(data - bzero);
      header.bitpix= bitpix;
      header.bzero+= header.bscale * bzero;
    } else if (structof(data) == char) {
      header.bitpix= 8;
    } else {
      error, "data type not supported";
    }

  } else {
    if (header.bscale != 1. || header.bzero != 0.)
      write, "WARNING: no rescaling, but non-unit BSCALE or non-zero BZERO";
    if (structof(data) == char) {
      header.bitpix= 8;
    } else if (structof(data) == short) {
      header.bitpix= 16;
    } else if (structof(data) == int) {
      data= long(data);  // for consistency across all platforms
      header.bitpix= 32;
    } else if (structof(data) == long) {
      header.bitpix= 32;
    } else if (structof(data) == float) {
      header.bitpix= -32;
    } else if (structof(data) == double) {
      header.bitpix= -64;
    } else if (structof(data) == complex) {
      write, "WARNING: using BITPIX=-128 is not standard FITS";
      header.bitpix= -128;
    } else {
      error, "bad data type for DATA";
    }
  }

  // compute dimensions list
  header.axis(*)= 0;
  dims= dimsof(data);
  if (dims(1) == 0) {
    // data is a scalar!
    header.naxis= 1;
    header.axis(1)= 1;
  } else {
    // there is at least one dimension
    dims= dims(2:);
    if (is_void(pack) || pack) {
      i= where(dims > 1);
      if (numberof(i)) {
	dims= dims(i);
      } else {
	dims= [1];
      }
    }
    header.naxis= numberof(dims);
    header.axis(1:header.naxis)= dims;
  }

  // write data file
  local address;
  if (open(name+"L", "", 1)) {
    write, format="%s", "Remove existing file "+name+"L (y or n)? ";
    response= strtok(rdline(prompt=string(0)))(1);
    if (response=="y" || response=="yes") {
      remove, name+"L";
    } else {
      write, "No file written or deleted.";
      return;
    }
  }
  f= open(name, "wb");
  sun_primitives, f;
  address= 0L;
  _fitsPutLogical, f, address, "SIMPLE", 1;
  _fitsPutInteger, f, address, "BITPIX", header.bitpix;
  _fitsPutInteger, f, address, "NAXIS", header.naxis;
  for (i=1; i<=header.naxis; i++) {
    _fitsPutInteger, f, address, swrite(format="NAXIS%d", i), header.axis(i);
  }
  if (header.bscale != 1.0 || header.bzero != 0.) {
    _fitsPutReal, f, address, "BSCALE", header.bscale;
    _fitsPutReal, f, address, "BZERO", header.bzero;
  }
  if (strlen(header.bunit))
    _fitsPutString, f, address, "BUNIT", header.bunit;
  if (header.datamax || header.datamin) {
    _fitsPutReal, f, address, "DATAMAX", header.datamax;
    _fitsPutReal, f, address, "DATAMIN", header.datamin;
  }
  if (strlen(header.object))
    _fitsPutString, f, address, "OBJECT", header.object;
  if (strlen(header.date))
    _fitsPutString, f, address, "DATE", header.date;
  if (strlen(header.date_obs))
    _fitsPutString, f, address, "DATE-OBS", header.date_obs;
  if (strlen(header.origin))
    _fitsPutString, f, address, "ORIGIN", header.origin;
  if (strlen(header.instrume))
    _fitsPutString, f, address, "INSTRUME", header.instrume;
  if (strlen(header.telescop))
    _fitsPutString, f, address, "TELESCOP", header.telescop;
  if (strlen(header.observer))
    _fitsPutString, f, address, "OBSERVER", header.observer;
  if (header.epoch)
    _fitsPutReal, f, address, "EPOCH", header.epoch;
  for (i=1; i<=header.naxis; i++) {
    if (strlen(header.ctype(i)))
      _fitsPutString, f, address, swrite(format="CTYPE%d", i), header.ctype(i);
    if (header.cdelt(i)) {
      _fitsPutReal, f, address, swrite(format="CRVAL%d", i), header.crval(i);
      _fitsPutReal, f, address, swrite(format="CRPIX%d", i), header.crpix(i);
      _fitsPutReal, f, address, swrite(format="CDELT%d", i), header.cdelt(i);
      _fitsPutReal, f, address, swrite(format="CROTA%d", i), header.crota(i);
    }
  }
  if (strlen(header.comment)) {
    comment= strtok(header.comment, "\n");
    while (comment(1)) {
      _fitsPutComment, f, address, "COMMENT", comment(1);
      comment= strtok(comment(2), "\n");
    }
  }
  if (strlen(header.history)) {
    history= strtok(header.history, "\n");
    while (history(1)) {
      _fitsPutComment, f, address, "HISTORY", history(1);
      history= strtok(history(2), "\n");
    }
  }
  _fitsPutEnd,f, address;
  _write, f, address, data;
  close, f;

  // Yorick uses a Contents Log file, which should be removed...
  if (open(name+"L", "", 1))
    remove, name+"L";
}

/*----------------------------------------------------------------------*/

func fitsRead(name, &header, which=, pack=, rescale=)
/* DOCUMENT a= fitsRead(filename, header)
	returns the data of the FITS file FILENAME.  If present, the
	optional argument HEADER will be used to store the contents of
	the FITS header file (a FitsHeader structure).

	Keyword WHICH may be used to indicate which sub-array should be
	returned.  For instance, if the array DATA with dimensions
	(235,453,7) is stored in the FITS file "data.fits", the sub-array
	DATA(,,4) can be read by:
		SUB_DATA= fitsRead("data.fits", which= 4);

	Keyword PACK, if non-nil and non-zero, indicates that axis whith
	unit dimension should be ignored.  The default is to ignore only
	zero length axis.

	Keyword RESCALE, if non-nil and zero, indicates that read data
	values should not be rescaled according to FITS keywords
	BSCALE and BZERO.  The default is to rescale data values if
	BSCALE is not 1. or BZERO is not 0.

  SEE ALSO: fitsWrite, fitsRescale.
*/
{
  local axis;

  if (is_void(pack))
    pack= 1;	// default is to ignore zero *AND* unit dimensions
  if (is_void(rescale))
    rescale= 1;	// default is to rescale brightness

  fitsHeader, header;		// Create FITS header with defaults
  file= open(name, "rb");
  sun_primitives, file;
  buffer= array(char, 80, 36);
  buffer_size= 80*36;
  end_not_found= 1;
  n= 1;
  address= 0;
  do {
    if (_read(file, address, buffer) != buffer_size)
      error, "cannot read header";
    address+= buffer_size;
    for (i=1; i<=dimsof(buffer)(3) && end_not_found; i++, n++) {
      line= string(&buffer(,i));
      keyword= strtoupper(strtrim(strpart(line, 1:8), 2, blank=" "));
      value= strtrim(strpart(line, 9:80), 2);
      if (n==1) {
	if (keyword != "SIMPLE" || !_fitsGetLogical(keyword, value))
	  error, "not a standard FITS file";
	continue;
      }
      if (n==2) {
	if (keyword != "BITPIX" ||
	    noneof(((header.bitpix= _fitsGetInteger(keyword, value)) ==
		    [8, 16, 32, 64, -32, -64, -128])))
	  error, "bad BITPIX or no BITPIX at line 2";
	continue;
      }
      if (n==3) {
	if (keyword != "NAXIS" ||
	    (header.naxis= _fitsGetInteger(keyword, value)) < 1)
	  error, "bad NAXIS or no NAXIS at line 3";
	if (header.naxis > fits_max_naxis) {
	  write, swrite(format="*** Currently, number of dimensions for "+
			"FITS cannot exceed %d\n*** If you really need "+
			"more dimensions, you'll have to modify the\n"+
			"*** source file...", int(fits_max_naxis));
	  error, swrite(format="NAXIS=%d too large", header.naxis);
	}
	naxis= header.naxis;
	continue;
      }
      if (naxis) {
	k= n-3;
	s= swrite(format="NAXIS%d", k);
	if (keyword == s) {
	  if ((header.axis(k)= _fitsGetInteger(keyword, value)) < 0)
	    error, "bad "+s;
	  continue;
	}
	naxis= 0;	// no more NAXIS to read
      }
      if (keyword == "END") {
	end_not_found=0;
	break;
      }
      if (keyword == "") {
	continue;
      }
      if (keyword == "COMMENT") {
	value= strtrim(value);
	if (strlen(value))
	  header.comment+= value+"\n";
	continue;
      }
      if (keyword == "HISTORY") {
	value= strtrim(value);
	if (strlen(value))
	  header.history+= value+"\n";
	continue;
      }
      if (keyword == "BSCALE") {
	header.bscale= _fitsGetReal(keyword, value);
	continue;
      }
      if (keyword == "BZERO") {
	header.bzero= _fitsGetReal(keyword, value);
	continue;
      }
      if (keyword == "BUNIT") {
	header.bunit= _fitsGetString(keyword, value);
	continue;
      }
      if (keyword == "DATAMAX") {
	header.datamax= _fitsGetReal(keyword, value);
	continue;
      }
      if (keyword == "DATAMIN") {
	header.datamin= _fitsGetReal(keyword, value);
	continue;
      }
      if (keyword == "EPOCH") {
	header.epoch= _fitsGetReal(keyword, value);
	continue;
      }
      if (keyword == "OBJECT") {
	header.object= _fitsGetString(keyword, value);
	continue;
      }
      if (keyword == "DATE") {
	header.date= _fitsGetString(keyword, value);
	continue;
      }
      if (keyword == "DATE-OBS") {
	header.date_obs= _fitsGetString(keyword, value);
	continue;
      }
      if (keyword == "ORIGIN") {
	header.origin= _fitsGetString(keyword, value);
	continue;
      }
      if (keyword == "INSTRUME") {
	header.instrume= _fitsGetString(keyword, value);
	continue;
      }
      if (keyword == "TELESCOP") {
	header.telescop= _fitsGetString(keyword, value);
	continue;
      }
      if (keyword == "OBSERVER") {
	header.observer= _fitsGetString(keyword, value);
	continue;
      }
      if (strpart(keyword, 1:1) == "C") {
	axis=long();
	if (sread(keyword, format="CRVAL%d", axis)) {
	  if (axis < 1 || axis > header.naxis)
	    error, "bad "+keyword;
	  header.crval(axis)= _fitsGetReal(keyword, value);
	  continue;
	}
	if (sread(keyword, format="CRPIX%d", axis)) {
	  if (axis < 1 || axis > header.naxis)
	    error, "bad "+keyword;
	  header.crpix(axis)= _fitsGetReal(keyword, value);
	  continue;
	}
	if (sread(keyword, format="CDELT%d", axis)) {
	  if (axis < 1 || axis > header.naxis)
	    error, "bad "+keyword;
	  header.cdelt(axis)= _fitsGetReal(keyword, value);
	  continue;
	}
	if (sread(keyword, format="CROTA%d", axis)) {
	  if (axis < 1 || axis > header.naxis)
	    error, "bad "+keyword;
	  header.crota(axis)= _fitsGetReal(keyword, value);
	  continue;
	}
	if (sread(keyword, format="CTYPE%d", axis)) {
	  if (axis < 1 || axis > header.naxis)
	    error, "bad "+keyword;
	  header.ctype(axis)= _fitsGetString(keyword, value);
	  continue;
	}
      }
      if (keyword == "BLANK") {
	if (strpart(value, 1:1) == "=") {
	  header.blank= _fitsGetInteger(keyword, value);
	} else {
	  value= strtrim(value);
	  if (strlen(value))
	    write, "WARNING: discarding BLANK comment ``"+value+"''";
	}
	continue;
      }
      write, "WARNING: unknown keyword ``"+keyword+"''";
    }
  } while (end_not_found);

  /*
   * Get dimensions (ignore zero and unit dimensions, keep
   * unit dimensions if keyword `pack' is zero):
   */
  dims= array(long, 1+header.naxis);
  dims(1)=  2;	// just a dummy value!
  dims(2:)= header.axis(1:header.naxis);
  i= where(dims > (pack ? 1 : 0));
  dims(1)=  numberof(i) - 1;
  if (dims(1) <= 0) {
    write, "WARNING empty data file";
    close, file;
    return [];
  }
  dims= dims(i);

  if (is_void(which)) {
    which=0;
  } else {
    if (numberof(which) != 1 || which != long(which))
      error, "WHICH must be a scalar integer";
    last= dims(0);
    if (which <= 0)
      which+= last;
    if (which > last || which < 1)
      error, "WHICH out of range";
    dims= dims(:-1);
    dims(1)-= 1;
  }

  if (header.bitpix == 8) {
    data= array(char, dims);
    data_size= 1;
  } else if (header.bitpix == 16) {
    data= array(short, dims);
    data_size= 2;
  } else if (header.bitpix == 32) {
    data= array(long, dims);
    data_size= 4;
  } else if (header.bitpix == -32) {
    data= array(float, dims);
    data_size= 4;
  } else if (header.bitpix == -64) {
    data= array(double, dims);
    data_size= 8;
  } else if (header.bitpix == -128) {
    write, "WARNING: using BITPIX=-128 is not standard FITS";
    data= array(complex, dims);
    data_size= 16;
  } else {
    error, "congratulations: you have found a BUG!";
  }
  if (which > 1)
    address+= (which - 1) * numberof(data) * data_size;

  if (_read(file, address, data) != numberof(data))
    error, "cannot read data";
  close, file;

  fitsFixHeader, header;

  /*
   * Rescale pixel values
   */
  if (rescale && (header.bscale != 1. || header.bzero != 0.)) {
    if (abs(header.bitpix) < 32) {
      data=  float(header.bzero + header.bscale * data);
      header.bitpix= -32;	// data is now 4-byte float
    } else {
      data= header.bzero + header.bscale * data;
      header.bitpix= -64;	// data is now 8-byte double
    }
    header.bscale= 1.;
    header.bzero= 0.;
  }

  return data;
}

/*----------------------------------------------------------------------*/

func _fitsGetLogical(keyword, value)
{
  x= string();
  if (strpart(value, 1:1) == "=" &&
      sread(value, format="=%s", x) == 1 && strlen(x) == 1) {
    if (x == "T" || x == "t")
      return 1;
    if (x == "F" || x == "f")
      return 0;
  }
  error, "bad logical value for "+keyword;
}

func _fitsGetInteger(keyword, value)
{
  x= long();
  if (strpart(value, 1:1) == "=" && sread(value, format="=%d", x) == 1)
    return x;
  error, "bad integer value for "+keyword;
}

func _fitsGetReal(keyword, value)
{
  x= double();
  if (strpart(value, 1:1) == "=" && sread(value, format="=%f", x) == 1)
    return x;
  error, "bad real value for "+keyword;
}

func _fitsGetString(keyword, value)
{
  if (strpart(value, 1:1) == "=") {
    /* note that this does not allow for a / delimited comment field... */
    x= strtrim(strpart(value, 2:));
    if (strpart(x, 1:1) == "'") {
    // assume that the string is quoted
    if ((i= strchr(x, '\'', last=1)) > 1)
      return strtrim(strpart(x, 2:i-1));
    } else {
      return x;
    }
  }
  //error, "bad value for "+keyword;
  write, "WARNING: bad string value for "+keyword;
  return "";
}

/*----------------------------------------------------------------------*/

func _fitsPutValue(stream, &address, format, keyword, value, comment)
{
  if (is_void(address)) {
    address= 0L;
  } else if (structof(address) != long) {
    write, "ADDRESS=",address;
    error, "bad ADDRESS";
  }
  comment= is_void(comment)? "" : strpart(comment, 1:46);
  line= swrite(format=format, keyword, value, comment);
  if (strlen(line) != 80) {
    write, "BAD LINE: "+line;
    error, "bug...";
  }
  line= *pointer(line);
  _write, stream, address, line(1:80);
  address+= 80;
}

func _fitsPutInteger(stream, &address, keyword, value, comment)
{
  _fitsPutValue, stream, address, "%-8s= %20ld / %-46s\n", keyword,
    long(value), comment;
}

func _fitsPutReal(stream, &address, keyword, value, comment)
{
  _fitsPutValue, stream, address, "%-8s= %#20.13G / %-46s\n", keyword,
    double(value), comment;
}

func _fitsPutString(stream, &address, keyword, value, comment)
{
  _fitsPutValue, stream, address, "%-8s= '%-18s' / %-46s\n", keyword,
    strpart(value, 1:18), comment;
}

func _fitsPutLogical(stream, &address, keyword, value, comment)
{
  _fitsPutValue, stream, address, "%-8s= %20s / %-46s\n", keyword,
    (value? "T" : "F"), comment;
}

func _fitsPutComment(stream, &address, keyword, comment)
{
  comment= is_void(comment) ? "" : strpart(comment, 1:70);
  _write, stream, address,
    (*pointer(swrite(format="%-8s %-70s\n", keyword, comment)))(1:80);
  address+= 80;
}

func _fitsPutEnd(stream, &address)
{
  _fitsPutComment, stream, address, "END";
  empty= array(' ', 80);
  empty(0)= '\n';
  while ((address / 80) % 36) {
    _write, stream, address, empty;
    address+= 80;
  }
}

/*----------------------------------------------------------------------*/
