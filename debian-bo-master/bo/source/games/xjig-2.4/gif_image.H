#ifndef __gif_image_h
#define __gif_image_h

typedef unsigned char	byte;
typedef int					boolean;

#define	OPTION_EXTENSION		(0x10)
#define	REMOVETILE_EXTENSION	(0x11)
#define	FLATTILE_EXTENSION	(0x12)
#define	SUBSIZE_EXTENSION		(0x13)
#define	SCALEINFO_START		(0x20)
#define	COMMENT_EXTENSION		(0xFE)

class Extension {
	public:
		Extension(unsigned char code_in,class Extension *n)
			{ code=code_in; data=new char[1]; len=0; next=n; }
		~Extension()		{
				if (next)	delete next;
				if (data)	delete data;
		}
		void AddData( char *ndata, int nlen );

	private:
		unsigned char		code;
		char					*data;
		int					len;
		class Extension	*next;

	friend class GifImage;
};

class GifImage {
	public:
		GifImage( const char *filename, int autocrop=1 );
		GifImage();
		~GifImage();

		const char *Name()	{ return name; }
		int Width()				{ return width; }
		int Height()			{ return height; }
		const byte* Data()	{ return data; }

		const char *GetExtensionData( unsigned char code );
		void GetSize( int *h, int *w );
		long GetAverageColor();

		int CropImage(int x1,int y1, int x2, int y2);
		int CropImage();

		void AddLock(class GifXImage *locker);
		void RemoveLock(class GifXImage *locker);

		void Recolor( class GifImage *gif_p );
		void AddAt( int x, int y, class GifImage *gif2 );

		void Rotate90();

	protected:
		int LoadGIF(const char *fname);
		int ReadImageData(FILE *fp);			// read raw raster data into buffer
		int DecodeImage();						// decode data into image
		static int ReadCode();
		void AddToPixel(byte Index);
#if (0)
		int ColorDicking();
#endif
		void ReadColormap(FILE *fp);
		void CloseGif();

	protected:
		char	*name;
		int	width, height;								// image dimensions
		byte	*data;										// image data
		byte	Red[256], Green[256], Blue[256];		// Colormap-Data
		boolean	HasColormap;
		int		ColorMapSize;						/* number of colors */
		int		Background;							/* background color */

		int		lockcount;							/* number of lockers */

		Extension	*first;

	public:
		int	GetNCols()	{ return ColorMapSize; }
		int	GetColor(int id, unsigned short *red, unsigned short *green, unsigned short *blue);

friend class GifXImage;
};

#endif
