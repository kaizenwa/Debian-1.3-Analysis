//	vedcnv.h:	Header for vedCanvasPane class
//=======================================================================

#ifndef vedCNV_H
#define vedCNV_H

#include <v/vtexted.h>

#include <fstream.h>

    class vedCmdWindow;

    class vedTextEditor : public vTextEditor
      {
      public:		//---------------------------------------- public
	vedTextEditor(vedCmdWindow* parent);
	virtual ~vedTextEditor();

	int ReadFile(char* name);
	int SaveFile(char* name);

	virtual void ChangeLoc(long line, int col);
	virtual void ChangeInsMode(int IsInsMode);
	virtual void StatusMessage(char *msg);
	virtual void ErrorMsg(char *str);

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private
      };
#endif
