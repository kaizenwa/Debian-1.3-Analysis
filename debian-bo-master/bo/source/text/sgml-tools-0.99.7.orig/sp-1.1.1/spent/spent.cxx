// Copyright (c) 1995 James Clark
// See the file COPYING for copying permission.

#include "config.h"

#include "EntityApp.h"
#include "InputSource.h"
#include "OutputCharStream.h"

#ifdef SP_NAMESPACE
using namespace SP_NAMESPACE;
#endif

class SpentApp : public EntityApp {
public:
  SpentApp();
  void processOption(AppChar opt, const AppChar *arg);
  int processSysid(const StringC &);
private:
  Boolean rawOutput_;
};

SP_DEFINE_APP(SpentApp)

SpentApp::SpentApp()
: rawOutput_(0)
{
  registerOption('r');
}

void SpentApp::processOption(AppChar opt, const AppChar *arg)
{
  switch (opt) {
  case 'r':
    rawOutput_ = 1;
    break;
  default:
    EntityApp::processOption(opt, arg);
    break;
  }
}

int SpentApp::processSysid(const StringC &sysid)
{
  InputSource *in = entityManager()->open(sysid,
					  systemCharset_,
					  new InputSourceOrigin,
					  0,
					  *this);
  if (!in)
    return 1;
  OutputCharStream *os = makeStdOut();
  if (!rawOutput_)
    os = new RecordOutputCharStream(os);
  for (;;) {
    Xchar c = in->get(*this);
    if (c == InputSource::eE)
      break;
    in->extendToBufferEnd();
    os->write(in->currentTokenStart(), in->currentTokenLength());
  }
  delete os;
  return 0;
}
