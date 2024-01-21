// Copyright (c) 1996 James Clark
// See the file COPYING for copying permission.

#include "config.h"
#include "ParserApp.h"
#include "GenericEventHandler.h"
#include "SGMLGenerator.h"

#ifdef SP_NAMESPACE
using namespace SP_NAMESPACE;
#endif

class SGMLGeneratorEH : public SGMLGenerator, public GenericEventHandler {
public:
  SGMLGeneratorEH(OutputCharStream *, unsigned genFlags, Messenger *);
  void reportMessage(const Message &msg, StringC &);
private:
  Messenger *mgr_;
};

class SgmlnormApp : public ParserApp {
public:
  SgmlnormApp();
  void processOption(AppChar opt, const AppChar *arg);
  ErrorCountEventHandler *makeEventHandler();
private:
  unsigned genFlags_;
};

SP_DEFINE_APP(SgmlnormApp)

SgmlnormApp::SgmlnormApp()
: genFlags_(0)
{
  registerOption('d');
  registerOption('m');
  registerOption('n');
}

void SgmlnormApp::processOption(AppChar opt, const AppChar *arg)
{
  switch (opt) {
  case 'd':
    genFlags_ |= SGMLGenerator::generateDtd;
    break;
  case 'm':
    options_.eventsWanted.addMarkedSections();
    break;
  case 'n':
    options_.eventsWanted.addCommentDecls();
    break;
  default:
    ParserApp::processOption(opt, arg);
    break;
  }
}

ErrorCountEventHandler *SgmlnormApp::makeEventHandler()
{
  return new SGMLGeneratorEH(new RecordOutputCharStream(makeStdOut()),
			     genFlags_,
			     this);
}

SGMLGeneratorEH::SGMLGeneratorEH(OutputCharStream *os, unsigned genFlags,
				 Messenger *mgr)
: SGMLGenerator(os, genFlags), GenericEventHandler(*this, 1), mgr_(mgr)
{
}

void SGMLGeneratorEH::reportMessage(const Message &msg, StringC &)
{
  mgr_->dispatchMessage(msg);
}
