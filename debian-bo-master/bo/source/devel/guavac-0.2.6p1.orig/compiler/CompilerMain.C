// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: CompilerMain.C,v 1.8 1996/08/03 03:02:59 geppetto Exp $
#include "unicode_string.h"
#include "Compiler.h"
#include "CommandLine.h"
#include "JavaClassFile.h"
#include "FilePath.h"
#include "JavaDirectory.h"
#include "config.h"
#include <cstdio>
#include <iostream.h>
#include <fstream.h>
extern "C" {
  #include <stdlib.h>
}

static const string kSourceSuffix = ".java";

void SyntaxAbort(const string& command)
{
  cerr << ">> Syntax: " << command
       << " [-version] [-classpath <path>] [-d <class-directory>] [-M] "
       << "<input-file> [<input-file>]*" << endl;
  exit(1);
}

bool ParseClassPath(const string& pathString, deque<string>& pathList)
{
  string::size_type startPosition = 0;
  string::size_type colonPosition;
  do {
    colonPosition = pathString.find(':', startPosition);
    if (colonPosition > pathString.size()) {
      colonPosition = pathString.size();
    }
    string directoryName(pathString, startPosition,
			 colonPosition - startPosition);
    CJavaDirectory path(directoryName);
    if (path.IsValid()) {
      pathList.push_back(directoryName);
    } else {
      cerr << ">> Warning: Invalid classpath entry " << directoryName << endl;
    }
    startPosition = colonPosition + 1;
  } while(colonPosition < pathString.size());
  return !pathList.empty();
}

int
main(int argc, const char** argv) {
  CCommandLine arguments;
  arguments.ParseArgumentVector(argc, argv);
  deque<string> sourceFiles;
  string classPathString;
  CFilePath* classDestinationBase = 0;
  bool dependMode = false;

  if (arguments.GetArgumentCount() < 1) {
    SyntaxAbort(arguments.GetCommand());
  }
  for (CCommandLine::ArgumentIteratorType arg = arguments.GetArgumentBegin();
       arg != arguments.GetArgumentEnd(); ++arg) {
    unsigned long optionLength = (*arg).length();
    if (*arg == "-classpath") {
      ++arg;
      if (arg == arguments.GetArgumentEnd()) {
	SyntaxAbort(arguments.GetCommand());
      } else {
	classPathString = *arg;
      }
    } else if (*arg == "-version") {
      cerr << "guavac version " << VERSION << endl;
    } else if (*arg == "-M") {
      dependMode = true;
    } else if (*arg == "-d") {
      ++arg;
      if (arg == arguments.GetArgumentEnd()) {
	SyntaxAbort(arguments.GetCommand());
      } else {
	delete classDestinationBase;
	classDestinationBase = new CFilePath(*arg);
	if (!classDestinationBase->IsDirectory()) {
	  cerr << ">> Invalid destination directory: " << *arg << endl;
	  delete classDestinationBase;
	  classDestinationBase = 0;
	}
      }
    } else if (optionLength > 0 && (*arg)[0] == '-') {
      cerr << ">> Invalid compiler flag: " << *arg << endl;
      SyntaxAbort(arguments.GetCommand());
    } else {
      CFilePath filePath(*arg);
      if (!filePath.IsFile()) {
	cerr << ">> Invalid argument (not a plain file): " << *arg << endl;
	SyntaxAbort(arguments.GetCommand());
      } else if (optionLength < kSourceSuffix.length() ||
		 (*arg).compare(".java",
				optionLength - kSourceSuffix.length()) != 0) {
	cerr << ">> Invalid source file (no .java suffix): " << *arg << endl;
	SyntaxAbort(arguments.GetCommand());
      } else {
	sourceFiles.push_back(*arg);
      }
    }
  }
  if (classPathString.size() == 0) {
    const char* classPathEnvironment = getenv("CLASSPATH");
    if (classPathEnvironment == 0) {
      classPathString = DEFAULT_CLASSPATH;
    } else {
      classPathString = classPathEnvironment;
    }
  }
  if (sourceFiles.size() == 0) {
    cerr << ">> No valid source file names given." << endl;
    exit(1);
  }
  deque<string> classPath;
  if (! ParseClassPath(classPathString, classPath)) {
    cerr << ">> Invalid class path: " << classPathString << endl;
    exit(1);
  }
  bool success = true;
  for (deque<string>::const_iterator sourceIterator = sourceFiles.begin();
       success && !(sourceIterator == sourceFiles.end()); ++sourceIterator) {
    string fileName = *sourceIterator;
    string::size_type lastSlash = fileName.find_last_of('/');
    if (lastSlash == string::npos) {
      lastSlash = 0;
    } else {
      lastSlash++;
    }
    string fileDirectory(fileName, 0, lastSlash);
    CCompiler::ClassList resultClasses;
    deque<string> depends;
    success &= 
      CCompiler::CompileFile(fileName, classPath, resultClasses, depends);
    if (success) {
      if (!dependMode) {
	cout << "Compilation Successful: " << resultClasses.size()
	     << " classes or interfaces found:" << endl;
      }
      for (CCompiler::ClassList::iterator i = resultClasses.begin();
	   i != resultClasses.end(); ++i) {
	string className = ::UnicodeToString((*i)->GetShortName());
	string outFileName;
	if (classDestinationBase == 0) {
	  outFileName = fileDirectory + className + ".class";
	} else {
	  outFileName = classDestinationBase->GetString() + "/" +
	    ::UnicodeToString((*i)->GetClassName()) + ".class";
	}
	if (dependMode) {
	  cout << outFileName << ":";
	  for (deque<string>::iterator iter = depends.begin();
	       !(iter == depends.end()); ++iter) {
	    cout << " \\\n        " << *iter;
	  }
	  cout << endl;
	} else {
	  cout << " *  " << className << endl;
	  ofstream outFile;
	  outFile.open(outFileName.c_str());
	  if (!outFile.is_open()) {
	    cerr << ">> could not open output file " << outFileName << endl;
	  } else {
	    (*i)->Compile(outFile);
	    outFile.close();
	  }
	}
      }
    } else {
      cerr << "Compilation Failed" << endl;
    }
    for (CCompiler::ClassList::iterator i = resultClasses.begin();
	 i != resultClasses.end(); ++i) {
      delete *i;
    }
  }
  return success ? 0 : 1;
}

