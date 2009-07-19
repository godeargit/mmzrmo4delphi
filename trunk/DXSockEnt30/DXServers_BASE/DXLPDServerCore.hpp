// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXLPDServerCore.pas' rev: 5.00

#ifndef DXLPDServerCoreHPP
#define DXLPDServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxlpdservercore
{
//-- type declarations -------------------------------------------------------
struct TLPDControlFile;
typedef TLPDControlFile *PLPDControlFile;

struct TLPDControlFile
{
	System::SmallString<31>  ClassStr;
	System::SmallString<31>  Host;
	int Indent;
	System::SmallString<99>  Job;
	AnsiString UserBanner;
	AnsiString MailResultsTo;
	System::SmallString<131>  SourceFileName;
	System::SmallString<31>  UserID;
	AnsiString SymbolicLinkData;
	System::SmallString<79>  Title;
	AnsiString Unlink;
	int Width;
	AnsiString TroffRFont;
	AnsiString TroffIFont;
	AnsiString TroffBFont;
	AnsiString TroffSFont;
	AnsiString PlotCIF;
	AnsiString PrintDVI;
	AnsiString FormattedFile;
	AnsiString PlotFile;
	AnsiString Kerberized;
	AnsiString PrintRAW;
	AnsiString DITroff;
	AnsiString PostScript;
	AnsiString PRFormat;
	AnsiString Fortran;
	AnsiString TroffOutput;
	AnsiString Raster;
	AnsiString Palladium;
} ;

typedef void __fastcall (__closure *LPDTControlFileEvent)(Dxservercore::TDXClientThread* ClientThread
	, PLPDControlFile ControlFile);

typedef void __fastcall (__closure *LPDTSpoolFileEvent)(Dxservercore::TDXClientThread* ClientThread, 
	int Size, AnsiString PathFileName);

typedef void __fastcall (__closure *LPDTQueueEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Queue);

typedef void __fastcall (__closure *LPDTQueueListEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Queue, AnsiString List);

typedef void __fastcall (__closure *LPDTQueueRemoveEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Queue, AnsiString Agent, AnsiString List);

typedef void __fastcall (__closure *LPDTQueueQueryEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Queue);

typedef void __fastcall (__closure *LPDTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXLPDServerCore;
class PASCALIMPLEMENTATION TDXLPDServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	LPDTQueueEvent fOnCommandPrintWaiting;
	LPDTQueueEvent fOnCommandReceiveJob;
	LPDTQueueListEvent fOnCommandSendJob;
	LPDTQueueQueryEvent fOnCommandQueueQuery;
	LPDTQueueRemoveEvent fOnCommandRemoveJob;
	LPDTOtherEvent fOnCommandOther;
	LPDTControlFileEvent fOnJobControlFile;
	LPDTSpoolFileEvent fOnJobSpoolFile;
	AnsiString fSpoolPath;
	AnsiString fControlPath;
	
protected:
	void __fastcall SetOnCommandPrintWaiting(LPDTQueueEvent value);
	void __fastcall SetOnCommandReceiveJob(LPDTQueueEvent value);
	void __fastcall SetOnCommandSendJob(LPDTQueueListEvent value);
	void __fastcall SetOnCommandQueueQuery(LPDTQueueQueryEvent value);
	void __fastcall SetOnCommandRemoveJob(LPDTQueueRemoveEvent value);
	
public:
	__fastcall virtual TDXLPDServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXLPDServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SayOK(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall ProcessReceiveJobSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddQueueEvent(char Command, LPDTQueueEvent EventProc);
	void __fastcall AddListEvent(char Command, LPDTQueueListEvent EventProc);
	void __fastcall AddRemoveEvent(char Command, LPDTQueueRemoveEvent EventProc);
	void __fastcall AddQueryEvent(char Command, LPDTQueueQueryEvent EventProc);
	
__published:
	__property LPDTQueueEvent OnCommandPrintWaiting = {read=fOnCommandPrintWaiting, write=SetOnCommandPrintWaiting
		};
	__property LPDTQueueEvent OnCommandReceiveJob = {read=fOnCommandReceiveJob, write=SetOnCommandReceiveJob
		};
	__property LPDTQueueListEvent OnCommandSendJob = {read=fOnCommandSendJob, write=SetOnCommandSendJob
		};
	__property LPDTQueueQueryEvent OnCommandQueueQuery = {read=fOnCommandQueueQuery, write=SetOnCommandQueueQuery
		};
	__property LPDTQueueRemoveEvent OnCommandRemoveJob = {read=fOnCommandRemoveJob, write=SetOnCommandRemoveJob
		};
	__property LPDTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
	__property LPDTControlFileEvent OnJobControlFile = {read=fOnJobControlFile, write=fOnJobControlFile
		};
	__property LPDTSpoolFileEvent OnJobSpoolFile = {read=fOnJobSpoolFile, write=fOnJobSpoolFile};
	__property AnsiString InternalSpoolPath = {read=fSpoolPath, write=fSpoolPath};
	__property AnsiString InternalControlPath = {read=fControlPath, write=fControlPath};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxlpdservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxlpdservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXLPDServerCore
