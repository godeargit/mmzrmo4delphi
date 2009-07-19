// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXRequestReplyServerCore.pas' rev: 5.00

#ifndef DXRequestReplyServerCoreHPP
#define DXRequestReplyServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxrequestreplyservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *RequestReplyTBasicEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1);

typedef void __fastcall (__closure *RequestReplyTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread
	);

typedef void __fastcall (__closure *RequestReplyTComplexEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1, AnsiString Parm2);

class DELPHICLASS TDXRequestReplyServerCore;
class PASCALIMPLEMENTATION TDXRequestReplyServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
public:
	__fastcall virtual TDXRequestReplyServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXRequestReplyServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, RequestReplyTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, RequestReplyTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, RequestReplyTComplexEvent EventProc);
};


//-- var, const, procedure ---------------------------------------------------
static const Word PACKET_SIZE = 0x800;
static const int MAX_TIMEOUT = 0x1d4c0;
#define LOGON_REQUEST "LogonRequest"
#define LOGOFF_REQUEST "LogoffRequest"
#define COMMAND_REQUEST "CommandRequest"
#define UPLOAD_REQUEST "UploadRequest"
#define DOWNLOAD_REQUEST "DownloadRequest"

}	/* namespace Dxrequestreplyservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxrequestreplyservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXRequestReplyServerCore
