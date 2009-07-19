// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXGopherServerCore.pas' rev: 5.00

#ifndef DXGopherServerCoreHPP
#define DXGopherServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxgopherservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *GopherTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1);

typedef void __fastcall (__closure *GopherTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread)
	;

class DELPHICLASS TDXGopherServerCore;
class PASCALIMPLEMENTATION TDXGopherServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	GopherTSimpleEvent fOnCommandWhatDoYouHave;
	GopherTBasicEvent fOnCommandRequestedDocument;
	
public:
	__fastcall virtual TDXGopherServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXGopherServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SAYIHave(Dxservercore::TDXClientThread* ClientThread, char MenuType, AnsiString Title
		, AnsiString DocumentName, AnsiString Server, int Port);
	
__published:
	__property GopherTSimpleEvent OnWhatDoYouHave = {read=fOnCommandWhatDoYouHave, write=fOnCommandWhatDoYouHave
		};
	__property GopherTBasicEvent OnRequestedDocument = {read=fOnCommandRequestedDocument, write=fOnCommandRequestedDocument
		};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxgopherservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxgopherservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXGopherServerCore
