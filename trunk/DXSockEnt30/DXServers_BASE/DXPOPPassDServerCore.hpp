// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXPOPPassDServerCore.pas' rev: 5.00

#ifndef DXPOPPassDServerCoreHPP
#define DXPOPPassDServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxpoppassdservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *POPPassDTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread
	);

typedef void __fastcall (__closure *POPPassDTBasicEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm);

typedef void __fastcall (__closure *POPPassDTOtherEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXPOPPassDServerCore;
class PASCALIMPLEMENTATION TDXPOPPassDServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	POPPassDTBasicEvent fOnCommandUSER;
	POPPassDTBasicEvent fOnCommandPASS;
	POPPassDTBasicEvent fOnCommandNEWPASS;
	POPPassDTSimpleEvent fOnCommandQUIT;
	POPPassDTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandUSER(POPPassDTBasicEvent value);
	void __fastcall SetOnCommandPASS(POPPassDTBasicEvent value);
	void __fastcall SetOnCommandNEWPASS(POPPassDTBasicEvent value);
	void __fastcall SetOnCommandQUIT(POPPassDTSimpleEvent value);
	
public:
	__fastcall virtual TDXPOPPassDServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXPOPPassDServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall NewPassOK(Dxservercore::TDXClientThread* ClientThread, AnsiString Message);
	void __fastcall NewPassERR(Dxservercore::TDXClientThread* ClientThread, AnsiString Message);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, POPPassDTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, POPPassDTSimpleEvent EventProc);
	
__published:
	__property POPPassDTBasicEvent OnCommandUSER = {read=fOnCommandUSER, write=SetOnCommandUSER};
	__property POPPassDTBasicEvent OnCommandPASS = {read=fOnCommandPASS, write=SetOnCommandPASS};
	__property POPPassDTBasicEvent OnCommandNEWPASS = {read=fOnCommandNEWPASS, write=SetOnCommandNEWPASS
		};
	__property POPPassDTSimpleEvent OnCommandQUIT = {read=fOnCommandQUIT, write=SetOnCommandQUIT};
	__property POPPassDTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxpoppassdservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxpoppassdservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXPOPPassDServerCore
