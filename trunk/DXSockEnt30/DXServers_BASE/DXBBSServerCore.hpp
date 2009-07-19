// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXBBSServerCore.pas' rev: 5.00

#ifndef DXBBSServerCoreHPP
#define DXBBSServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxbbsservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *BBSTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *BBSTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *BBSTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *BBSTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXBBSServerCore;
class PASCALIMPLEMENTATION TDXBBSServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	BBSTSimpleEvent fOnCommandTimeout;
	BBSTComplexEvent fOnCommandAUTH;
	BBSTBasicEvent fOnCommandMENU;
	BBSTSimpleEvent fOnCommandPING;
	BBSTSimpleEvent fOnCommandPONG;
	BBSTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandPING(BBSTSimpleEvent value);
	void __fastcall SetOnCommandPONG(BBSTSimpleEvent value);
	void __fastcall SetOnCommandMENU(BBSTBasicEvent value);
	void __fastcall SetOnCommandAUTH(BBSTComplexEvent value);
	
public:
	__fastcall virtual TDXBBSServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXBBSServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, Classes::TStream* Header, Classes::TStream* 
		MOTD);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, Classes::TStream* Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, BBSTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, BBSTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, BBSTComplexEvent EventProc);
	
__published:
	__property BBSTComplexEvent OnCommandAUTH = {read=fOnCommandAUTH, write=SetOnCommandAUTH};
	__property BBSTBasicEvent OnCommandMENU = {read=fOnCommandMENU, write=SetOnCommandMENU};
	__property BBSTSimpleEvent OnCommandPING = {read=fOnCommandPING, write=SetOnCommandPING};
	__property BBSTSimpleEvent OnCommandPONG = {read=fOnCommandPONG, write=SetOnCommandPONG};
	__property BBSTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
	__property BBSTSimpleEvent OnCommandTimeout = {read=fOnCommandTimeout, write=fOnCommandTimeout};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxbbsservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxbbsservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXBBSServerCore
