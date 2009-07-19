// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXWhoisServerCore.pas' rev: 5.00

#ifndef DXWhoisServerCoreHPP
#define DXWhoisServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxwhoisservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *WHOISTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Lookup);

class DELPHICLASS TDXWhoisServerCore;
class PASCALIMPLEMENTATION TDXWhoisServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	WHOISTBasicEvent fOnCommandGET;
	
public:
	__fastcall virtual TDXWhoisServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXWhoisServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property WHOISTBasicEvent OnCommandGET = {read=fOnCommandGET, write=fOnCommandGET};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxwhoisservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxwhoisservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXWhoisServerCore
