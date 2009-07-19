// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXChargenServerCore.pas' rev: 5.00

#ifndef DXChargenServerCoreHPP
#define DXChargenServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxchargenservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXChargenServerCore;
class PASCALIMPLEMENTATION TDXChargenServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	int fiDelay;
	int fiDelay2;
	int fLineCtr;
	int fCharCtr;
	
public:
	__fastcall virtual TDXChargenServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXChargenServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property int DelayBetweenChars = {read=fiDelay, write=fiDelay, nodefault};
	__property int DelayBetweenLines = {read=fiDelay2, write=fiDelay2, nodefault};
	__property int LinesSent = {read=fLineCtr, nodefault};
	__property int CharCurLine = {read=fCharCtr, nodefault};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxchargenservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxchargenservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXChargenServerCore
