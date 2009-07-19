// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXPIGenServerCore.pas' rev: 5.00

#ifndef DXPIGenServerCoreHPP
#define DXPIGenServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxpigenservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXPIGenServerCore;
class PASCALIMPLEMENTATION TDXPIGenServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	int fiDelay;
	int fCharCtr;
	
public:
	__fastcall virtual TDXPIGenServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXPIGenServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property int DelayBetweenChars = {read=fiDelay, write=fiDelay, nodefault};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxpigenservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxpigenservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXPIGenServerCore
