// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXDiscardServerCore.pas' rev: 5.00

#ifndef DXDiscardServerCoreHPP
#define DXDiscardServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxdiscardservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXDiscardServerCore;
class PASCALIMPLEMENTATION TDXDiscardServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	bool fbCloseOnTimeout;
	
public:
	__fastcall virtual TDXDiscardServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXDiscardServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property bool CloseOnTimeout = {read=fbCloseOnTimeout, write=fbCloseOnTimeout, nodefault};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxdiscardservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxdiscardservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXDiscardServerCore
