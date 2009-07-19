// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXEchoServerCore.pas' rev: 5.00

#ifndef DXEchoServerCoreHPP
#define DXEchoServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxechoservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXEchoServerCore;
class PASCALIMPLEMENTATION TDXEchoServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	bool fbCloseOnTimeout;
	
public:
	__fastcall virtual TDXEchoServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXEchoServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property bool CloseOnTimeout = {read=fbCloseOnTimeout, write=fbCloseOnTimeout, nodefault};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxechoservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxechoservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXEchoServerCore
