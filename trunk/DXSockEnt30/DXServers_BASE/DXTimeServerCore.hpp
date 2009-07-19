// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXTimeServerCore.pas' rev: 5.00

#ifndef DXTimeServerCoreHPP
#define DXTimeServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxtimeservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXTimeServerCore;
class PASCALIMPLEMENTATION TDXTimeServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
public:
	__fastcall virtual TDXTimeServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXTimeServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxtimeservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxtimeservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXTimeServerCore
