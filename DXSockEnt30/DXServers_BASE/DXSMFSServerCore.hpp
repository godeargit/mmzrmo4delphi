// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSMFSServerCore.pas' rev: 5.00

#ifndef DXSMFSServerCoreHPP
#define DXSMFSServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsmfsservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXSMFSServerCore;
class PASCALIMPLEMENTATION TDXSMFSServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
public:
	__fastcall virtual TDXSMFSServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXSMFSServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxsmfsservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsmfsservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSMFSServerCore
