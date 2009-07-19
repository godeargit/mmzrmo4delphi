// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXNTPServerCore.pas' rev: 5.00

#ifndef DXNTPServerCoreHPP
#define DXNTPServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxntpservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXNTPServerCore;
class PASCALIMPLEMENTATION TDXNTPServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
public:
	__fastcall virtual TDXNTPServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXNTPServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxntpservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxntpservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXNTPServerCore
