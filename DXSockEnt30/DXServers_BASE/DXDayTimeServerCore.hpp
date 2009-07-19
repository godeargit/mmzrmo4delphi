// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXDayTimeServerCore.pas' rev: 5.00

#ifndef DXDayTimeServerCoreHPP
#define DXDayTimeServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxdaytimeservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXDayTimeServerCore;
class PASCALIMPLEMENTATION TDXDayTimeServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	AnsiString fsDateFormat;
	
public:
	__fastcall virtual TDXDayTimeServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXDayTimeServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property AnsiString DateFormat = {read=fsDateFormat, write=fsDateFormat};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxdaytimeservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxdaytimeservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXDayTimeServerCore
