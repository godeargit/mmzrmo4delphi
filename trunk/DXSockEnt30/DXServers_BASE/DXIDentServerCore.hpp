// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXIDentServerCore.pas' rev: 5.00

#ifndef DXIDentServerCoreHPP
#define DXIDentServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxidentservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *IDentTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Query);

class DELPHICLASS TDXIDentServerCore;
class PASCALIMPLEMENTATION TDXIDentServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	IDentTBasicEvent fOnCommandQuery;
	
public:
	__fastcall virtual TDXIDentServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXIDentServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property IDentTBasicEvent OnCommandQuery = {read=fOnCommandQuery, write=fOnCommandQuery};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxidentservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxidentservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXIDentServerCore
