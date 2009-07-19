// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXQOTDServerCore.pas' rev: 5.00

#ifndef DXQOTDServerCoreHPP
#define DXQOTDServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxqotdservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *QOTDTBasicEvent)(Dxservercore::TDXClientThread* ClientThread);

class DELPHICLASS TDXQOTDServerCore;
class PASCALIMPLEMENTATION TDXQOTDServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	QOTDTBasicEvent fOnCommandQOTD;
	
public:
	__fastcall virtual TDXQOTDServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXQOTDServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property QOTDTBasicEvent OnCommandQOTD = {read=fOnCommandQOTD, write=fOnCommandQOTD};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxqotdservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxqotdservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXQOTDServerCore
