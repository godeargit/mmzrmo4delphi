// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXFingerServerCore.pas' rev: 5.00

#ifndef DXFingerServerCoreHPP
#define DXFingerServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxfingerservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *FINGERTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Lookup);

class DELPHICLASS TDXFingerServerCore;
class PASCALIMPLEMENTATION TDXFingerServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	FINGERTBasicEvent fOnCommandGET;
	
public:
	__fastcall virtual TDXFingerServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXFingerServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	
__published:
	__property FINGERTBasicEvent OnCommandGET = {read=fOnCommandGET, write=fOnCommandGET};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxfingerservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxfingerservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXFingerServerCore
