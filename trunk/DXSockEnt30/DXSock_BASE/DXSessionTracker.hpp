// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSessionTracker.pas' rev: 5.00

#ifndef DXSessionTrackerHPP
#define DXSessionTrackerHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <DXString.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsessiontracker
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXSessionTracker;
class PASCALIMPLEMENTATION TDXSessionTracker : public Dxstring::TDXComponent 
{
	typedef Dxstring::TDXComponent inherited;
	
private:
	Classes::TList* fSessionArray;
	
public:
	__fastcall virtual TDXSessionTracker(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXSessionTracker(void);
	void __fastcall RegisterSession(Classes::TThread* ClientThread);
	void __fastcall UnregisterSession(Classes::TThread* ClientThread);
	void __fastcall CloseAllSessions(void);
	void __fastcall ClearAllSessions(void);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxsessiontracker */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsessiontracker;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSessionTracker
