// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXPOP2ServerCore.pas' rev: 5.00

#ifndef DXPOP2ServerCoreHPP
#define DXPOP2ServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxpop2servercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *POP2TSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *POP2TBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *POP2TComplexEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *POP2TOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXPOP2ServerCore;
class PASCALIMPLEMENTATION TDXPOP2ServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	POP2TComplexEvent fOnCommandHELO;
	POP2TBasicEvent fOnCommandFOLD;
	POP2TBasicEvent fOnCommandREAD;
	POP2TSimpleEvent fOnCommandRETR;
	POP2TSimpleEvent fOnCommandACKS;
	POP2TSimpleEvent fOnCommandACKD;
	POP2TSimpleEvent fOnCommandNACK;
	POP2TSimpleEvent fOnCommandQUIT;
	POP2TOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandHELO(POP2TComplexEvent value);
	void __fastcall SetOnCommandFOLD(POP2TBasicEvent value);
	void __fastcall SetOnCommandREAD(POP2TBasicEvent value);
	void __fastcall SetOnCommandRETR(POP2TSimpleEvent value);
	void __fastcall SetOnCommandACKS(POP2TSimpleEvent value);
	void __fastcall SetOnCommandACKD(POP2TSimpleEvent value);
	void __fastcall SetOnCommandNACK(POP2TSimpleEvent value);
	void __fastcall SetOnCommandQUIT(POP2TSimpleEvent value);
	
public:
	__fastcall virtual TDXPOP2ServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXPOP2ServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, POP2TBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, POP2TSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, POP2TComplexEvent EventProc);
	
__published:
	__property POP2TComplexEvent OnCommandHELO = {read=fOnCommandHELO, write=SetOnCommandHELO};
	__property POP2TBasicEvent OnCommandFOLD = {read=fOnCommandFOLD, write=SetOnCommandFOLD};
	__property POP2TBasicEvent OnCommandREAD = {read=fOnCommandREAD, write=SetOnCommandREAD};
	__property POP2TSimpleEvent OnCommandRETR = {read=fOnCommandRETR, write=SetOnCommandRETR};
	__property POP2TSimpleEvent OnCommandACKS = {read=fOnCommandACKS, write=SetOnCommandACKS};
	__property POP2TSimpleEvent OnCommandACKD = {read=fOnCommandACKD, write=SetOnCommandACKD};
	__property POP2TSimpleEvent OnCommandNACK = {read=fOnCommandNACK, write=SetOnCommandNACK};
	__property POP2TSimpleEvent OnCommandQUIT = {read=fOnCommandQUIT, write=SetOnCommandQUIT};
	__property POP2TOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxpop2servercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxpop2servercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXPOP2ServerCore
