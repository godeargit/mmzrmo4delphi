// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXPOP3ServerCore.pas' rev: 5.00

#ifndef DXPOP3ServerCoreHPP
#define DXPOP3ServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxpop3servercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *POP3TSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *POP3TBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *POP3TComplexEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *POP3TOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXPOP3ServerCore;
class PASCALIMPLEMENTATION TDXPOP3ServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	POP3TBasicEvent fOnCommandUSER;
	POP3TBasicEvent fOnCommandPASS;
	POP3TSimpleEvent fOnCommandSTAT;
	POP3TBasicEvent fOnCommandLIST;
	POP3TBasicEvent fOnCommandUIDL;
	POP3TBasicEvent fOnCommandRETR;
	POP3TSimpleEvent fOnCommandNOOP;
	POP3TBasicEvent fOnCommandDELE;
	POP3TSimpleEvent fOnCommandRSET;
	POP3TSimpleEvent fOnCommandQUIT;
	POP3TComplexEvent fOnCommandAPOP;
	POP3TComplexEvent fOnCommandTOP;
	POP3TOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandUSER(POP3TBasicEvent value);
	void __fastcall SetOnCommandPASS(POP3TBasicEvent value);
	void __fastcall SetOnCommandSTAT(POP3TSimpleEvent value);
	void __fastcall SetOnCommandLIST(POP3TBasicEvent value);
	void __fastcall SetOnCommandUIDL(POP3TBasicEvent value);
	void __fastcall SetOnCommandRETR(POP3TBasicEvent value);
	void __fastcall SetOnCommandNOOP(POP3TSimpleEvent value);
	void __fastcall SetOnCommandDELE(POP3TBasicEvent value);
	void __fastcall SetOnCommandRSET(POP3TSimpleEvent value);
	void __fastcall SetOnCommandQUIT(POP3TSimpleEvent value);
	void __fastcall SetOnCommandAPOP(POP3TComplexEvent value);
	void __fastcall SetOnCommandTOP(POP3TComplexEvent value);
	
public:
	__fastcall virtual TDXPOP3ServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXPOP3ServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall SayOK(Dxservercore::TDXClientThread* ClientThread, AnsiString Message);
	void __fastcall SayERR(Dxservercore::TDXClientThread* ClientThread, AnsiString Message);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, POP3TBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, POP3TSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, POP3TComplexEvent EventProc);
	
__published:
	__property POP3TBasicEvent OnCommandUSER = {read=fOnCommandUSER, write=SetOnCommandUSER};
	__property POP3TBasicEvent OnCommandPASS = {read=fOnCommandPASS, write=SetOnCommandPASS};
	__property POP3TSimpleEvent OnCommandSTAT = {read=fOnCommandSTAT, write=SetOnCommandSTAT};
	__property POP3TBasicEvent OnCommandLIST = {read=fOnCommandLIST, write=SetOnCommandLIST};
	__property POP3TBasicEvent OnCommandUIDL = {read=fOnCommandUIDL, write=SetOnCommandUIDL};
	__property POP3TBasicEvent OnCommandRETR = {read=fOnCommandRETR, write=SetOnCommandRETR};
	__property POP3TSimpleEvent OnCommandNOOP = {read=fOnCommandNOOP, write=SetOnCommandNOOP};
	__property POP3TBasicEvent OnCommandDELE = {read=fOnCommandDELE, write=SetOnCommandDELE};
	__property POP3TSimpleEvent OnCommandRSET = {read=fOnCommandRSET, write=SetOnCommandRSET};
	__property POP3TSimpleEvent OnCommandQUIT = {read=fOnCommandQUIT, write=SetOnCommandQUIT};
	__property POP3TComplexEvent OnCommandAPOP = {read=fOnCommandAPOP, write=SetOnCommandAPOP};
	__property POP3TComplexEvent OnCommandTOP = {read=fOnCommandTOP, write=SetOnCommandTOP};
	__property POP3TOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxpop3servercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxpop3servercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXPOP3ServerCore
