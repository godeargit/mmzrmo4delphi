// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSMTPServerCore.pas' rev: 5.00

#ifndef DXSMTPServerCoreHPP
#define DXSMTPServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsmtpservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *SMTPTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *SMTPTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *SMTPTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1, AnsiString OptionalParm2);

typedef void __fastcall (__closure *SMTPTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXSMTPServerCore;
class PASCALIMPLEMENTATION TDXSMTPServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	SMTPTBasicEvent fOnCommandHELO;
	SMTPTBasicEvent fOnCommandEHLO;
	SMTPTSimpleEvent fOnCommandRset;
	SMTPTComplexEvent fOnCommandMail;
	SMTPTComplexEvent fOnCommandRcpt;
	SMTPTSimpleEvent fOnCommandData;
	SMTPTSimpleEvent fOnCommandQuit;
	SMTPTBasicEvent fOnCommandHelp;
	SMTPTBasicEvent fOnCommandSend;
	SMTPTBasicEvent fOnCommandSaml;
	SMTPTBasicEvent fOnCommandSoml;
	SMTPTSimpleEvent fOnCommandNoop;
	SMTPTBasicEvent fOnCommandExpn;
	SMTPTBasicEvent fOnCommandVrfy;
	SMTPTSimpleEvent fOnCommandTurn;
	SMTPTBasicEvent fOnCommandEtrn;
	SMTPTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandHELO(SMTPTBasicEvent value);
	void __fastcall SetOnCommandEHLO(SMTPTBasicEvent value);
	void __fastcall SetOnCommandRSET(SMTPTSimpleEvent value);
	void __fastcall SetOnCommandMAIL(SMTPTComplexEvent value);
	void __fastcall SetOnCommandRCPT(SMTPTComplexEvent value);
	void __fastcall SetOnCommandDATA(SMTPTSimpleEvent value);
	void __fastcall SetOnCommandQUIT(SMTPTSimpleEvent value);
	void __fastcall SetOnCommandHELP(SMTPTBasicEvent value);
	void __fastcall SetOnCommandSEND(SMTPTBasicEvent value);
	void __fastcall SetOnCommandSAML(SMTPTBasicEvent value);
	void __fastcall SetOnCommandSOML(SMTPTBasicEvent value);
	void __fastcall SetOnCommandNOOP(SMTPTSimpleEvent value);
	void __fastcall SetOnCommandEXPN(SMTPTBasicEvent value);
	void __fastcall SetOnCommandVRFY(SMTPTBasicEvent value);
	void __fastcall SetOnCommandTURN(SMTPTSimpleEvent value);
	void __fastcall SetOnCommandETRN(SMTPTBasicEvent value);
	
public:
	__fastcall virtual TDXSMTPServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXSMTPServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall SayOK(Dxservercore::TDXClientThread* ClientThread, AnsiString OptionalNote);
	void __fastcall SayNotImplemented(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SayRcptUnknown(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SayMailUnknown(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SayReadyForMessage(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SayReceivedMessage(Dxservercore::TDXClientThread* ClientThread, int OptionalSize);
	bool __fastcall GetMessage(Dxservercore::TDXClientThread* ClientThread, Classes::TStream* Stream, __int64 
		MaxSize);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, SMTPTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, SMTPTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, SMTPTComplexEvent EventProc);
	
__published:
	__property SMTPTBasicEvent OnCommandHelo = {read=fOnCommandHELO, write=SetOnCommandHELO};
	__property SMTPTBasicEvent OnCommandEhlo = {read=fOnCommandEHLO, write=SetOnCommandEHLO};
	__property SMTPTSimpleEvent OnCommandRset = {read=fOnCommandRset, write=SetOnCommandRSET};
	__property SMTPTComplexEvent OnCommandMail = {read=fOnCommandMail, write=SetOnCommandMAIL};
	__property SMTPTComplexEvent OnCommandRcpt = {read=fOnCommandRcpt, write=SetOnCommandRCPT};
	__property SMTPTSimpleEvent OnCommandData = {read=fOnCommandData, write=SetOnCommandDATA};
	__property SMTPTSimpleEvent OnCommandQuit = {read=fOnCommandQuit, write=SetOnCommandQUIT};
	__property SMTPTBasicEvent OnCommandHelp = {read=fOnCommandHelp, write=SetOnCommandHELP};
	__property SMTPTBasicEvent OnCommandSend = {read=fOnCommandSend, write=SetOnCommandSEND};
	__property SMTPTBasicEvent OnCommandSaml = {read=fOnCommandSaml, write=SetOnCommandSAML};
	__property SMTPTBasicEvent OnCommandSoml = {read=fOnCommandSoml, write=SetOnCommandSOML};
	__property SMTPTSimpleEvent OnCommandNoop = {read=fOnCommandNoop, write=SetOnCommandNOOP};
	__property SMTPTBasicEvent OnCommandExpn = {read=fOnCommandExpn, write=SetOnCommandEXPN};
	__property SMTPTBasicEvent OnCommandVrfy = {read=fOnCommandVrfy, write=SetOnCommandVRFY};
	__property SMTPTSimpleEvent OnCommandTurn = {read=fOnCommandTurn, write=SetOnCommandTURN};
	__property SMTPTBasicEvent OnCommandEtrn = {read=fOnCommandEtrn, write=SetOnCommandETRN};
	__property SMTPTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxsmtpservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsmtpservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSMTPServerCore
