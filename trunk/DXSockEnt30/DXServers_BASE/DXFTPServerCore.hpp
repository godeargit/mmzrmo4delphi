// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXFTPServerCore.pas' rev: 5.00

#ifndef DXFTPServerCoreHPP
#define DXFTPServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxftpservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *FTPTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *FTPTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *FTPTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *FTPTPassEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm, bool &SuccessfulLogin);

typedef void __fastcall (__closure *FTPTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXFTPServerCore;
class PASCALIMPLEMENTATION TDXFTPServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	FTPTBasicEvent fOnCommandUSER;
	FTPTPassEvent fOnCommandPASS;
	FTPTBasicEvent fOnCommandACCT;
	FTPTBasicEvent fOnCommandCWD;
	FTPTSimpleEvent fOnCommandCDUP;
	FTPTBasicEvent fOnCommandSMNT;
	FTPTSimpleEvent fOnCommandQUIT;
	FTPTSimpleEvent fOnCommandREIN;
	FTPTBasicEvent fOnCommandPORT;
	FTPTSimpleEvent fOnCommandPASV;
	FTPTBasicEvent fOnCommandTYPE;
	FTPTBasicEvent fOnCommandSTRU;
	FTPTBasicEvent fOnCommandMODE;
	FTPTBasicEvent fOnCommandRETR;
	FTPTBasicEvent fOnCommandSTOR;
	FTPTSimpleEvent fOnCommandSTOU;
	FTPTBasicEvent fOnCommandAPPE;
	FTPTComplexEvent fOnCommandALLO;
	FTPTBasicEvent fOnCommandREST;
	FTPTBasicEvent fOnCommandRNFR;
	FTPTBasicEvent fOnCommandRNTO;
	FTPTSimpleEvent fOnCommandABOR;
	FTPTBasicEvent fOnCommandDELE;
	FTPTBasicEvent fOnCommandRMD;
	FTPTBasicEvent fOnCommandMKD;
	FTPTSimpleEvent fOnCommandPWD;
	FTPTBasicEvent fOnCommandLIST;
	FTPTBasicEvent fOnCommandNLST;
	FTPTBasicEvent fOnCommandSITE;
	FTPTBasicEvent fOnCommandSIZE;
	FTPTSimpleEvent fOnCommandSYST;
	FTPTBasicEvent fOnCommandSTAT;
	FTPTBasicEvent fOnCommandHELP;
	FTPTSimpleEvent fOnCommandNOOP;
	FTPTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandUSER(FTPTBasicEvent value);
	void __fastcall SetOnCommandPASS(FTPTPassEvent value);
	void __fastcall SetOnCommandACCT(FTPTBasicEvent value);
	void __fastcall SetOnCommandCWD(FTPTBasicEvent value);
	void __fastcall SetOnCommandCDUP(FTPTSimpleEvent value);
	void __fastcall SetOnCommandSMNT(FTPTBasicEvent value);
	void __fastcall SetOnCommandQUIT(FTPTSimpleEvent value);
	void __fastcall SetOnCommandREIN(FTPTSimpleEvent value);
	void __fastcall SetOnCommandPORT(FTPTBasicEvent value);
	void __fastcall SetOnCommandPASV(FTPTSimpleEvent value);
	void __fastcall SetOnCommandTYPE(FTPTBasicEvent value);
	void __fastcall SetOnCommandSTRU(FTPTBasicEvent value);
	void __fastcall SetOnCommandMODE(FTPTBasicEvent value);
	void __fastcall SetOnCommandRETR(FTPTBasicEvent value);
	void __fastcall SetOnCommandSTOR(FTPTBasicEvent value);
	void __fastcall SetOnCommandSTOU(FTPTSimpleEvent value);
	void __fastcall SetOnCommandAPPE(FTPTBasicEvent value);
	void __fastcall SetOnCommandALLO(FTPTComplexEvent value);
	void __fastcall SetOnCommandREST(FTPTBasicEvent value);
	void __fastcall SetOnCommandRNFR(FTPTBasicEvent value);
	void __fastcall SetOnCommandRNTO(FTPTBasicEvent value);
	void __fastcall SetOnCommandABOR(FTPTSimpleEvent value);
	void __fastcall SetOnCommandDELE(FTPTBasicEvent value);
	void __fastcall SetOnCommandRMD(FTPTBasicEvent value);
	void __fastcall SetOnCommandMKD(FTPTBasicEvent value);
	void __fastcall SetOnCommandPWD(FTPTSimpleEvent value);
	void __fastcall SetOnCommandLIST(FTPTBasicEvent value);
	void __fastcall SetOnCommandNLST(FTPTBasicEvent value);
	void __fastcall SetOnCommandSITE(FTPTBasicEvent value);
	void __fastcall SetOnCommandSIZE(FTPTBasicEvent value);
	void __fastcall SetOnCommandSYST(FTPTSimpleEvent value);
	void __fastcall SetOnCommandSTAT(FTPTBasicEvent value);
	void __fastcall SetOnCommandHELP(FTPTBasicEvent value);
	void __fastcall SetOnCommandNOOP(FTPTSimpleEvent value);
	
public:
	__fastcall virtual TDXFTPServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXFTPServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, Classes::TStrings* Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, Classes::TStrings* Footer);
		
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread, Classes::TStrings* MOTD
		);
	void __fastcall AddBasicEvent(AnsiString Command, FTPTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, FTPTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, FTPTComplexEvent EventProc);
	void __fastcall AddPassEvent(AnsiString Command, FTPTPassEvent EventProc);
	
__published:
	__property FTPTBasicEvent OnCommandUSER = {read=fOnCommandUSER, write=SetOnCommandUSER};
	__property FTPTPassEvent OnCommandPASS = {read=fOnCommandPASS, write=SetOnCommandPASS};
	__property FTPTBasicEvent OnCommandACCT = {read=fOnCommandACCT, write=SetOnCommandACCT};
	__property FTPTBasicEvent OnCommandCWD = {read=fOnCommandCWD, write=SetOnCommandCWD};
	__property FTPTSimpleEvent OnCommandCDUP = {read=fOnCommandCDUP, write=SetOnCommandCDUP};
	__property FTPTBasicEvent OnCommandSMNT = {read=fOnCommandSMNT, write=SetOnCommandSMNT};
	__property FTPTSimpleEvent OnCommandQUIT = {read=fOnCommandQUIT, write=SetOnCommandQUIT};
	__property FTPTSimpleEvent OnCommandREIN = {read=fOnCommandREIN, write=SetOnCommandREIN};
	__property FTPTBasicEvent OnCommandPORT = {read=fOnCommandPORT, write=SetOnCommandPORT};
	__property FTPTSimpleEvent OnCommandPASV = {read=fOnCommandPASV, write=SetOnCommandPASV};
	__property FTPTBasicEvent OnCommandTYPE = {read=fOnCommandTYPE, write=SetOnCommandTYPE};
	__property FTPTBasicEvent OnCommandSTRU = {read=fOnCommandSTRU, write=SetOnCommandSTRU};
	__property FTPTBasicEvent OnCommandMODE = {read=fOnCommandMODE, write=SetOnCommandMODE};
	__property FTPTBasicEvent OnCommandRETR = {read=fOnCommandRETR, write=SetOnCommandRETR};
	__property FTPTBasicEvent OnCommandSTOR = {read=fOnCommandSTOR, write=SetOnCommandSTOR};
	__property FTPTSimpleEvent OnCommandSTOU = {read=fOnCommandSTOU, write=SetOnCommandSTOU};
	__property FTPTBasicEvent OnCommandAPPE = {read=fOnCommandAPPE, write=SetOnCommandAPPE};
	__property FTPTComplexEvent OnCommandALLO = {read=fOnCommandALLO, write=SetOnCommandALLO};
	__property FTPTBasicEvent OnCommandREST = {read=fOnCommandREST, write=SetOnCommandREST};
	__property FTPTBasicEvent OnCommandRNFR = {read=fOnCommandRNFR, write=SetOnCommandRNFR};
	__property FTPTBasicEvent OnCommandRNTO = {read=fOnCommandRNTO, write=SetOnCommandRNTO};
	__property FTPTSimpleEvent OnCommandABOR = {read=fOnCommandABOR, write=SetOnCommandABOR};
	__property FTPTBasicEvent OnCommandDELE = {read=fOnCommandDELE, write=SetOnCommandDELE};
	__property FTPTBasicEvent OnCommandRMD = {read=fOnCommandRMD, write=SetOnCommandRMD};
	__property FTPTBasicEvent OnCommandMKD = {read=fOnCommandMKD, write=SetOnCommandMKD};
	__property FTPTSimpleEvent OnCommandPWD = {read=fOnCommandPWD, write=SetOnCommandPWD};
	__property FTPTBasicEvent OnCommandLIST = {read=fOnCommandLIST, write=SetOnCommandLIST};
	__property FTPTBasicEvent OnCommandNLST = {read=fOnCommandNLST, write=SetOnCommandNLST};
	__property FTPTBasicEvent OnCommandSITE = {read=fOnCommandSITE, write=SetOnCommandSITE};
	__property FTPTBasicEvent OnCommandSIZE = {read=fOnCommandSIZE, write=SetOnCommandSIZE};
	__property FTPTSimpleEvent OnCommandSYST = {read=fOnCommandSYST, write=SetOnCommandSYST};
	__property FTPTBasicEvent OnCommandSTAT = {read=fOnCommandSTAT, write=SetOnCommandSTAT};
	__property FTPTBasicEvent OnCommandHELP = {read=fOnCommandHELP, write=SetOnCommandHELP};
	__property FTPTSimpleEvent OnCommandNOOP = {read=fOnCommandNOOP, write=SetOnCommandNOOP};
	__property FTPTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxftpservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxftpservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXFTPServerCore
