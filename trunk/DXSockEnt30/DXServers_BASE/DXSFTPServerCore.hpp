// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSFTPServerCore.pas' rev: 5.00

#ifndef DXSFTPServerCoreHPP
#define DXSFTPServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsftpservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *SFTPTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *SFTPTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *SFTPTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *SFTPTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXSFTPServerCore;
class PASCALIMPLEMENTATION TDXSFTPServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	SFTPTBasicEvent fOnCommandUSER;
	SFTPTBasicEvent fOnCommandACCT;
	SFTPTBasicEvent fOnCommandPASS;
	SFTPTBasicEvent fOnCommandTYPE;
	SFTPTBasicEvent fOnCommandLIST;
	SFTPTBasicEvent fOnCommandCDIR;
	SFTPTBasicEvent fOnCommandKILL;
	SFTPTBasicEvent fOnCommandNAME;
	SFTPTBasicEvent fOnCommandTOBE;
	SFTPTSimpleEvent fOnCommandDONE;
	SFTPTBasicEvent fOnCommandRETR;
	SFTPTComplexEvent fOnCommandSTOR;
	SFTPTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandUSER(SFTPTBasicEvent value);
	void __fastcall SetOnCommandACCT(SFTPTBasicEvent value);
	void __fastcall SetOnCommandPASS(SFTPTBasicEvent value);
	void __fastcall SetOnCommandTYPE(SFTPTBasicEvent value);
	void __fastcall SetOnCommandLIST(SFTPTBasicEvent value);
	void __fastcall SetOnCommandCDIR(SFTPTBasicEvent value);
	void __fastcall SetOnCommandKILL(SFTPTBasicEvent value);
	void __fastcall SetOnCommandNAME(SFTPTBasicEvent value);
	void __fastcall SetOnCommandTOBE(SFTPTBasicEvent value);
	void __fastcall SetOnCommandDONE(SFTPTSimpleEvent value);
	void __fastcall SetOnCommandRETR(SFTPTBasicEvent value);
	void __fastcall SetOnCommandSTOR(SFTPTComplexEvent value);
	
public:
	__fastcall virtual TDXSFTPServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXSFTPServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, SFTPTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, SFTPTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, SFTPTComplexEvent EventProc);
	
__published:
	__property SFTPTBasicEvent OnCommandUSER = {read=fOnCommandUSER, write=SetOnCommandUSER};
	__property SFTPTBasicEvent OnCommandACCT = {read=fOnCommandACCT, write=SetOnCommandACCT};
	__property SFTPTBasicEvent OnCommandPASS = {read=fOnCommandPASS, write=SetOnCommandPASS};
	__property SFTPTBasicEvent OnCommandTYPE = {read=fOnCommandTYPE, write=SetOnCommandTYPE};
	__property SFTPTBasicEvent OnCommandLIST = {read=fOnCommandLIST, write=SetOnCommandLIST};
	__property SFTPTBasicEvent OnCommandCDIR = {read=fOnCommandCDIR, write=SetOnCommandCDIR};
	__property SFTPTBasicEvent OnCommandKILL = {read=fOnCommandKILL, write=SetOnCommandKILL};
	__property SFTPTBasicEvent OnCommandNAME = {read=fOnCommandNAME, write=SetOnCommandNAME};
	__property SFTPTBasicEvent OnCommandTOBE = {read=fOnCommandTOBE, write=SetOnCommandTOBE};
	__property SFTPTSimpleEvent OnCommandDONE = {read=fOnCommandDONE, write=SetOnCommandDONE};
	__property SFTPTBasicEvent OnCommandRETR = {read=fOnCommandRETR, write=SetOnCommandRETR};
	__property SFTPTComplexEvent OnCommandSTOR = {read=fOnCommandSTOR, write=SetOnCommandSTOR};
	__property SFTPTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxsftpservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsftpservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSFTPServerCore
