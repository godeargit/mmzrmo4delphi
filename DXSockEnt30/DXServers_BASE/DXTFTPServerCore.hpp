// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXTFTPServerCore.pas' rev: 5.00

#ifndef DXTFTPServerCoreHPP
#define DXTFTPServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxtftpservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TFTPTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *TFTPTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *TFTPTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *TFTPTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXTFTPServerCore;
class PASCALIMPLEMENTATION TDXTFTPServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	TFTPTBasicEvent fOnCommandUSER;
	TFTPTBasicEvent fOnCommandACCT;
	TFTPTBasicEvent fOnCommandPASS;
	TFTPTBasicEvent fOnCommandTYPE;
	TFTPTBasicEvent fOnCommandLIST;
	TFTPTBasicEvent fOnCommandCDIR;
	TFTPTBasicEvent fOnCommandKILL;
	TFTPTBasicEvent fOnCommandNAME;
	TFTPTBasicEvent fOnCommandTOBE;
	TFTPTSimpleEvent fOnCommandDONE;
	TFTPTBasicEvent fOnCommandRETR;
	TFTPTComplexEvent fOnCommandSTOR;
	TFTPTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandUSER(TFTPTBasicEvent value);
	void __fastcall SetOnCommandACCT(TFTPTBasicEvent value);
	void __fastcall SetOnCommandPASS(TFTPTBasicEvent value);
	void __fastcall SetOnCommandTYPE(TFTPTBasicEvent value);
	void __fastcall SetOnCommandLIST(TFTPTBasicEvent value);
	void __fastcall SetOnCommandCDIR(TFTPTBasicEvent value);
	void __fastcall SetOnCommandKILL(TFTPTBasicEvent value);
	void __fastcall SetOnCommandNAME(TFTPTBasicEvent value);
	void __fastcall SetOnCommandTOBE(TFTPTBasicEvent value);
	void __fastcall SetOnCommandDONE(TFTPTSimpleEvent value);
	void __fastcall SetOnCommandRETR(TFTPTBasicEvent value);
	void __fastcall SetOnCommandSTOR(TFTPTComplexEvent value);
	
public:
	__fastcall virtual TDXTFTPServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXTFTPServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, TFTPTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, TFTPTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, TFTPTComplexEvent EventProc);
	
__published:
	__property TFTPTBasicEvent OnCommandUSER = {read=fOnCommandUSER, write=SetOnCommandUSER};
	__property TFTPTBasicEvent OnCommandACCT = {read=fOnCommandACCT, write=SetOnCommandACCT};
	__property TFTPTBasicEvent OnCommandPASS = {read=fOnCommandPASS, write=SetOnCommandPASS};
	__property TFTPTBasicEvent OnCommandTYPE = {read=fOnCommandTYPE, write=SetOnCommandTYPE};
	__property TFTPTBasicEvent OnCommandLIST = {read=fOnCommandLIST, write=SetOnCommandLIST};
	__property TFTPTBasicEvent OnCommandCDIR = {read=fOnCommandCDIR, write=SetOnCommandCDIR};
	__property TFTPTBasicEvent OnCommandKILL = {read=fOnCommandKILL, write=SetOnCommandKILL};
	__property TFTPTBasicEvent OnCommandNAME = {read=fOnCommandNAME, write=SetOnCommandNAME};
	__property TFTPTBasicEvent OnCommandTOBE = {read=fOnCommandTOBE, write=SetOnCommandTOBE};
	__property TFTPTSimpleEvent OnCommandDONE = {read=fOnCommandDONE, write=SetOnCommandDONE};
	__property TFTPTBasicEvent OnCommandRETR = {read=fOnCommandRETR, write=SetOnCommandRETR};
	__property TFTPTComplexEvent OnCommandSTOR = {read=fOnCommandSTOR, write=SetOnCommandSTOR};
	__property TFTPTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxtftpservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxtftpservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXTFTPServerCore
