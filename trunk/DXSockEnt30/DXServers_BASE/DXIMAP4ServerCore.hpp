// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXIMAP4ServerCore.pas' rev: 5.00

#ifndef DXIMAP4ServerCoreHPP
#define DXIMAP4ServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dximap4servercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *IMAPTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Tag);

typedef void __fastcall (__closure *IMAPTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Tag, AnsiString Parm);

typedef void __fastcall (__closure *IMAPTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Tag, AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *IMAPTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Tag, AnsiString Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXIMAP4ServerCore;
class PASCALIMPLEMENTATION TDXIMAP4ServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	IMAPTSimpleEvent fOnCommandCAPABILITY;
	IMAPTSimpleEvent fOnCommandNOOP;
	IMAPTSimpleEvent fOnCommandLOGOUT;
	IMAPTBasicEvent fOnCommandAUTHENTICATE;
	IMAPTComplexEvent fOnCommandLOGIN;
	IMAPTBasicEvent fOnCommandSELECT;
	IMAPTBasicEvent fOnCommandEXAMINE;
	IMAPTBasicEvent fOnCommandCREATE;
	IMAPTBasicEvent fOnCommandDELETE;
	IMAPTComplexEvent fOnCommandRENAME;
	IMAPTBasicEvent fOnCommandSUBSCRIBE;
	IMAPTBasicEvent fOnCommandUNSUBSCRIBE;
	IMAPTComplexEvent fOnCommandLIST;
	IMAPTComplexEvent fOnCommandLSUB;
	IMAPTComplexEvent fOnCommandSTATUS;
	IMAPTComplexEvent fOnCommandAPPEND;
	IMAPTSimpleEvent fOnCommandCHECK;
	IMAPTSimpleEvent fOnCommandCLOSE;
	IMAPTSimpleEvent fOnCommandEXPUNGE;
	IMAPTComplexEvent fOnCommandSEARCH;
	IMAPTComplexEvent fOnCommandFETCH;
	IMAPTComplexEvent fOnCommandSTORE;
	IMAPTComplexEvent fOnCommandCOPY;
	IMAPTComplexEvent fOnCommandUID;
	IMAPTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandCAPABILITY(IMAPTSimpleEvent value);
	void __fastcall SetOnCommandNOOP(IMAPTSimpleEvent value);
	void __fastcall SetOnCommandLOGOUT(IMAPTSimpleEvent value);
	void __fastcall SetOnCommandAUTHENTICATE(IMAPTBasicEvent value);
	void __fastcall SetOnCommandLOGIN(IMAPTComplexEvent value);
	void __fastcall SetOnCommandSELECT(IMAPTBasicEvent value);
	void __fastcall SetOnCommandEXAMINE(IMAPTBasicEvent value);
	void __fastcall SetOnCommandCREATE(IMAPTBasicEvent value);
	void __fastcall SetOnCommandDELETE(IMAPTBasicEvent value);
	void __fastcall SetOnCommandRENAME(IMAPTComplexEvent value);
	void __fastcall SetOnCommandSUBSCRIBE(IMAPTBasicEvent value);
	void __fastcall SetOnCommandUNSUBSCRIBE(IMAPTBasicEvent value);
	void __fastcall SetOnCommandLIST(IMAPTComplexEvent value);
	void __fastcall SetOnCommandLSUB(IMAPTComplexEvent value);
	void __fastcall SetOnCommandSTATUS(IMAPTComplexEvent value);
	void __fastcall SetOnCommandAPPEND(IMAPTComplexEvent value);
	void __fastcall SetOnCommandCHECK(IMAPTSimpleEvent value);
	void __fastcall SetOnCommandCLOSE(IMAPTSimpleEvent value);
	void __fastcall SetOnCommandEXPUNGE(IMAPTSimpleEvent value);
	void __fastcall SetOnCommandSEARCH(IMAPTComplexEvent value);
	void __fastcall SetOnCommandFETCH(IMAPTComplexEvent value);
	void __fastcall SetOnCommandSTORE(IMAPTComplexEvent value);
	void __fastcall SetOnCommandCOPY(IMAPTComplexEvent value);
	void __fastcall SetOnCommandUID(IMAPTComplexEvent value);
	
public:
	__fastcall virtual TDXIMAP4ServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXIMAP4ServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddSimpleEvent(AnsiString Command, IMAPTSimpleEvent EventProc);
	void __fastcall AddBasicEvent(AnsiString Command, IMAPTBasicEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, IMAPTComplexEvent EventProc);
	
__published:
	__property IMAPTSimpleEvent OnCommandCAPABILITY = {read=fOnCommandCAPABILITY, write=SetOnCommandCAPABILITY
		};
	__property IMAPTSimpleEvent OnCommandNOOP = {read=fOnCommandNOOP, write=SetOnCommandNOOP};
	__property IMAPTSimpleEvent OnCommandLOGOUT = {read=fOnCommandLOGOUT, write=SetOnCommandLOGOUT};
	__property IMAPTBasicEvent OnCommandAUTHENTICATE = {read=fOnCommandAUTHENTICATE, write=SetOnCommandAUTHENTICATE
		};
	__property IMAPTComplexEvent OnCommandLOGIN = {read=fOnCommandLOGIN, write=SetOnCommandLOGIN};
	__property IMAPTBasicEvent OnCommandSELECT = {read=fOnCommandSELECT, write=SetOnCommandSELECT};
	__property IMAPTBasicEvent OnCommandEXAMINE = {read=fOnCommandEXAMINE, write=SetOnCommandEXAMINE};
	__property IMAPTBasicEvent OnCommandCREATE = {read=fOnCommandCREATE, write=SetOnCommandCREATE};
	__property IMAPTBasicEvent OnCommandDELETE = {read=fOnCommandDELETE, write=SetOnCommandDELETE};
	__property IMAPTComplexEvent OnCommandRENAME = {read=fOnCommandRENAME, write=SetOnCommandRENAME};
	__property IMAPTBasicEvent OnCommandSUBSCRIBE = {read=fOnCommandSUBSCRIBE, write=SetOnCommandSUBSCRIBE
		};
	__property IMAPTBasicEvent OnCommandUNSUBSCRIBE = {read=fOnCommandUNSUBSCRIBE, write=SetOnCommandUNSUBSCRIBE
		};
	__property IMAPTComplexEvent OnCommandLIST = {read=fOnCommandLIST, write=SetOnCommandLIST};
	__property IMAPTComplexEvent OnCommandLSUB = {read=fOnCommandLSUB, write=SetOnCommandLSUB};
	__property IMAPTComplexEvent OnCommandSTATUS = {read=fOnCommandSTATUS, write=SetOnCommandSTATUS};
	__property IMAPTComplexEvent OnCommandAPPEND = {read=fOnCommandAPPEND, write=SetOnCommandAPPEND};
	__property IMAPTSimpleEvent OnCommandCHECK = {read=fOnCommandCHECK, write=SetOnCommandCHECK};
	__property IMAPTSimpleEvent OnCommandCLOSE = {read=fOnCommandCLOSE, write=SetOnCommandCLOSE};
	__property IMAPTSimpleEvent OnCommandEXPUNGE = {read=fOnCommandEXPUNGE, write=SetOnCommandEXPUNGE};
		
	__property IMAPTComplexEvent OnCommandSEARCH = {read=fOnCommandSEARCH, write=SetOnCommandSEARCH};
	__property IMAPTComplexEvent OnCommandFETCH = {read=fOnCommandFETCH, write=SetOnCommandFETCH};
	__property IMAPTComplexEvent OnCommandSTORE = {read=fOnCommandSTORE, write=SetOnCommandSTORE};
	__property IMAPTComplexEvent OnCommandCOPY = {read=fOnCommandCOPY, write=SetOnCommandCOPY};
	__property IMAPTComplexEvent OnCommandUID = {read=fOnCommandUID, write=SetOnCommandUID};
	__property IMAPTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dximap4servercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dximap4servercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXIMAP4ServerCore
