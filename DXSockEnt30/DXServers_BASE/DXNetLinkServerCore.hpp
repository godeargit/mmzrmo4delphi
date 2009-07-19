// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXNetLinkServerCore.pas' rev: 5.00

#ifndef DXNetLinkServerCoreHPP
#define DXNetLinkServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DXSock.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxnetlinkservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *NetLinkTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread
	);

typedef void __fastcall (__closure *NetLinkTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm);

typedef void __fastcall (__closure *NetLinkTComplexEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *NetLinkTRealComplexEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1, AnsiString Parm2, AnsiString Parm3, AnsiString Parm4);

typedef void __fastcall (__closure *NetLinkTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXNetLinkServerCore;
class PASCALIMPLEMENTATION TDXNetLinkServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	NetLinkTSimpleEvent fOnCommandTimeout;
	NetLinkTRealComplexEvent fOnCommandALERT;
	NetLinkTComplexEvent fOnCommandLOGIN;
	NetLinkTSimpleEvent fOnCommandPING;
	NetLinkTSimpleEvent fOnCommandPONG;
	NetLinkTComplexEvent fOnCommandSENDIM;
	NetLinkTBasicEvent fOnCommandMYSTATUS;
	NetLinkTBasicEvent fOnCommandADDCONTACT;
	NetLinkTBasicEvent fOnCommandDELCONTACT;
	NetLinkTBasicEvent fOnCommandADDSERVICE;
	NetLinkTBasicEvent fOnCommandDELSERVICE;
	NetLinkTSimpleEvent fOnCommandSERVERSTATUS;
	NetLinkTSimpleEvent fOnCommandWATCHLIST;
	NetLinkTSimpleEvent fOnCommandALLSTATUS;
	NetLinkTSimpleEvent fOnCommandSERVICES;
	NetLinkTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandSENDIM(NetLinkTComplexEvent value);
	void __fastcall SetOnCommandMYSTATUS(NetLinkTBasicEvent value);
	void __fastcall SetOnCommandADDCONTACT(NetLinkTBasicEvent value);
	void __fastcall SetOnCommandDELCONTACT(NetLinkTBasicEvent value);
	void __fastcall SetOnCommandADDSERVICE(NetLinkTBasicEvent value);
	void __fastcall SetOnCommandDELSERVICE(NetLinkTBasicEvent value);
	void __fastcall SetOnCommandSERVERSTATUS(NetLinkTSimpleEvent value);
	void __fastcall SetOnCommandWATCHLIST(NetLinkTSimpleEvent value);
	void __fastcall SetOnCommandALLSTATUS(NetLinkTSimpleEvent value);
	void __fastcall SetOnCommandSERVICES(NetLinkTSimpleEvent value);
	void __fastcall SetOnCommandPING(NetLinkTSimpleEvent value);
	void __fastcall SetOnCommandPONG(NetLinkTSimpleEvent value);
	void __fastcall SetOnCommandLOGIN(NetLinkTComplexEvent value);
	void __fastcall SetOnCommandALERT(NetLinkTRealComplexEvent value);
	
public:
	__fastcall virtual TDXNetLinkServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXNetLinkServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, Classes::TStream* Header, Classes::TStream* 
		MOTD);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, Classes::TStream* Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, NetLinkTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, NetLinkTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, NetLinkTComplexEvent EventProc);
	void __fastcall AddRealComplexEvent(AnsiString Command, NetLinkTRealComplexEvent EventProc);
	
__published:
	__property NetLinkTComplexEvent OnCommandLOGIN = {read=fOnCommandLOGIN, write=SetOnCommandLOGIN};
	__property NetLinkTRealComplexEvent OnCommandALERT = {read=fOnCommandALERT, write=SetOnCommandALERT
		};
	__property NetLinkTComplexEvent OnCommandSENDIM = {read=fOnCommandSENDIM, write=SetOnCommandSENDIM}
		;
	__property NetLinkTBasicEvent OnCommandMYSTATUS = {read=fOnCommandMYSTATUS, write=SetOnCommandMYSTATUS
		};
	__property NetLinkTBasicEvent OnCommandADDCONTACT = {read=fOnCommandADDCONTACT, write=SetOnCommandADDCONTACT
		};
	__property NetLinkTBasicEvent OnCommandDELCONTACT = {read=fOnCommandDELCONTACT, write=SetOnCommandDELCONTACT
		};
	__property NetLinkTBasicEvent OnCommandADDSERVICE = {read=fOnCommandADDSERVICE, write=SetOnCommandADDSERVICE
		};
	__property NetLinkTBasicEvent OnCommandDELSERVICE = {read=fOnCommandDELSERVICE, write=SetOnCommandDELSERVICE
		};
	__property NetLinkTSimpleEvent OnCommandSERVERSTATUS = {read=fOnCommandSERVERSTATUS, write=SetOnCommandSERVERSTATUS
		};
	__property NetLinkTSimpleEvent OnCommandWATCHLIST = {read=fOnCommandWATCHLIST, write=SetOnCommandWATCHLIST
		};
	__property NetLinkTSimpleEvent OnCommandALLSTATUS = {read=fOnCommandALLSTATUS, write=SetOnCommandALLSTATUS
		};
	__property NetLinkTSimpleEvent OnCommandSERVICES = {read=fOnCommandSERVICES, write=SetOnCommandSERVICES
		};
	__property NetLinkTSimpleEvent OnCommandPING = {read=fOnCommandPING, write=SetOnCommandPING};
	__property NetLinkTSimpleEvent OnCommandPONG = {read=fOnCommandPONG, write=SetOnCommandPONG};
	__property NetLinkTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
	__property NetLinkTSimpleEvent OnCommandTimeout = {read=fOnCommandTimeout, write=fOnCommandTimeout}
		;
};


//-- var, const, procedure ---------------------------------------------------
static const char NetlinkStrDelim = '\xff';

}	/* namespace Dxnetlinkservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxnetlinkservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXNetLinkServerCore
