// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXHTTPServerCore.pas' rev: 5.00

#ifndef DXHTTPServerCoreHPP
#define DXHTTPServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXHTTPHeaderTools.hpp>	// Pascal unit
#include <DXISAPI.hpp>	// Pascal unit
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DXSocket.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxhttpservercore
{
//-- type declarations -------------------------------------------------------
struct DXHTTPServerCore__1
{
	int Code;
	AnsiString Msg;
} ;

typedef DXHTTPServerCore__1 DXHTTPServerCore__2[41];

typedef void __fastcall (__closure *HTTPTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, Dxhttpheadertools::PHeaderInfo 
	HeaderInfo, bool &EnableKeepAlive);

typedef void __fastcall (__closure *HTTPTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, Dxhttpheadertools::PHeaderInfo 
	HeaderInfo, bool &Handled);

class DELPHICLASS TDXHTTPServerCore;
class PASCALIMPLEMENTATION TDXHTTPServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	HTTPTBasicEvent fOnCommandGET;
	HTTPTBasicEvent fOnCommandPOST;
	HTTPTBasicEvent fOnCommandHEAD;
	HTTPTBasicEvent fOnCommandCHECKOUT;
	HTTPTBasicEvent fOnCommandSHOWMETHOD;
	HTTPTBasicEvent fOnCommandPUT;
	HTTPTBasicEvent fOnCommandDELETE;
	HTTPTBasicEvent fOnCommandLINK;
	HTTPTBasicEvent fOnCommandUNLINK;
	HTTPTBasicEvent fOnCommandCHECKIN;
	HTTPTBasicEvent fOnCommandTEXTSEARCH;
	HTTPTBasicEvent fOnCommandSPACEJUMP;
	HTTPTBasicEvent fOnCommandSEARCH;
	HTTPTBasicEvent fOnCommandOPTIONS;
	HTTPTBasicEvent fOnCommandTRACE;
	HTTPTBasicEvent fOnCommandCONNECT;
	HTTPTBasicEvent fOnCommandPATCH;
	HTTPTOtherEvent fOnCommandOther;
	bool fSupportKeepAlive;
	Dxisapi::TDXISAPI* fDXISAPI;
	
protected:
	void __fastcall SetOnCommandGET(HTTPTBasicEvent value);
	void __fastcall SetOnCommandPOST(HTTPTBasicEvent value);
	void __fastcall SetOnCommandHEAD(HTTPTBasicEvent value);
	void __fastcall SetOnCommandCHECKOUT(HTTPTBasicEvent value);
	void __fastcall SetOnCommandSHOWMETHOD(HTTPTBasicEvent value);
	void __fastcall SetOnCommandPUT(HTTPTBasicEvent value);
	void __fastcall SetOnCommandDELETE(HTTPTBasicEvent value);
	void __fastcall SetOnCommandLINK(HTTPTBasicEvent value);
	void __fastcall SetOnCommandUNLINK(HTTPTBasicEvent value);
	void __fastcall SetOnCommandCHECKIN(HTTPTBasicEvent value);
	void __fastcall SetOnCommandTEXTSEARCH(HTTPTBasicEvent value);
	void __fastcall SetOnCommandSPACEJUMP(HTTPTBasicEvent value);
	void __fastcall SetOnCommandSEARCH(HTTPTBasicEvent value);
	void __fastcall SetOnCommandOPTIONS(HTTPTBasicEvent value);
	void __fastcall SetOnCommandTRACE(HTTPTBasicEvent value);
	void __fastcall SetOnCommandCONNECT(HTTPTBasicEvent value);
	void __fastcall SetOnCommandPATCH(HTTPTBasicEvent value);
	
public:
	__fastcall virtual TDXHTTPServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXHTTPServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, HTTPTBasicEvent EventProc);
	AnsiString __fastcall HeaderText(int StatusCode);
	virtual void __fastcall Start(void);
	virtual void __fastcall Stop(void);
	
__published:
	__property bool SupportKeepAlive = {read=fSupportKeepAlive, write=fSupportKeepAlive, nodefault};
	__property HTTPTBasicEvent OnCommandGET = {read=fOnCommandGET, write=SetOnCommandGET};
	__property HTTPTBasicEvent OnCommandPOST = {read=fOnCommandPOST, write=SetOnCommandPOST};
	__property HTTPTBasicEvent OnCommandHEAD = {read=fOnCommandHEAD, write=SetOnCommandHEAD};
	__property HTTPTBasicEvent OnCommandCHECKOUT = {read=fOnCommandCHECKOUT, write=SetOnCommandCHECKOUT
		};
	__property HTTPTBasicEvent OnCommandSHOWMETHOD = {read=fOnCommandSHOWMETHOD, write=SetOnCommandSHOWMETHOD
		};
	__property HTTPTBasicEvent OnCommandPUT = {read=fOnCommandPUT, write=SetOnCommandPUT};
	__property HTTPTBasicEvent OnCommandDELETE = {read=fOnCommandDELETE, write=SetOnCommandDELETE};
	__property HTTPTBasicEvent OnCommandLINK = {read=fOnCommandLINK, write=SetOnCommandLINK};
	__property HTTPTBasicEvent OnCommandUNLINK = {read=fOnCommandUNLINK, write=SetOnCommandUNLINK};
	__property HTTPTBasicEvent OnCommandCHECKIN = {read=fOnCommandCHECKIN, write=SetOnCommandCHECKIN};
	__property HTTPTBasicEvent OnCommandTEXTSEARCH = {read=fOnCommandTEXTSEARCH, write=SetOnCommandTEXTSEARCH
		};
	__property HTTPTBasicEvent OnCommandSPACEJUMP = {read=fOnCommandSPACEJUMP, write=SetOnCommandSPACEJUMP
		};
	__property HTTPTBasicEvent OnCommandSEARCH = {read=fOnCommandSEARCH, write=SetOnCommandSEARCH};
	__property HTTPTBasicEvent OnCommandOPTIONS = {read=fOnCommandOPTIONS, write=SetOnCommandOPTIONS};
	__property HTTPTBasicEvent OnCommandTRACE = {read=fOnCommandTRACE, write=SetOnCommandTRACE};
	__property HTTPTBasicEvent OnCommandCONNECT = {read=fOnCommandCONNECT, write=SetOnCommandCONNECT};
	__property HTTPTBasicEvent OnCommandPATCH = {read=fOnCommandPATCH, write=SetOnCommandPATCH};
	__property HTTPTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
	__property Dxisapi::TDXISAPI* ISAPIServer = {read=fDXISAPI, write=fDXISAPI};
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint MaxStatusCodes = 0x28;
extern PACKAGE DXHTTPServerCore__1 StatusCodes[41];

}	/* namespace Dxhttpservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxhttpservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXHTTPServerCore
