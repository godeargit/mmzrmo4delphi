// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXTelnetServerCore.pas' rev: 5.00

#ifndef DXTelnetServerCoreHPP
#define DXTelnetServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxtelnetservercore
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXTelnetServerCore;
class PASCALIMPLEMENTATION TDXTelnetServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
public:
	__fastcall virtual TDXTelnetServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXTelnetServerCore(void);
	void __fastcall DoNegotiation(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SayWill(Dxservercore::TDXClientThread* ClientThread, char Option);
	void __fastcall SayWont(Dxservercore::TDXClientThread* ClientThread, char Option);
	void __fastcall SayDo(Dxservercore::TDXClientThread* ClientThread, char Option);
	void __fastcall SayDont(Dxservercore::TDXClientThread* ClientThread, char Option);
};


//-- var, const, procedure ---------------------------------------------------
static const char TELNET_IAC = '\xff';
static const char TELNET_DONT = '\xfe';
static const char TELNET_DO = '\xfd';
static const char TELNET_WONT = '\xfc';
static const char TELNET_WILL = '\xfb';
static const char TELNET_SB = '\xfa';
static const char TELNET_GA = '\xf9';
static const char TELNET_EL = '\xf8';
static const char TELNET_EC = '\xf7';
static const char TELNET_AYT = '\xf6';
static const char TELNET_AO = '\xf5';
static const char TELNET_IP = '\xf4';
static const char TELNET_BRK = '\xf3';
static const char TELNET_DM = '\xf2';
static const char TELNET_NOP = '\xf1';
static const char TELNET_SE = '\xf0';
static const char TELNET_EOR = '\xef';
static const char TELNET_ABORT = '\xee';
static const char TELNET_SUSP = '\xed';
static const char TELNET_EOF = '\xec';
static const char TELNETOPT_BINARY = '\x0';
static const char TELNETOPT_ECHO = '\x1';
static const char TELNETOPT_SUPGA = '\x3';
static const char TELNETOPT_TERM = '\x18';
static const char TELNETOPT_SPEED = '\x20';

}	/* namespace Dxtelnetservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxtelnetservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXTelnetServerCore
