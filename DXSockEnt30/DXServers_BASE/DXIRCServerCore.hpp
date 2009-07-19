// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXIRCServerCore.pas' rev: 5.00

#ifndef DXIRCServerCoreHPP
#define DXIRCServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxircservercore
{
//-- type declarations -------------------------------------------------------
struct DXIRCServerCore__1
{
	int Code;
	AnsiString Msg;
} ;

typedef DXIRCServerCore__1 DXIRCServerCore__2[45];

typedef void __fastcall (__closure *IRCTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm1);

typedef void __fastcall (__closure *IRCTMultipleEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1);

typedef void __fastcall (__closure *IRCTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *IRCTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *IRCTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parms, bool &Handled);

class DELPHICLASS TDXIRCServerCore;
class PASCALIMPLEMENTATION TDXIRCServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	IRCTBasicEvent fOnCommandPASS;
	IRCTMultipleEvent fOnCommandNICK;
	IRCTMultipleEvent fOnCommandUSER;
	IRCTMultipleEvent fOnCommandSERVER;
	IRCTComplexEvent fOnCommandOPER;
	IRCTBasicEvent fOnCommandQUIT;
	IRCTComplexEvent fOnCommandSQUIT;
	IRCTMultipleEvent fOnCommandJOIN;
	IRCTBasicEvent fOnCommandPART;
	IRCTMultipleEvent fOnCommandMODE;
	IRCTComplexEvent fOnCommandTOPIC;
	IRCTBasicEvent fOnCommandNAMES;
	IRCTComplexEvent fOnCommandLIST;
	IRCTComplexEvent fOnCommandINVITE;
	IRCTMultipleEvent fOnCommandKICK;
	IRCTBasicEvent fOnCommandVERSION;
	IRCTComplexEvent fOnCommandSTATS;
	IRCTComplexEvent fOnCommandLINKS;
	IRCTBasicEvent fOnCommandTIME;
	IRCTMultipleEvent fOnCommandCONNECT;
	IRCTBasicEvent fOnCommandTRACE;
	IRCTBasicEvent fOnCommandADMIN;
	IRCTBasicEvent fOnCommandINFO;
	IRCTComplexEvent fOnCommandPRIVMSG;
	IRCTComplexEvent fOnCommandNOTICE;
	IRCTComplexEvent fOnCommandWHO;
	IRCTComplexEvent fOnCommandWHOIS;
	IRCTMultipleEvent fOnCommandWHOWAS;
	IRCTComplexEvent fOnCommandKILL;
	IRCTComplexEvent fOnCommandPING;
	IRCTComplexEvent fOnCommandPONG;
	IRCTBasicEvent fOnCommandAWAY;
	IRCTSimpleEvent fOnCommandREHASH;
	IRCTSimpleEvent fOnCommandRESTART;
	IRCTComplexEvent fOnCommandSUMMON;
	IRCTBasicEvent fOnCommandUSERS;
	IRCTBasicEvent fOnCommandWALLOPS;
	IRCTComplexEvent fOnCommandUSERHOST;
	IRCTComplexEvent fOnCommandISON;
	IRCTMultipleEvent fOnCommandERROR;
	IRCTOtherEvent fOnCommandOther;
	IRCTSimpleEvent fOnCommandTimeout;
	AnsiString fServerName;
	
protected:
	void __fastcall SetOnCommandPASS(IRCTBasicEvent Value);
	void __fastcall SetOnCommandNICK(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandUSER(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandSERVER(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandOPER(IRCTComplexEvent Value);
	void __fastcall SetOnCommandQUIT(IRCTBasicEvent Value);
	void __fastcall SetOnCommandSQUIT(IRCTComplexEvent Value);
	void __fastcall SetOnCommandJOIN(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandPART(IRCTBasicEvent Value);
	void __fastcall SetOnCommandMODE(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandTOPIC(IRCTComplexEvent Value);
	void __fastcall SetOnCommandNAMES(IRCTBasicEvent Value);
	void __fastcall SetOnCommandLIST(IRCTComplexEvent Value);
	void __fastcall SetOnCommandINVITE(IRCTComplexEvent Value);
	void __fastcall SetOnCommandKICK(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandVERSION(IRCTBasicEvent Value);
	void __fastcall SetOnCommandSTATS(IRCTComplexEvent Value);
	void __fastcall SetOnCommandLINKS(IRCTComplexEvent Value);
	void __fastcall SetOnCommandTIME(IRCTBasicEvent Value);
	void __fastcall SetOnCommandCONNECT(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandTRACE(IRCTBasicEvent Value);
	void __fastcall SetOnCommandADMIN(IRCTBasicEvent Value);
	void __fastcall SetOnCommandINFO(IRCTBasicEvent Value);
	void __fastcall SetOnCommandPRIVMSG(IRCTComplexEvent Value);
	void __fastcall SetOnCommandNOTICE(IRCTComplexEvent Value);
	void __fastcall SetOnCommandWHO(IRCTComplexEvent Value);
	void __fastcall SetOnCommandWHOIS(IRCTComplexEvent Value);
	void __fastcall SetOnCommandWHOWAS(IRCTMultipleEvent Value);
	void __fastcall SetOnCommandKILL(IRCTComplexEvent Value);
	void __fastcall SetOnCommandPING(IRCTComplexEvent Value);
	void __fastcall SetOnCommandPONG(IRCTComplexEvent Value);
	void __fastcall SetOnCommandAWAY(IRCTBasicEvent Value);
	void __fastcall SetOnCommandREHASH(IRCTSimpleEvent Value);
	void __fastcall SetOnCommandRESTART(IRCTSimpleEvent Value);
	void __fastcall SetOnCommandSUMMON(IRCTComplexEvent Value);
	void __fastcall SetOnCommandUSERS(IRCTBasicEvent Value);
	void __fastcall SetOnCommandWALLOPS(IRCTBasicEvent Value);
	void __fastcall SetOnCommandUSERHOST(IRCTComplexEvent Value);
	void __fastcall SetOnCommandISON(IRCTComplexEvent Value);
	void __fastcall SetOnCommandERROR(IRCTMultipleEvent Value);
	
public:
	__fastcall virtual TDXIRCServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXIRCServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, IRCTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, IRCTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, IRCTComplexEvent EventProc);
	void __fastcall AddMultipleEvent(AnsiString Command, IRCTMultipleEvent EventProc);
	void __fastcall SayError(int ErrorCode, AnsiString OptionalText, Dxservercore::TDXClientThread* ClientThread
		);
	void __fastcall SayWelcome(AnsiString Nick, Classes::TStrings* Text, Dxservercore::TDXClientThread* 
		ClientThread);
	void __fastcall SayMOTD(AnsiString Nick, Classes::TStrings* Text, Dxservercore::TDXClientThread* ClientThread
		);
	void __fastcall SayNOTICE(AnsiString Nick, AnsiString Text, Dxservercore::TDXClientThread* ClientThread
		);
	void __fastcall SayChannelList(AnsiString Nick, Classes::TStrings* Text, Dxservercore::TDXClientThread* 
		ClientThread);
	
__published:
	__property IRCTBasicEvent OnCommandPASS = {read=fOnCommandPASS, write=SetOnCommandPASS};
	__property IRCTMultipleEvent OnCommandNICK = {read=fOnCommandNICK, write=SetOnCommandNICK};
	__property IRCTMultipleEvent OnCommandUSER = {read=fOnCommandUSER, write=SetOnCommandUSER};
	__property IRCTMultipleEvent OnCommandSERVER = {read=fOnCommandSERVER, write=SetOnCommandSERVER};
	__property IRCTComplexEvent OnCommandOPER = {read=fOnCommandOPER, write=SetOnCommandOPER};
	__property IRCTBasicEvent OnCommandQUIT = {read=fOnCommandQUIT, write=SetOnCommandQUIT};
	__property IRCTComplexEvent OnCommandSQUIT = {read=fOnCommandSQUIT, write=SetOnCommandSQUIT};
	__property IRCTMultipleEvent OnCommandJOIN = {read=fOnCommandJOIN, write=SetOnCommandJOIN};
	__property IRCTBasicEvent OnCommandPART = {read=fOnCommandPART, write=SetOnCommandPART};
	__property IRCTMultipleEvent OnCommandMODE = {read=fOnCommandMODE, write=SetOnCommandMODE};
	__property IRCTComplexEvent OnCommandTOPIC = {read=fOnCommandTOPIC, write=SetOnCommandTOPIC};
	__property IRCTBasicEvent OnCommandNAMES = {read=fOnCommandNAMES, write=SetOnCommandNAMES};
	__property IRCTComplexEvent OnCommandLIST = {read=fOnCommandLIST, write=SetOnCommandLIST};
	__property IRCTComplexEvent OnCommandINVITE = {read=fOnCommandINVITE, write=SetOnCommandINVITE};
	__property IRCTMultipleEvent OnCommandKICK = {read=fOnCommandKICK, write=SetOnCommandKICK};
	__property IRCTBasicEvent OnCommandVERSION = {read=fOnCommandVERSION, write=SetOnCommandVERSION};
	__property IRCTComplexEvent OnCommandSTATS = {read=fOnCommandSTATS, write=SetOnCommandSTATS};
	__property IRCTComplexEvent OnCommandLINKS = {read=fOnCommandLINKS, write=SetOnCommandLINKS};
	__property IRCTBasicEvent OnCommandTIME = {read=fOnCommandTIME, write=SetOnCommandTIME};
	__property IRCTMultipleEvent OnCommandCONNECT = {read=fOnCommandCONNECT, write=SetOnCommandCONNECT}
		;
	__property IRCTBasicEvent OnCommandTRACE = {read=fOnCommandTRACE, write=SetOnCommandTRACE};
	__property IRCTBasicEvent OnCommandADMIN = {read=fOnCommandADMIN, write=SetOnCommandADMIN};
	__property IRCTBasicEvent OnCommandINFO = {read=fOnCommandINFO, write=SetOnCommandINFO};
	__property IRCTComplexEvent OnCommandPRIVMSG = {read=fOnCommandPRIVMSG, write=SetOnCommandPRIVMSG};
		
	__property IRCTComplexEvent OnCommandNOTICE = {read=fOnCommandNOTICE, write=SetOnCommandNOTICE};
	__property IRCTComplexEvent OnCommandWHO = {read=fOnCommandWHO, write=SetOnCommandWHO};
	__property IRCTComplexEvent OnCommandWHOIS = {read=fOnCommandWHOIS, write=SetOnCommandWHOIS};
	__property IRCTMultipleEvent OnCommandWHOWAS = {read=fOnCommandWHOWAS, write=SetOnCommandWHOWAS};
	__property IRCTComplexEvent OnCommandKILL = {read=fOnCommandKILL, write=SetOnCommandKILL};
	__property IRCTComplexEvent OnCommandPING = {read=fOnCommandPING, write=SetOnCommandPING};
	__property IRCTComplexEvent OnCommandPONG = {read=fOnCommandPONG, write=SetOnCommandPONG};
	__property IRCTBasicEvent OnCommandAWAY = {read=fOnCommandAWAY, write=SetOnCommandAWAY};
	__property IRCTSimpleEvent OnCommandREHASH = {read=fOnCommandREHASH, write=SetOnCommandREHASH};
	__property IRCTSimpleEvent OnCommandRESTART = {read=fOnCommandRESTART, write=SetOnCommandRESTART};
	__property IRCTComplexEvent OnCommandSUMMON = {read=fOnCommandSUMMON, write=SetOnCommandSUMMON};
	__property IRCTBasicEvent OnCommandUSERS = {read=fOnCommandUSERS, write=SetOnCommandUSERS};
	__property IRCTBasicEvent OnCommandWALLOPS = {read=fOnCommandWALLOPS, write=SetOnCommandWALLOPS};
	__property IRCTComplexEvent OnCommandUSERHOST = {read=fOnCommandUSERHOST, write=SetOnCommandUSERHOST
		};
	__property IRCTComplexEvent OnCommandISON = {read=fOnCommandISON, write=SetOnCommandISON};
	__property IRCTMultipleEvent OnCommandERROR = {read=fOnCommandERROR, write=SetOnCommandERROR};
	__property IRCTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
	__property AnsiString ServerName = {read=fServerName, write=fServerName};
	__property IRCTSimpleEvent OnReadTimeout = {read=fOnCommandTimeout, write=fOnCommandTimeout};
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint MaxStatusCodes = 0x2d;
static const Word ERR_NOSUCHNICK = 0x191;
static const Word ERR_NOSUCHSERVER = 0x192;
static const Word ERR_NOSUCHCHANNEL = 0x193;
static const Word ERR_CANNOTSENDTOCHAN = 0x194;
static const Word ERR_TOOMANYCHANNELS = 0x195;
static const Word ERR_WASNOSUCHNICK = 0x196;
static const Word ERR_TOOMANYTARGETS = 0x197;
static const Word ERR_NOORIGIN = 0x199;
static const Word ERR_NORECIPIENT = 0x19b;
static const Word ERR_NOTEXTTOSEND = 0x19c;
static const Word ERR_NOTOPLEVEL = 0x19d;
static const Word ERR_WILDTOPLEVEL = 0x19e;
static const Word ERR_UNKNOWNCOMMAND = 0x1a5;
static const Word ERR_NOMOTD = 0x1a6;
static const Word ERR_NOADMININFO = 0x1a7;
static const Word ERR_FILEERROR = 0x1a8;
static const Word ERR_NONICKNAMEGIVEN = 0x1af;
static const Word ERR_ERRONEUSNICKNAME = 0x1b0;
static const Word ERR_NICKNAMEINUSE = 0x1b1;
static const Word ERR_NICKCOLLISION = 0x1b4;
static const Word ERR_USERNOTINCHANNEL = 0x1b9;
static const Word ERR_NOTONCHANNEL = 0x1ba;
static const Word ERR_USERONCHANNEL = 0x1bb;
static const Word ERR_NOLOGIN = 0x1bc;
static const Word ERR_SUMMONDISABLED = 0x1bd;
static const Word ERR_USERSDISABLED = 0x1be;
static const Word ERR_NOTREGISTERED = 0x1c3;
static const Word ERR_NEEDMOREPARAMS = 0x1cd;
static const Word ERR_ALREADYREGISTRED = 0x1ce;
static const Word ERR_NOPERMFORHOST = 0x1cf;
static const Word ERR_PASSWDMISMATCH = 0x1d0;
static const Word ERR_YOUREBANNEDCREEP = 0x1d1;
static const Word ERR_KEYSET = 0x1d3;
static const Word ERR_CHANNELISFULL = 0x1d7;
static const Word ERR_UNKNOWNMODE = 0x1d8;
static const Word ERR_INVITEONLYCHAN = 0x1d9;
static const Word ERR_BANNEDFROMCHAN = 0x1da;
static const Word ERR_BADCHANNELKEY = 0x1db;
static const Word ERR_BADCHANMASK = 0x1dc;
static const Word ERR_NOPRIVILEGES = 0x1e1;
static const Word ERR_CHANOPRIVSNEEDED = 0x1e2;
static const Word ERR_CANTKILLSERVER = 0x1e3;
static const Word ERR_NOOPERHOST = 0x1eb;
static const Word ERR_UMODEUNKNOWNFLAG = 0x1f5;
static const Word ERR_USERSDONTMATCH = 0x1f6;
extern PACKAGE DXIRCServerCore__1 StatusCodes[45];

}	/* namespace Dxircservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxircservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXIRCServerCore
