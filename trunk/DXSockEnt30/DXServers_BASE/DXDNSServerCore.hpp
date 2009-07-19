// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXDNSServerCore.pas' rev: 5.00

#ifndef DXDNSServerCoreHPP
#define DXDNSServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <DXString.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxdnsservercore
{
//-- type declarations -------------------------------------------------------
struct TDXDNSQueryPacket;
typedef TDXDNSQueryPacket *PDXDNSQueryPacket;

struct TDXDNSQueryPacket
{
	Word ID;
	bool QR;
	Byte OpCode;
	bool AA;
	bool TC;
	bool RD;
	bool RA;
	Byte Z;
	Byte RCode;
	Word QDCount;
	Word ANCount;
	Word NSCount;
	Word ARCount;
	AnsiString Query;
	Word QueryType;
	Word QueryClass;
} ;

typedef void __fastcall (__closure *DNSTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread, PDXDNSQueryPacket 
	QueryPacket);

typedef void __fastcall (__closure *DNSTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, PDXDNSQueryPacket 
	QueryPacket, bool &Handled);

class DELPHICLASS TDXDNSServerCore;
class PASCALIMPLEMENTATION TDXDNSServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	DNSTSimpleEvent fOnCommandA;
	DNSTSimpleEvent fOnCommandNS;
	DNSTSimpleEvent fOnCommandMD;
	DNSTSimpleEvent fOnCommandMF;
	DNSTSimpleEvent fOnCommandCNAME;
	DNSTSimpleEvent fOnCommandSOA;
	DNSTSimpleEvent fOnCommandMB;
	DNSTSimpleEvent fOnCommandMG;
	DNSTSimpleEvent fOnCommandMR;
	DNSTSimpleEvent fOnCommandNULL;
	DNSTSimpleEvent fOnCommandWKS;
	DNSTSimpleEvent fOnCommandPTR;
	DNSTSimpleEvent fOnCommandHINFO;
	DNSTSimpleEvent fOnCommandMINFO;
	DNSTSimpleEvent fOnCommandMX;
	DNSTSimpleEvent fOnCommandTXT;
	DNSTSimpleEvent fOnCommandRP;
	DNSTSimpleEvent fOnCommandAFSDB;
	DNSTSimpleEvent fOnCommandX25;
	DNSTSimpleEvent fOnCommandISDN;
	DNSTSimpleEvent fOnCommandRT;
	DNSTSimpleEvent fOnCommandOSINSAP;
	DNSTSimpleEvent fOnCommandAXFR;
	DNSTSimpleEvent fOnCommandMAILB;
	DNSTSimpleEvent fOnCommandMAILA;
	DNSTSimpleEvent fOnCommandALL;
	DNSTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandA(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandNS(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMD(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMF(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandCNAME(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandSOA(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMB(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMG(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMR(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandNULL(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandWKS(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandPTR(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandHINFO(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMINFO(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMX(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandTXT(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandRP(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandAFSDB(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandX25(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandISDN(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandRT(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandOSINSAP(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandAXFR(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMAILB(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandMAILA(DNSTSimpleEvent Value);
	void __fastcall SetOnCommandALL(DNSTSimpleEvent Value);
	
public:
	__fastcall virtual TDXDNSServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXDNSServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddSimpleEvent(short Command, DNSTSimpleEvent EventProc);
	
__published:
	__property DNSTSimpleEvent OnCommandA = {read=fOnCommandA, write=SetOnCommandA};
	__property DNSTSimpleEvent OnCommandNS = {read=fOnCommandNS, write=SetOnCommandNS};
	__property DNSTSimpleEvent OnCommandMD = {read=fOnCommandMD, write=SetOnCommandMD};
	__property DNSTSimpleEvent OnCommandMF = {read=fOnCommandMF, write=SetOnCommandMF};
	__property DNSTSimpleEvent OnCommandCNAME = {read=fOnCommandCNAME, write=SetOnCommandCNAME};
	__property DNSTSimpleEvent OnCommandSOA = {read=fOnCommandSOA, write=SetOnCommandSOA};
	__property DNSTSimpleEvent OnCommandMB = {read=fOnCommandMB, write=SetOnCommandMB};
	__property DNSTSimpleEvent OnCommandMG = {read=fOnCommandMG, write=SetOnCommandMG};
	__property DNSTSimpleEvent OnCommandMR = {read=fOnCommandMR, write=SetOnCommandMR};
	__property DNSTSimpleEvent OnCommandNULL = {read=fOnCommandNULL, write=SetOnCommandNULL};
	__property DNSTSimpleEvent OnCommandWKS = {read=fOnCommandWKS, write=SetOnCommandWKS};
	__property DNSTSimpleEvent OnCommandPTR = {read=fOnCommandPTR, write=SetOnCommandPTR};
	__property DNSTSimpleEvent OnCommandHINFO = {read=fOnCommandHINFO, write=SetOnCommandHINFO};
	__property DNSTSimpleEvent OnCommandMINFO = {read=fOnCommandMINFO, write=SetOnCommandMINFO};
	__property DNSTSimpleEvent OnCommandMX = {read=fOnCommandMX, write=SetOnCommandMX};
	__property DNSTSimpleEvent OnCommandTXT = {read=fOnCommandTXT, write=SetOnCommandTXT};
	__property DNSTSimpleEvent OnCommandRP = {read=fOnCommandRP, write=SetOnCommandRP};
	__property DNSTSimpleEvent OnCommandAFSDB = {read=fOnCommandAFSDB, write=SetOnCommandAFSDB};
	__property DNSTSimpleEvent OnCommandX25 = {read=fOnCommandX25, write=SetOnCommandX25};
	__property DNSTSimpleEvent OnCommandISDN = {read=fOnCommandISDN, write=SetOnCommandISDN};
	__property DNSTSimpleEvent OnCommandRT = {read=fOnCommandRT, write=SetOnCommandRT};
	__property DNSTSimpleEvent OnCommandOSINSAP = {read=fOnCommandOSINSAP, write=SetOnCommandOSINSAP};
	__property DNSTSimpleEvent OnCommandAXFR = {read=fOnCommandAXFR, write=SetOnCommandAXFR};
	__property DNSTSimpleEvent OnCommandMAILB = {read=fOnCommandMAILB, write=SetOnCommandMAILB};
	__property DNSTSimpleEvent OnCommandMAILA = {read=fOnCommandMAILA, write=SetOnCommandMAILA};
	__property DNSTSimpleEvent OnCommandALL = {read=fOnCommandALL, write=SetOnCommandALL};
	__property DNSTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxdnsservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxdnsservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXDNSServerCore
