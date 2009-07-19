// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXDictionaryServerCore.pas' rev: 5.00

#ifndef DXDictionaryServerCoreHPP
#define DXDictionaryServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxdictionaryservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *DictTBasicEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *DictTOptionEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm);

typedef void __fastcall (__closure *DictTDefineEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Database, AnsiString WordToFind);

typedef void __fastcall (__closure *DictTMatchEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Database, AnsiString Strategy, AnsiString WordToFind);

typedef void __fastcall (__closure *DictTShowEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command);

typedef void __fastcall (__closure *DictTAuthEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Username, AnsiString authstring);

typedef void __fastcall (__closure *DictTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool Handled);

class DELPHICLASS TDXDictionaryServerCore;
class PASCALIMPLEMENTATION TDXDictionaryServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	DictTAuthEvent fOnCommandAUTH;
	DictTShowEvent fOnCommandCLIENT;
	DictTDefineEvent fOnCommandDEFINE;
	DictTBasicEvent fOnCommandHELP;
	DictTMatchEvent fOnCommandMATCH;
	DictTOptionEvent fOnCommandOPTION;
	DictTBasicEvent fOnCommandQUIT;
	DictTAuthEvent fOnCommandSASLAUTH;
	DictTShowEvent fOnCommandSHOW;
	DictTBasicEvent fOnCommandSTAT;
	DictTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandAUTH(DictTAuthEvent value);
	void __fastcall SetOnCommandCLIENT(DictTShowEvent value);
	void __fastcall SetOnCommandDEFINE(DictTDefineEvent value);
	void __fastcall SetOnCommandHELP(DictTBasicEvent value);
	void __fastcall SetOnCommandMATCH(DictTMatchEvent value);
	void __fastcall SetOnCommandOPTION(DictTOptionEvent value);
	void __fastcall SetOnCommandQUIT(DictTBasicEvent value);
	void __fastcall SetOnCommandSASLAUTH(DictTAuthEvent value);
	void __fastcall SetOnCommandSHOW(DictTShowEvent value);
	void __fastcall SetOnCommandSTAT(DictTBasicEvent value);
	
public:
	__fastcall virtual TDXDictionaryServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXDictionaryServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, DictTBasicEvent EventProc);
	void __fastcall AddDefineEvent(AnsiString Command, DictTDefineEvent EventProc);
	void __fastcall AddMatchEvent(AnsiString Command, DictTMatchEvent EventProc);
	void __fastcall AddShowEvent(AnsiString Command, DictTShowEvent EventProc);
	void __fastcall AddAuthEvent(AnsiString Command, DictTAuthEvent EventProc);
	void __fastcall AddOptionEvent(AnsiString Command, DictTOptionEvent EventProc);
	
__published:
	__property DictTAuthEvent OnCommandAUTH = {read=fOnCommandAUTH, write=SetOnCommandAUTH};
	__property DictTShowEvent OnCommandCLIENT = {read=fOnCommandCLIENT, write=SetOnCommandCLIENT};
	__property DictTDefineEvent OnCommandDEFINE = {read=fOnCommandDEFINE, write=SetOnCommandDEFINE};
	__property DictTBasicEvent OnCommandHELP = {read=fOnCommandHELP, write=SetOnCommandHELP};
	__property DictTMatchEvent OnCommandMATCH = {read=fOnCommandMATCH, write=SetOnCommandMATCH};
	__property DictTOptionEvent OnCommandOPTION = {read=fOnCommandOPTION, write=SetOnCommandOPTION};
	__property DictTBasicEvent OnCommandQUIT = {read=fOnCommandQUIT, write=SetOnCommandQUIT};
	__property DictTAuthEvent OnCommandSASLAUTH = {read=fOnCommandSASLAUTH, write=SetOnCommandSASLAUTH}
		;
	__property DictTShowEvent OnCommandSHOW = {read=fOnCommandSHOW, write=SetOnCommandSHOW};
	__property DictTBasicEvent OnCommandSTAT = {read=fOnCommandSTAT, write=SetOnCommandSTAT};
	__property DictTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxdictionaryservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxdictionaryservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXDictionaryServerCore
